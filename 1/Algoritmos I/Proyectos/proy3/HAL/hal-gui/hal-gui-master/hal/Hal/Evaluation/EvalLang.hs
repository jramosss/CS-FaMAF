{- | Evaluador del lenguaje Hal
    
    Para evaluar asumimos el programa typechekeo sin problemas.
-}
{-# LANGUAGE RecordWildCards, FlexibleInstances, MultiParamTypeClasses #-}
module Hal.Evaluation.EvalLang where

import Control.Applicative
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.State (StateT,get,put,execStateT)
import Control.Monad.Fix (fix)
import System.IO

import Data.IORef
import Data.Maybe
import Data.Either
import Data.Reference
import qualified Data.List as L

-- Imports de Hal
import Hal.Lang
import Hal.Parser



-- | Elemento de un estado. Representa el valor de una variable en un momento
-- de la evaluación.
data StateTuple = IntVar  Identifier (Maybe Int)
                | BoolVar Identifier (Maybe Bool)

instance Show StateTuple where
    show (IntVar  i mi) = show i ++ ":" ++ prettyMaybe mi
    show (BoolVar i mb) = show i ++ ":" ++ prettyMaybe mb

takeIdentifier :: StateTuple -> Identifier
takeIdentifier (IntVar i _) = i
takeIdentifier (BoolVar i _) = i
    
prettyMaybe :: Show a => Maybe a -> String
prettyMaybe Nothing  = "Sin valor."
prettyMaybe (Just v) = show v
    
fromRight :: Either a b -> b
fromRight (Right b) = b

fromLeft :: Either a b -> a
fromLeft (Left a) = a
    
instance Eq StateTuple where
    (IntVar i _) == (IntVar i' _) = i == i'
    (BoolVar i _) == (BoolVar i' _) = i == i'
    _ == _ = False

type EitherBI = Either Bool Int

data ExpectValue = ExpectBool | ExpectInt
    
-- | Estado de la evaluación.
data State = State { vars        :: [StateTuple] 
                   , newValueFun :: Identifier -> ExpectValue -> ProgState EitherBI
                   }
                   
instance Show State where
    show (State vars _ ) = show vars

instance Reference IORef (StateT State IO) where
    newRef = liftIO . newRef
    readRef = liftIO . readRef
    writeRef r = liftIO . writeRef r

initState :: State
initState = makeState [] getNewValue

makeState :: [StateTuple] -> (Identifier -> ExpectValue -> ProgState EitherBI) -> State
makeState = State 

takeIdentifiers :: State -> [Identifier]
takeIdentifiers = map takeIdentifier . vars

fillState :: State -> [Identifier] -> State
fillState st vars = st {vars = map makeVar vars}

makeVar :: Identifier -> StateTuple
makeVar i@(Identifier {..}) = case idDataType of
                                IntTy  -> IntVar  i Nothing
                                BoolTy -> BoolVar i Nothing

getNewValue :: Identifier -> ExpectValue -> ProgState EitherBI
getNewValue i ev = 
            do
            st <- get
            
            liftIO $ putStr ("Ingrese la variable "++show (idName i) ++": ")
            liftIO $ hFlush stdout
            
            str <- liftIO getLine
            
            case ev of
                ExpectBool -> case parseBConFromString str of
                                   Left er -> liftIO (putStr "Valor no valido, intente de nuevo.\n") >> 
                                              getNewValue i ev
                                   Right v -> do st <- get
                                                 let idSts = vars st
                                                 v' <- evalBExp v
                                                 let idSts' = L.map (updateValue i (Left v')) idSts
                                                 put (st { vars =  idSts'})
                                                 return $ Left v'
                ExpectInt  -> case parseConFromString str of
                                   Left er -> liftIO (putStr "Valor no valido, intente de nuevo.\n") >> 
                                              getNewValue i ev
                                   Right v -> do st <- get
                                                 let idSts = vars st
                                                 v' <- evalExp v
                                                 let idSts' = L.map (updateValue i (Right v')) idSts
                                                 put (st { vars =  idSts'})
                                                 return $ Right v'
-- | Mónada de la semántica denotacional.
type ProgState = StateT State IO

-- | Evaluador de los operadores binarios enteros.
evalIntBOp :: IntBOp -> ProgState Int -> ProgState Int -> ProgState Int
evalIntBOp Plus   = liftA2 (+)
evalIntBOp Times  = liftA2 (*)
evalIntBOp Substr = liftA2 (-)
evalIntBOp Div    = liftA2 div
evalIntBOp Mod    = liftA2 mod

-- | Evaluador de los operadores binarios boleanos.
evalBoolBOp :: BoolBOp -> ProgState Bool -> ProgState Bool -> ProgState Bool
evalBoolBOp And = liftA2 (&&)
evalBoolBOp Or  = liftA2 (||)

-- | Evaluador de los operadores unarios boleanos.
evalBoolUOp :: BoolUOp -> ProgState Bool -> ProgState Bool
evalBoolUOp Not = fmap not

-- | Evaluador de las relaciones binarias.
evalRelOp :: (Eq a, Ord a) => 
             RelOp -> ProgState a -> ProgState a -> ProgState Bool
evalRelOp Equal  = liftA2 (==)
evalRelOp Lt     = liftA2 (<)

-- | Evaluador de expresiones enteras.
evalExp :: Exp -> ProgState Int
evalExp (IBOp iop e e') = evalIntBOp iop (evalExp e) (evalExp e')
evalExp (ICon i)  = return i
evalExp ide@(IntId i) = do 
                    st <- get
                    let idSts = vars st
                    maybe (error "Imposible, siempre encontramos una variable.")
                          stGetIntValue $ L.find (==(IntVar i Nothing)) idSts
    where
        stGetIntValue :: StateTuple -> ProgState Int
        stGetIntValue (IntVar _ mv) = maybe getValue return mv
        getValue :: ProgState Int
        getValue = do 
                   st <- get
                   let f = newValueFun st
                   Right v <- f i ExpectInt
                   return v

-- | Evaluador de expresiones boleanas.
evalBExp :: BExp -> ProgState Bool
evalBExp (BRel rop e e') = evalRelOp rop (evalExp e) (evalExp e')
evalBExp (BUOp bop e)    = evalBoolUOp bop $ evalBExp e
evalBExp (BBOp bop e e') = evalBoolBOp bop (evalBExp e) (evalBExp e')
evalBExp (BCon b) = return b
evalBExp ide@(BoolId i) = do 
                    st <- get
                    let idSts = vars st
                    maybe (error "Imposible, siempre encontramos una variable.")
                           stGetBoolValue $ L.find (==(BoolVar i Nothing)) idSts
    where
        stGetBoolValue :: StateTuple -> ProgState Bool
        stGetBoolValue (BoolVar _ mv) = maybe getValue return mv
        getValue :: ProgState Bool
        getValue = do 
                   st <- get
                   let f = newValueFun st
                   Left v <- f i ExpectBool
                   return v

-- | Actualiza el valor de un identificador en una tupla del estado.
updateValue :: Identifier -> Either Bool Int -> StateTuple -> StateTuple
updateValue i (Left v) stt@(BoolVar i' _) = if i == i' 
                                                then (BoolVar i $ Just v)
                                                else stt
updateValue i (Right v) stt@(IntVar i' _) = if i == i' 
                                                then (IntVar i $ Just v)
                                                else stt
updateValue _ _ stt = stt

-- | Agrega un identificador con su valor al estado.
addValue :: Identifier -> Either Bool Int -> State -> State
addValue i (Left v) st  = st {vars = (vars st)++[BoolVar i $ Just v]}
addValue i (Right v) st = st {vars = (vars st)++[IntVar i $ Just v]}

-- | Evaluador de los comandos.
evalComm :: Comm -> ProgState ()
evalComm Skip = return ()
evalComm Abort = error "¿Este no es el comportamiento que debería tener? :D"
evalComm (Assert b) = return ()
evalComm (If b c c') = evalBExp b >>= \vb ->
                       if vb then evalComm c else evalComm c'
evalComm (IAssig a e) = do 
                        evalE <- evalExp e
                        st <- get 
                        let idSts = vars st
                        let idSts' = L.map (updateValue a (Right evalE)) idSts
                        put (st { vars =  idSts'})
evalComm (BAssig a e) = do 
                        evalE <- evalBExp e
                        st <- get 
                        let idSts = vars st
                        let idSts' = L.map (updateValue a (Left evalE)) idSts
                        put (st { vars =  idSts'})
evalComm (Seq c c') = evalComm c >> evalComm c'
evalComm (Do inv b c) = fix evalDo
    where
        evalDo :: ProgState () -> ProgState ()
        evalDo f = do
                   vb <- evalBExp b
                   if vb then (evalComm (Seq c (Assert inv))) >> f 
                         else return ()

-- | Evaluador de los programas.
evalProgram :: Program -> IO State
evalProgram (Prog vars pre comms post) = execStateT (evalComm comms) (fillState initState vars)

evalStepComm :: Comm -> ProgState (Maybe Comm,Maybe Comm)
evalStepComm (Seq c c') = evalStepComm c >>= \mcc' -> 
                          case (mcc') of
                              (Just c,Nothing) -> return (Just c, Just c')
                              (Just c,Just c'') -> return (Just c, Just (Seq c'' c'))
evalStepComm wc@(Do _ b c) = do
                          vb <- evalBExp b
                          if vb 
                             then return (Nothing,Just $ Seq c wc)
                             else return (Just wc,Nothing)
evalStepComm wc@(If b c c') = do
                          vb <- evalBExp b
                          if vb 
                             then return (Nothing,Just c)
                             else return (Nothing,Just c')
evalStepComm c = evalComm c >> return (Just c,Nothing)

