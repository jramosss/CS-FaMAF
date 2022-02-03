{-# Language DoAndIfThenElse,OverloadedStrings #-}
{-| Algoritmo de chequeo e inferencia de tipos para pre-expre-
siones. Este algoritmo es esencialmente el de Hindley-Milner-Damas
para el cálculo lambda: si tenemos informacion en la pre-expresion
entonces se verifica que sea unificable con el tipo de inferido. A
diferencia de esos algoritmos, no se hay un contexto donde se declara
el tipo de las variabes, ya que la informacion de las variables
(símbolos de función y constantes son tratadas exactamente de la misma
manera) está contenida en la expresión misma (en este aspecto se
parece más al chequeo de un cálculo à la Church).
-}
module Equ.TypeChecker 
    ( module Equ.TypeChecker.Error      
      -- * Algoritmo de unificaci&#243;n con relaci&#243;n de orden.
    , unify
    , TySubst
    , emptySubst
    , unifyTest
    , unificate
    , rewrite
    , typeCheckPreExpr 
    , typeCheckPre
    , match
    , match2
      -- * Algoritmo de TypeChecking.
    , checkPreExpr
    , getType
    , TCState(..)
    , checkPreExpr'
    , checkPreExpr''
    , initCtx
    , TCCtx(..)
    )
    where

import Equ.Syntax
import Equ.PreExpr
import Equ.Types
import Equ.Theories.AbsName
import Equ.TypeChecker.Error
import Equ.TypeChecker.Unification

import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.Sequence as S

import qualified Data.Set as Set (elems)
import Control.Monad.Trans.Either (runEitherT, hoistEither)
import Control.Monad.RWS.Class (ask, tell, gets, modify)
import Control.Monad.RWS (runRWS)

import Control.Applicative((<$>))

import Control.Lens hiding (cons,rewrite,op)

-- | Ciertos s&#237;mbolos deben tener un &#250;nico tipo en toda una expresi&#243;n;
-- un contexto lleva la cuenta de todos los tipos que vamos viendo. En
-- principio s&#243;lo deber&#237;a un tipo a lo sumo.
type CtxSyn s = M.Map s [Type]
    
-- | El contexto global es un conjunto con los contextos de cada tipo
-- de s&#237;mbolo; el contexto para los cuantificadores es fijo,
-- inicialmente tiene los cuantificadores "homog&#233;neos" (por ejemplo,
-- sumatoria est&#225;, pero forall no est&#225;).
data TCCtx = TCCtx { vars :: CtxSyn VarName
                   , ops  :: CtxSyn OpName 
                   , cons :: CtxSyn ConName
                   , quants :: CtxSyn QuantName
                   }
         deriving Show


varCtx :: Lens' TCCtx (CtxSyn VarName)
varCtx = lens vars (\c v -> c { vars = v})

opCtx :: Lens' TCCtx (CtxSyn OpName)
opCtx = lens ops (\c o -> c {ops = o})

conCtx :: Lens' TCCtx (CtxSyn ConName)
conCtx = lens cons (\c c' -> c {cons = c'})

quantCtx :: Lens' TCCtx (CtxSyn QuantName)
quantCtx = lens quants (\c q -> c {quants = q})

-- | El error est&#225; acompa&#241;ado de la expresi&#243;n enfocada donde ocurri&#243;.
type TMErr = (Focus,TyErr)

-- TODO: cambiar: el estado tendr&#237;a el contexto adem&#225;s de la
-- sustituci&#243;n.
-- | La m&#243;nada de estado del type-checker.
type TyState = MonadTraversal TMErr TCState

data TCState = TCState { subst :: TySubst
                       , ctx   :: TCCtx
                       , fTyVar :: Int
                       }
             deriving Show

-- | Agrega una l&#237;nea de log.
addLog :: String -> TyState ()
addLog = tell . S.fromList . return . T.pack 

-- | Generaci&#243;n de mensaje de Error.
tyerr :: TyErr -> TyState a
tyerr err = ask >>= \foc -> hoistEither $ Left (foc, err)

getSub :: TyState TySubst
getSub = gets subst


getFreshTyVar :: TyState Type
getFreshTyVar = gets fTyVar >>= \n -> 
                modify (\st -> st { fTyVar = n+1}) >>
                return (TyVar $ T.pack (show n))

modCtx :: (TCCtx -> TCCtx) -> TyState ()
modCtx f = modify (\st -> st { ctx = f (ctx st)})

modSubst :: (TySubst -> TySubst) -> TyState ()
modSubst f = modify (\st -> st { subst = f (subst st)}) >> getSub >>= updateCtxS

{- 

   Las PreExpresiones y las Expresiones son b&#225;sicamente &#225;rboles de
   sintaxis abstracta con distinta informaci&#243;n en cada nodo.  Por
   ejemplo, podr&#237;a ser que las PreExpresiones tengan un componente de
   tipo 'Maybe Type', mientras que el mismo componente en una Expresi&#243;n
   ser&#225; de tipo 'Type'. Esto nos permite ver las PreExpresiones c&#243;mo
   Expresiones parcialmente tipadas.

   Una cosa que s&#237; necesitamos es informaci&#243;n sobre por qu&#233; fall&#243; un
   chequeo/inferencia de tipos. 

   El type-checker usar&#225; en lo posible la informaci&#243;n de tipos de las
   PreExpresiones; de esta manera podremos tener un chequeo incremental.

-}

extCtx :: (Syntactic s,Ord k) => (s -> k) -> s -> [Type] -> CtxSyn k -> CtxSyn k
extCtx f s = M.insertWith (flip const) (f s)

extCtxV :: Variable -> Type -> TyState ()
extCtxV v t = modCtx (over varCtx (extCtx varName v [t]))

extCtxVar :: VarName -> Type -> TyState ()
extCtxVar v t = modCtx (over varCtx (M.insertWith (flip const) v [t]))

extCtxOp :: Operator -> Type -> TyState ()
extCtxOp o t = modCtx (over opCtx (extCtx opName o [t]))

extCtxCon :: Constant -> Type -> TyState ()
extCtxCon c t = modCtx (over conCtx (extCtx conName c [t]))

extCtxQuan :: Quantifier -> Type -> TyState ()
extCtxQuan q t = modCtx (over quantCtx (extCtx quantName q [t]))


-- | Chequeo de diferentes elementos sint&#225;cticos simples como
-- variables, constantes, s&#237;mbolos de funci&#243;n y operadores.
checkSyn :: (Syntactic s,Ord k) => s -> (s -> k) -> (TCCtx -> M.Map k [Type]) -> TyState Type
checkSyn s name getM = gets (getM . ctx) >>= \ct ->
                       case M.lookup (name s) ct of
                         Nothing -> tyerr $ ErrClashTypes s []
                         Just ts -> rewriteS (head ts)

-- | Las diferentes instancias de checkSyn.
checkVar :: Syntactic s => s -> TyState Type
checkVar v = checkSyn v tRepr vars
checkCon :: Constant -> TyState Type
checkCon c = checkSyn c conName  cons
checkOp :: Operator -> TyState Type
checkOp op = checkSyn op opName ops
checkQuant :: Quantifier -> TyState Type
checkQuant q = checkSyn q quantName quants

-- | Actualiza los tipos en el contexto.
updateCtx :: TCCtx -> TySubst -> TCCtx
updateCtx ct subs = over varCtx apSub $
                    over opCtx apSub  $
                    over conCtx apSub ct
          where apSub = M.map $ map (rewrite subs)

-- | Checkea una sub-expresi&#243;n y actualiza el contexto.
checkAndUpdate :: PreExpr -> (Focus -> Maybe Focus) -> TyState Type
checkAndUpdate e go = localGo go (check e)

updateCtxS :: TySubst -> TyState ()
updateCtxS = modCtx . flip updateCtx

-- Lifting de unificación para la mónada de TC
unifyS :: Type -> Type -> TyState TySubst
unifyS t t' = getSub >>= \s ->
              case unify t t' s of
                Left err -> addLog ("Error trying to unify: "
                                   ++ show t ++ " and " ++ show t'
                                  )
                           >> tyerr err
                Right s' -> modSubst (const s') >> return s'                

unifyListS :: [Type] -> TyState TySubst
unifyListS [] = getSub
unifyListS [_] = getSub
unifyListS (t:t':ts) = unifyS t t' >> unifyListS (t':ts)

rewriteS :: Type -> TyState Type
rewriteS t = flip rewrite t <$> getSub

-- TODO: 
--  * extraer la expresi&#243;n del focus que tenemos en el ambiente?
--  * pensar el caso de cuantificadores; 
--  * definir propiedades.
check :: PreExpr -> TyState Type
check (Var v) = checkVar v
check (Con c) = checkCon c 
check (PrExHole h) = return (tType h)
check (Paren e) = localGo goDown $ check e
check (UnOp op e) = do t <- checkAndUpdate e goDown
                       t' <- checkOp op
                       w <- getFreshTyVar 
                       _ <- unifyS t' (t :-> w) 
                       rewriteS w
check (BinOp op e e') = do te <- checkAndUpdate e goDown
                           te' <- checkAndUpdate e' goDownR
                           tOp <- checkOp op
                           w <- getFreshTyVar
                           _ <- unifyS (te :-> te' :-> w) tOp
                           rewriteS w
check (App e e') = do te <- checkAndUpdate e goDown
                      te' <- checkAndUpdate e' goDownR
                      w <- getFreshTyVar
                      _ <- unifyS  te (te' :-> w) 
                      rewriteS w
check (Quant q v r t) = do tyQ <- checkQuant q
                           () <- addLog $ show tyQ
                           tyV <- getFreshTyVar
                           extCtxV v tyV
                           tyR <- checkAndUpdate r goDown
                           tyT <- checkAndUpdate t goDownR
                           case tyQ of 
                             t1 :-> t2 -> do
                                 _ <- unifyS tyV t1
                                 _ <- unifyS t2 tyT
                                 _ <- unifyS tyR tyBool
                                 rewriteS tyT
                             t1 -> tyerr $ ErrNotExpected (tyV :-> tyT) t1
check (If b t f) = do tb <- checkAndUpdate b goDown
                      _ <- unifyS tb  (TyAtom ATyBool)
                      tt <- checkAndUpdate t goIfTrue
                      tf <- checkAndUpdate f goIfFalse
                      _ <- addLog (show tt ++ show tf)
                      _ <- unifyS tt tf
                      rewriteS tt
                                     
check (Case e cs) = do texp <- checkAndUpdate e goDown
                       pats <- mapM checkCase cs
                       _ <- unifyListS (texp:map fst pats)
                       _ <- unifyListS (map snd pats) 
                       rewriteS (snd (head pats))


-- | Devuelve el tipo de un patrón y de la expresión.
checkCase :: (PreExpr,PreExpr) -> TyState ((Type,Type))
checkCase (pat,e) = do tpat <- checkPat pat
                       texp <- check e
                       return (tpat,texp)


checkPat :: PreExpr -> TyState Type
checkPat (Var v) = getFreshTyVar >>= \w -> extCtxV v w >> return w
checkPat (Con c) = checkCon c
checkPat (UnOp op e) = checkOp op >>= \t ->
                       checkPat e >>= \t'->
                       getFreshTyVar >>= \w ->
                       unifyS t (t' :-> w) >>
                       rewriteS w
checkPat (BinOp op e e') = checkOp op >>= \t ->
                           checkPat e >>= \t' ->
                           checkPat e' >>= \t'' ->
                           getFreshTyVar >>= \w ->
                           unifyS t (t' :-> t'' :-> w) >>
                           rewriteS w
checkPat (Paren p) = checkPat p
checkPat _ = error "Expression is not a pattern."

initCtx :: TCCtx
initCtx = TCCtx { vars = M.empty
              , ops  = M.empty
              , cons = M.empty
              , quants = M.empty
              }

check' :: PreExpr -> TyState Type
check' e = initCtxE e >> check e

initTCState :: TCState
initTCState = TCState { subst = emptySubst
                      , ctx = initCtx
                      , fTyVar = 0
                      }

-- | Construye un contexto con variables frescas para las variables
-- que no tienen un tipo 
mkCtxVar :: PreExpr -> TyState ()
mkCtxVar e = mapM_ updCtx vs
    where vs = Set.elems $ freeVars e
          updCtx v = renTy M.empty (varTy v) >>= extCtxV v . fst

mkCtxOps :: PreExpr -> TyState ()
mkCtxOps = mapM_ updCtx . getOperators
    where updCtx op = renTy M.empty (opTy op) >>= extCtxOp op . fst

mkCtxCon :: PreExpr -> TyState ()
mkCtxCon = mapM_ updCtx . getConstants
    where updCtx con = renTy M.empty (conTy con) >>= extCtxCon con . fst

mkCtxQuan :: PreExpr -> TyState ()
mkCtxQuan = mapM_ updCtx . getQuants
    where updCtx quan = renTy M.empty (quantTy quan) >>= extCtxQuan quan . fst

-- | Dado un tipo, reemplaza todas las variables libres del
-- tipo por variables frescas.
renTy :: TySubst -> Type -> TyState (Type,TySubst)
renTy s TyUnknown = getFreshTyVar >>= \t -> return (t,s)
renTy s t@(TyAtom _) = return (t,s)
renTy s (TyVar v) = maybe newVar (\t -> return (t,s)) $ M.lookup v s
    where newVar = getFreshTyVar >>= \w ->
                   return (w, M.insert v w s)
renTy s (TyList t) = renTy s t >>= \(t',s') -> return (TyList t',s')
renTy s (t :-> t') = renTy s t >>= \(t1,s') -> 
                     renTy s' t' >>= \(t2,s'') -> return (t1 :-> t2,s'')

initCtxE :: PreExpr -> TyState ()
initCtxE e = mkCtxVar e >> mkCtxOps e >> mkCtxCon e >> mkCtxQuan e

mkSubst :: TCCtx -> ((Variable -> Type), (Constant -> Type), (Operator -> Type))
mkSubst (TCCtx vs os cs _) = (updVar,updCons,updOps)
    where updVar = M.foldrWithKey (go varName) tyUnk vs
          updCons = M.foldrWithKey (go conName) tyUnk cs
          updOps =  M.foldrWithKey (go opName) tyUnk os
          tyUnk _ = TyUnknown          
          go acc k ty f v = if acc v == k then head ty else f v

setType' :: TCCtx -> PreExpr -> PreExpr
setType' ct e = setType v c o e
         where (v,c,o) = mkSubst ct


-- | Retorna el tipo de una expresi&#243;n bien tipada.
checkPreExpr :: PreExpr -> Either (TMErr,Log) Type
checkPreExpr e = case runRWS (runEitherT (check' e)) (toFocus e) initTCState of
                   (res, _, l) -> either (\err -> Left (err,l)) Right res

checkPreExpr' :: M.Map VarName Type -> PreExpr -> Either TMErr (Type, TCState)
checkPreExpr' env e = case runRWS (runEitherT checkWithEnv) (toFocus e) initTCState of
                   (res, st, _) -> either Left (\r -> Right (r,st)) res
    where checkWithEnv = initCtxE e >> mapM_ (uncurry extCtxVar) (M.toList env) >> check e

checkPreExpr'' :: Variable -> PreExpr -> Either TMErr (Type, TCState)
checkPreExpr'' fun e = case runRWS (runEitherT checkWithEnv) (toFocus e) initTCState of
                         (res, st, _) -> either Left (\r -> Right (r,st)) res
    where checkWithEnv = initCtxE e >> getFreshTyVar >>= \t -> 
                         getFreshTyVar >>= \t' ->
                         extCtxV fun (t :-> t') >>
                         check e

typeCheckPreExpr :: PreExpr -> Either (TMErr,Log) PreExpr
typeCheckPreExpr e = case runRWS (runEitherT (check' e)) (toFocus e) initTCState of
                   (res, st,l) -> either (\err -> Left (err,l)) (const $ Right $ setType' (ctx st) e) res

typeCheckPre :: PreExpr -> Log
typeCheckPre e = runRWS (runEitherT (check' e)) (toFocus e) initTCState ^. _3


getType :: PreExpr -> Maybe Type
getType = either (const Nothing) return . checkPreExpr

