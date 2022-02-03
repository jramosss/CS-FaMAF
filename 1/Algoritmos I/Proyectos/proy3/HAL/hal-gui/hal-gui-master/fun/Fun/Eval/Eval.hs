
----------------------------------------------------------------------------
-- |
-- Module      :  $Header$
-- Copyright   :  (c) Proyecto Theona, 2012-2013
--                (c) Alejandro Gadea, Emmanuel Gunther, Miguel Pagano
-- License     :  <license>
-- 
-- Maintainer  :  miguel.pagano+theona@gmail.com
-- Stability   :  experimental
-- Portability :  portable
--
-- Evaluador para expresiones que son programas.
-- 
----------------------------------------------------------------------------

{-# LANGUAGE OverloadedStrings #-}
module Fun.Eval.Eval ( eval
                     , evalTrace
                     , evalStep
                     , evalStepTrace
                     , createEvalEnv
                     , EvalEnv) where

import Equ.PreExpr
import Equ.IndTypes(getIndType)
import Equ.IndType (IndType,constants,isConstructor)
import Equ.Matching(match)

import Fun.TypeChecker

import Prelude hiding(sum)

import Control.Arrow
import Control.Monad ((>=>))
import Control.Monad.Trans.Class(lift,MonadTrans)
import Control.Monad.Trans.State

import qualified Data.Map as M
import qualified Data.Set as S

import Fun.Decl
import Fun.Eval.Rules

-- | El environment tendrá las variables que son simbolo de función
--   con sus definiciones.
type EvalEnv = M.Map Variable ([Variable],PreExpr)

type EvalMonad a = StateT EvalEnv Maybe a



-- | Las expresiones constantes, aplicacion de operadores e if, tienen
--   reglas mediante las cuales se evaluan.  La aplicación de
--   variables, la expresión por casos y las variables definidas
--   tienen reglas únicas, por lo cual son consideradas
--   especialmente. Definimos un tipo de dato para referirnos a cada
--   una de esas reglas especiales.
data SpecialRule a = SpecialRule {
                        appRule :: a
                      , caseRule :: a
                      , varRule :: a
                    }

    
-- | Esta sería la función principal de evaluación
--   Toma una expresión cualquiera y devuelve una expresión canónica.
--   Se asume que las expresiones ESTAN BIEN TIPADAS
eval :: PreExpr -> EvalEnv -> PreExpr
eval e env = maybe e (uncurry eval) $ runStateT (evalStep e) env
                
evalTrace :: PreExpr -> EvalEnv -> (PreExpr,[(String,PreExpr)])
evalTrace = evalTrace' []
                
evalTrace' :: [(String,PreExpr)] -> PreExpr -> EvalEnv -> (PreExpr,[(String,PreExpr)])
evalTrace' steps e env =
    maybe (e,steps)
          (\((e',rulename),env') -> evalTrace' (steps++[(rulename,e')]) e' env')
          (runStateT (evalStepTrace e) env)

isCan :: PreExpr -> EvalMonad Bool
isCan e = do env <- get
             let tyenv = mkTypeEnv env
             let ty = tcExprEnv tyenv e
             maybe (return False) (isCanonical e) $ getIndType ty 
      where mkTypeEnv = M.foldrWithKey (\v _ -> M.insert (varName v) (varTy v)) M.empty 

isCanonical :: PreExpr -> IndType -> EvalMonad Bool
isCanonical (Con c) t = return (c `elem` constants t)
isCanonical (UnOp op e') t = isCan e' >>= return . (isConstructor t op &&)
isCanonical (BinOp op e e') t = isCan e >>= \b ->
                                isCan e' >>= \b' ->
                                return (isConstructor t op && b && b')
-- Una variable es canónica si es un símbolo de función (es una
-- expresión lambda)
isCanonical (Var _) _ = return False
isCanonical _ _ = return False


vardef :: Variable -> EvalMonad Bool
vardef v = get >>= return . M.member v


evalStep :: PreExpr -> EvalMonad PreExpr
evalStep = evalStep' (\e -> liftMaybe () . matchRules e) (SpecialRule () () ())
           >=> return . fst
           
evalStepTrace :: PreExpr -> EvalMonad (PreExpr,String)
evalStepTrace = evalStep' matchRulesTrace (SpecialRule "E-APP" "E-CASE" "E-VAR") 
           
           
-- | Un paso de evaluación.
--   Si la expresión que se quiere evaluar no tiene las subexpresiones
--   canónicas, entonces el paso se aplicará a la subexpresión (en el marco de la teoria
--   presentada en la tesis) corresponde a aplicar el paso de evaluacion "E-CONTEXT"
evalStep' :: (PreExpr -> [EvalRule] -> Maybe (PreExpr,a)) -> 
             SpecialRule a ->
             PreExpr -> EvalMonad (PreExpr,a)
evalStep' mrules sr e@(UnOp op e') = 
    whenMT (isCan e')
           (lift (mrules e unOpRules))
           (evalStep' mrules sr e' >>=
                return . first (UnOp op))
evalStep' mrules sr e@(BinOp op e1 e2) = 
    whenMT (isCan e1)
           (whenMT (isCan e2) 
                    (lift (mrules e binOpRules))
                    (evalStep' mrules sr e2 >>=
                         return . first (BinOp op e1)))
            (evalStep' mrules sr e1 >>= 
               return . first (flip (BinOp op) e2))
evalStep' mrules sr e@(If b e1 e2) =
    whenMT (isCan b)
           (lift (mrules e ifRules))
           (evalStep' mrules sr b >>=
                \(bcan,a) -> return (If bcan e1 e2,a))
                
{- | En la tesis, tenemos expresiones lambda para expresar la evaluación de funciones.
     Aqui una variable puede ser aplicada si está en el environment de declaraciones de funciones.
     Si la función es de un solo argumento, la aplicación es directa (se reemplaza la variable por el valor
     en la expresión que define a la función).
     Si la función es de más de un argumento, se crea una nueva función, con un nombre fresco, que toma un argumento
     menos. Por ejemplo:
       g x y = x + y
       
     Queremos evaluar g@0@0. (esto se parsea: (g@0)@0 )
     
     Lo que hacemos entonces es crear una nueva función:
       g' y = 0 + y
       
     y la expresión que queríamos evaluar, se evalúa a:
       g'@0
       
     que al tener ya un solo parámetro, se evalúa trivialmente.
-}
evalStep' f sr (App v@(Var x) e2) =
    whenMT (vardef x)
           (whenMT (isCan e2)
                   (applyFun x e2 >>= \e' -> return (e',appRule sr))
                   (case e2 of
                         Var y -> whenMT (vardef y)
                                         (applyFun x e2 >>= \e' -> return (e',appRule sr))
                                         (lift Nothing)
                         _ -> (evalStep' f sr e2 >>=
                                    return . first (App v))))
           -- Si x no esta declarada como funcion, no se podrá evaluar.
           -- evalStep' v dará Nothing
           (evalStep' f sr v)
           
evalStep' f sr (App e1 e2) = 
     whenMT (isCan e1)
           -- Si e1 es canónica pero no es una variable, no se puede aplicar.
           (lift Nothing)
           (evalStep' f sr e1 >>= return . first (flip App e2))
evalStep' _ sr (Case e' ps) =
    matchPatterns e' ps >>= \(ei,subst) -> 
    return (applySubst ei subst,caseRule sr)
    

evalStep' f sr (Paren e) = evalStep' f sr e
evalStep' _ sr (Var x) = 
    whenMT (vardef x)
           (get >>= lift . M.lookup x >>= \(vars,edef) ->
            if null vars
            then return (edef,varRule sr)
            else lift Nothing)
           (lift Nothing)
evalStep' _ _ _ = lift Nothing

-- | matchPatterns busca por un patron en la lista que matchee con la
--  expresión e.  Si lo encuentra retorna la expresión correspondiente
--  al patrón y la substitución del matching.  Si no lo encuentra
--  Nothing
matchPatterns :: PreExpr -> [(PreExpr,PreExpr)] -> EvalMonad (PreExpr,ExprSubst)
matchPatterns _ [] = lift Nothing
matchPatterns e ((p1,e1):ps) = either (const $ matchPatterns e ps)
                                      (\(subst,_) -> return (e1,subst))
                                      (match p1 e)



applyFun :: Variable -> PreExpr -> EvalMonad PreExpr
applyFun v e =
    get >>= \env ->
    lift (M.lookup v env) >>= \(vars,edef) ->
    case vars of
         -- Si una función 0-aria es aplicada, hay un error de tipo
         [] -> lift Nothing
         [x] -> return (applySubst edef (subst x))
         (y:vs) -> return (freshVar $ S.fromList $ M.keys env) >>= \vnew ->
                   -- creamos la nueva funcion, la cual tiene un parámetro menos
                   -- y su expresión de definición es la misma que v, pero reemplazando
                   -- la variable 'y' por la expresión e
                   modify (M.insert vnew (vs,(applySubst edef $ subst y))) >>
                   return (Var vnew)
         
    where subst x = M.singleton x e


-- | Crea un EvalEnv a partir de una lista de declaraciones de funciones
createEvalEnv :: [FunDecl] -> [ValDecl] -> EvalEnv
createEvalEnv fs vals = addFunDecls fs `M.union` addValDecls vals

    where addFunDecls = foldl (\m (Fun v vs e _) -> M.insert v (vs,e) m) M.empty
          addValDecls = foldl (\m (Val v e) -> M.insert v ([],e) m) M.empty

          
whenMT :: (MonadTrans t, Monad (t m)) => t m Bool -> t m a -> t m a -> t m a
whenMT mb acTrue acFalse =
    mb >>= \b ->
    if b
       then acTrue
       else acFalse

liftMaybe :: b -> Maybe a -> Maybe (a,b)
liftMaybe _ Nothing = Nothing
liftMaybe b (Just a) = Just (a,b)
