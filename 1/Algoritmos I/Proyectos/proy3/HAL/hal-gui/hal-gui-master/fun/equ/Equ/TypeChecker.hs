----------------------------------------------------------------------------
-- |
-- Module      :  $Header$
-- Copyright   :  (c) Proyecto Theona, 2012-2013
--                (c) Alejandro Gadea, Emmanuel Gunther, Miguel Pagano
-- License     :  GPL-3
--
-- Maintainer  :  miguel.pagano+theona@gmail.com
-- Stability   :  experimental
-- Portability :  portable
--
-- Algoritmo de chequeo e inferencia de tipos para pre-expre-
-- siones. Este algoritmo es esencialmente el de Hindley-Milner-Damas
-- para el cálculo lambda: si tenemos informacion en la pre-expresion
-- entonces se verifica que sea unificable con el tipo de inferido. A
-- diferencia de esos algoritmos, no se hay un contexto donde se
-- declara el tipo de las variabes, ya que la informacion de las
-- variables (símbolos de función y constantes son tratadas
-- exactamente de la misma manera) está contenida en la expresión
-- misma (en este aspecto se parece más al chequeo de un cálculo à la
-- Church).
--
----------------------------------------------------------------------------
{-# Language DoAndIfThenElse,OverloadedStrings #-}
module Equ.TypeChecker 
    ( module Equ.TypeChecker.Error      
      -- * Algoritmo de unificaci&#243;n con relaci&#243;n de orden.
    , TySubst
    , emptySubst
    , unifyTest
    , unificate
    , rewrite
    , typeCheckPreExpr 
    , match
    , match2
    , mgu
      -- * Algoritmo de TypeChecking.
    , checkPreExpr
    , getType
    , TCState(..)
    , checkPreExpr'
    , checkPreExpr''
    , initCtx
    , TCCtx(..)
    , getCtx
    -- * Otras funciones útiles
    , checkWithEnv
    , setTypeS
    , updAss
    )
    where

import           Equ.PreExpr
import           Equ.Syntax
import           Equ.TypeChecker.Error
import           Equ.TypeChecker.State
import           Equ.Types

import qualified Data.Map as M
import           Control.Monad.Trans.State

import           Control.Lens hiding (cons,rewrite,op)

-- | Chequeo de diferentes elementos sint&#225;cticos simples como
-- variables, constantes, s&#237;mbolos de funci&#243;n y operadores.
checkSyn :: (Ord k, Syntactic s) => s -> (s -> k) ->
            ((CtxSyn k -> Const (CtxSyn k) (CtxSyn k))
                -> TCCtx -> Const (CtxSyn k) TCCtx)
              -> TyState Type
checkSyn s name getM = use (ctx . getM) >>= withTypes s . M.lookup (name s)


-- | Las diferentes instancias de checkSyn.
checkVar :: Variable -> TyState Type
checkVar v = use (ctx . vars) >>= withTypes v . M.lookup (varName v)
               
checkCon :: Constant -> TyState Type
checkCon c = checkSyn c conName cons
checkOp :: Operator -> TyState Type
checkOp o = checkSyn o opName ops
checkQuant :: Quantifier -> TyState Type
checkQuant q = checkSyn q quantName quants


-- TODO: 
--  * pensar el caso de cuantificadores; 
--  * definir propiedades.
check :: PreExpr -> TyState Type
check (Var v) = checkVar v
check (Con c) = checkCon c 
check (PrExHole h) = return (tType h)
check (Paren e) = check e
check (UnOp op e) = do t <- check e
                       t' <- checkOp op
                       w <- getFreshTyVar
                       _ <- unifyS t' (t :-> w) 
                       rewriteS w
check (BinOp op e e') = do te <- check e
                           te' <- check e'
                           tOp <- checkOp op
                           w <- getFreshTyVar
                           _ <- unifyS (te :-> te' :-> w) tOp
                           rewriteS w
check (App e e') = do te <- check e
                      te' <- check e'
                      w <- getFreshTyVar
                      _ <- unifyS te (te' :-> w) 
                      rewriteS w
check (Quant q v r t) = do tyQ <- checkQuant q
                           tyV <- getFreshTyVar
                           _ <- extCtxV v tyV
                           tyR <- check r
                           tyT <- check t
                           case tyQ of 
                             t1 :-> t2 -> do
                                 _ <- unifyS tyV t1
                                 _ <- unifyS t2 tyT
                                 _ <- unifyS tyR tyBool
                                 rewriteS tyT
                             t1 -> tyerr $ ErrNotExpected (tyV :-> tyT) t1
check (If b t f) = do tb <- check b
                      _ <- unifyS tb $ TyAtom ATyBool
                      tt <- check t
                      tf <- check f
                      _ <- unifyS tt tf
                      rewriteS tt
                                     
check (Case e cs) = do texp <- check e
                       pats <- mapM checkCase cs
                       _ <- unifyListS (texp:map fst pats)
                       _ <- unifyListS (map snd pats) 
                       rewriteS (snd (head pats))


-- | Devuelve el tipo de un patrón y de la expresión.
checkCase :: (PreExpr,PreExpr) -> TyState (Type,Type)
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

check' :: PreExpr -> TyState Type
check' e = initCtxE e >> check e




-- | Retorna el tipo de una expresi&#243;n bien tipada.
checkPreExpr :: PreExpr -> Either TMErr Type
checkPreExpr e = evalStateT (check' e) initTCState 

checkPreExpr' :: M.Map VarName Type -> PreExpr -> Either TMErr (Type, TCState)
checkPreExpr' env e = runStateT (checkWithEnv env e) initTCState

checkPreExpr'' :: Variable -> PreExpr -> Either TMErr (Type, TCState)
checkPreExpr'' fun e = runStateT checkFun initTCState
    where checkFun = initCtxE e >> 
                     getFreshTyVar >>= \t -> 
                     getFreshTyVar >>= \t' ->
                     extCtxV fun (t :-> t') >>
                     check e


typeCheckPreExpr :: PreExpr -> Either TMErr PreExpr
typeCheckPreExpr e = either Left (Right . setType' e . _ctx . snd) $ 
                         runStateT (check' e) initTCState

getType :: PreExpr -> Maybe Type
getType = either (const Nothing) return . checkPreExpr

checkWithEnv :: Env -> PreExpr -> TyState Type
checkWithEnv env e = initCtxE e >> mapM_ (uncurry extCtxVar) (M.toList env) >> check e

