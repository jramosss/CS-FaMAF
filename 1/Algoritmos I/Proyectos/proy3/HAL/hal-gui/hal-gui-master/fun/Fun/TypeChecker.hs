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
-- Chequeo de tipos para un módulo. Inspirado en ``Typing Haskell in Haskell''.
-- TODO: Agregar información sobre el lugar de los errores.
-- 
----------------------------------------------------------------------------
{-# Language TemplateHaskell,DoAndIfThenElse #-}
module Fun.TypeChecker ( tcExprEnv
                       , typeCheckDecls
                       ) where

import Fun.Module
import Fun.Declarations hiding (checkThm)
import Fun.Decl

import Equ.PreExpr
import Equ.Proof.Proof(Proof,Theorem(..),updThmExp,updThmPrf)
import Equ.Syntax(VarName)
import Equ.Types
import Equ.Expr (getPreExpr)
import Equ.TypeChecker
import Equ.TypeChecker.State
import Equ.TypeChecker.Proof


import Data.Map (fromList,delete)
import qualified Data.Map as M
import Control.Monad.Trans.State
import Control.Monad (replicateM)
import Control.Monad (when)
import Control.Lens hiding (rewrite)
import Control.Arrow (second)

-- | Chequeo de la declaración de una especificación
checkSpecDecl :: SpecDecl -> TyState (VarName,PreExpr)
checkSpecDecl (Spec fun args body) = checkTypedDecl fun args body

-- | Chequeo de la declaración de una función
checkFunDecl :: FunDecl -> TyState (VarName,PreExpr)
checkFunDecl (Fun fun args body _) = checkTypedDecl fun args body

-- | Chequeo de la declaración de un valor
checkVal :: Annot ValDecl -> TyState (Annot ValDecl)
checkVal (pos,(Val v expr)) = checkWithEnv M.empty expr >>= \t ->
                              getType' (varTy v) >>= \t' ->
                              unifyS t t' >>
                              rewriteS t >>= \t'' ->
                              updAss' >> 
                              setTypeS expr >>= \e ->
                              return $ (pos,Val (setVarType t'' v) e)
    where getType' TyUnknown = getFreshTyVar
          getType' t = return t

checkProp :: Annot PropDecl -> TyState (Annot PropDecl)
checkProp ap@(_,Prop pn expr) = mkCtxVar expr >> 
                                  checkWithEnv M.empty expr >>= \t -> 
                                  unifyS t (TyAtom ATyBool) >> 
                                  updAss' >>
                                  setTypeS expr >>= \e ->
                                  return $ second (const $ Prop pn e) ap

checkThm :: Annot ThmDecl -> TyState (Annot ThmDecl)
checkThm (pos,(Thm t e)) = mkCtxVar (getPreExpr . thExpr $ t) >>
                             checkWithEnv M.empty (getPreExpr . thExpr $ t) >>= \ty ->
                             unifyS ty (TyAtom ATyBool) >>
                             checkWithEnv M.empty e >>= \ty' ->
                             unifyS ty' (TyAtom ATyBool) >>                             
                             setTypeS (getPreExpr . thExpr $ t) >>= \e' ->
                             setTypeS e >>= \e'' ->
                             chkProof M.empty (thProof t) >>= \ p' ->
                             return (pos,Thm (updThmPrf p' (updThmExp e' t)) e'')


getFunType :: Variable -> TyState Type
getFunType fun = do ass <- use (ctx . vars)
                    let t = M.lookup (varName fun) ass 
                    maybe (tyerr $ ErrNoType fun) return t
             
checkDeriv :: Annot DerivDecl -> TyState (Annot DerivDecl)
checkDeriv (pos,Deriv fun v css) = do ty <- getFunType fun
                                      let fun' = setVarType ty fun
                                      let argsTy = argsTypes ty
                                      when (null argsTy) (tyerr $ ErrNotFunType ty)
                                      let argTy = head argsTy
                                          env = M.singleton (varName v) argTy
                                          v' = setVarType argTy v
                                      css' <- mapM (checkCaseDeriv env argTy) css
                                      return (pos,Deriv fun' v' css')
                                  
checkCaseDeriv :: Env -> Type -> (Focus,Proof) -> TyState (Focus,Proof)
checkCaseDeriv env tyArg ((e,pt),p)= mkCtxVar e >>
                                     checkWithEnv M.empty e >>= \ty ->
                                     unifyS tyArg ty >> 
                                     use (ctx . vars) >>= \env' ->
                                     chkProof (M.union env env') p >>= \p' ->
                                     setTypeS e >>= \e' ->
                                     return ((e',pt),p')

checkTypedDecl :: Variable -> [Variable] -> PreExpr -> TyState (VarName,PreExpr)
checkTypedDecl fun args body = do ass <- use (ctx . vars)
                                  ty <- getFunType fun
                                  when (arity ty /= length args)
                                       (tyerr $ ErrArity ty (length args))
                                  let argsTy = argsTypes ty
                                  let varsTy = fromList $ (varName fun,ty): zipWith (\v t' -> (varName v,t')) args argsTy
                                  ty' <- checkWithEnv varsTy body
                                  _ <- unifyS ty (exponential ty' argsTy)
                                  body' <- setTypeS body
                                  _ <- localState args
                                  ass' <- use (ctx . vars)
                                  _ <- updAss (M.union ass ass')
                                  return (varName fun,body')

addAssumption :: (Variable,[Variable]) -> TyState ()
addAssumption (f,args) = do ass <- use (ctx . vars)
                            tyFun <- case varTy f of
                                       TyUnknown -> freshFunType (length args)
                                       ty -> return ty
                            updAss $ M.insert (varName f) tyFun ass
   where freshFunType n = replicateM n getFreshTyVar >>= \ts ->
                          getFreshTyVar >>= \tr ->
                          return (exponential tr ts)

tcCheckModule :: Module -> [(VarName,Type)] -> TyState Module
tcCheckModule m a = do let dcls = m ^. validDecls
                       let fs   = bare functions dcls
                       let sps  = bare specs dcls
                       let vls  = dcls ^. vals 
                       let thms = dcls ^. theorems
                       let prps = dcls ^. props
                       let drvs = dcls ^. derivs
                       let env  = map (\f -> (f ^. funDeclName,f ^. funDeclArgs)) fs
                               ++ map (\f -> (f ^. specName,f ^. specArgs)) sps

                       updAss (M.fromList a)
                       mapM_ addAssumption env

                       fbds  <- mapM checkFunDecl fs
                       sbds  <- mapM checkSpecDecl sps
                       drvs' <- mapM checkDeriv drvs
                       vls'  <- mapM checkVal vls
                       thms' <- mapM checkThm thms
                       prps' <- mapM checkProp prps

                       env' <- use (ctx . vars)

                       return $ execState (do validDecls . functions %= updFunTypes fbds env' ;
                                              validDecls . specs  %= updSpecTypes sbds env'    ;
                                              validDecls . vals  .= vls'                     ;
                                              validDecls . theorems .= thms';
                                              validDecls . props .= prps';
                                              validDecls . derivs .= drvs' ;
                                            ) m

updFunTypes :: [(VarName,PreExpr)] -> Env -> [Annot FunDecl] -> [Annot FunDecl]
updFunTypes bds ass fs = map updDecl fs
    where updDecl f = maybe f (flip (changeTypeDecl bd) f) $ M.lookup fname ass
              where fname = f ^. _2 ^. funDeclName ^. to varName
                    bd = lookup fname bds

updSpecTypes :: [(VarName,PreExpr)] -> Env -> [Annot SpecDecl] -> [Annot SpecDecl]
updSpecTypes bds ass fs = map updDecl fs
    where updDecl f = maybe f (flip (changeSpecType bd) f) $ M.lookup fname ass
              where fname = f ^. _2 ^. specName ^. to varName
                    bd = lookup fname bds


setVarType :: Type -> Variable -> Variable
setVarType t v = v { varTy = t}

changeTypeDecl :: Maybe PreExpr -> Type -> Annot FunDecl -> Annot FunDecl
changeTypeDecl e ty = second $ (funDeclName %~ setVarType ty) . 
                                 (funDeclArgs %~ (zipWith setVarType (argsTypes ty))) .
                                 (funDeclBody %~ maybe id (\x -> const x) e)

changeSpecType :: Maybe PreExpr -> Type -> Annot SpecDecl -> Annot SpecDecl
changeSpecType e ty = second $ (specName %~ setVarType ty) .
                                 (specArgs %~ (zipWith setVarType (argsTypes ty))) .
                                 (specSpec %~ maybe id (\x -> const x) e)


withoutLocal :: Env -> [Variable] -> Env
withoutLocal = foldr (delete . varName)

localState :: [Variable] -> TyState ()
localState args = ctx . vars %= flip withoutLocal args

typeCheckDecls :: Module -> [(VarName,Type)] -> Either String Module
typeCheckDecls m ass = either (Left . show) Right $ evalStateT (tcCheckModule m ass) initTCState

tcExprEnv :: M.Map VarName Type -> PreExpr -> Type
tcExprEnv env e = either (const TyUnknown) id $ evalStateT (checkWithEnv env e) initTCState
