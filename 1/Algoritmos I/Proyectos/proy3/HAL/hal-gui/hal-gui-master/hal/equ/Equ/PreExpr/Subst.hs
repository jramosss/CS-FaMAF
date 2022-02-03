-- | En este modulo definimos las funciones necesarias para analizar el
--  matching entre preExpresiones.
module Equ.PreExpr.Subst
    ( applySubst
    , ExprSubst
    , freeVars
    , freshVar
    )
    where

import Equ.Syntax
import Equ.PreExpr.Internal
import Equ.Types

import qualified Data.Map as M
import Data.Set (Set,union,delete,empty,insert,member,unions,difference)
import qualified Data.Set as S (fold) 
import Data.Function (on)
import Data.Text(pack)

-- | Mapa de substituciones de variable - preExpresiones.
type ExprSubst = M.Map Variable PreExpr


freeVars :: PreExpr -> Set Variable
freeVars (Var v) = insert v empty
freeVars (Con _) = empty
freeVars (PrExHole _) = empty
freeVars (UnOp _ e) = freeVars e
freeVars (BinOp _ e1 e2) = freeVars e1 `union` freeVars e2
freeVars (App e1 e2) = freeVars e1 `union` freeVars e2
freeVars (Quant _ v e1 e2) = delete v $ freeVars e1 `union` freeVars e2
freeVars (Paren e) = freeVars e
freeVars (If b t f) = freeVars b `union` freeVars t `union` freeVars f
freeVars (Case e cs) = freeVars e `union` (unions . map (uncurry ((flip difference) `on` freeVars))) cs


-- | Esta funci&#243;n devuelve una variable fresca con respecto a un conjunto de variables
freshVar :: Set Variable -> Variable
freshVar s = firstNotIn s infListVar
    where infListVar = [var (pack $ "v" ++ show n) TyUnknown | n <- [(0::Int)..]]
          -- PRE: xs es infinita
          firstNotIn set xs | head xs `member` set = firstNotIn set $ tail xs
                            | otherwise = head xs


-- | Aplica una substituci&#243;n a una expresi&#243;n dada.
applySubst :: PreExpr -> ExprSubst -> PreExpr
applySubst (Var v) s = M.findWithDefault (Var v) v s
applySubst (UnOp op e) s = UnOp op $ applySubst e s
applySubst (BinOp op e f) s = BinOp op (applySubst e s) (applySubst f s)
applySubst (App e f) s = App (applySubst e s) (applySubst f s)
applySubst (Quant q v e1 e2) s = Quant q v (applySubst e1 s) (applySubst e2 s)
applySubst (Paren e) s = Paren $ applySubst e s
applySubst (PrExHole h) _ = PrExHole h
applySubst (Con c) _ = Con c
applySubst (If b t f) s = If (applySubst b s) (applySubst t s) (applySubst f s)
applySubst (Case e cs) s = Case (applySubst e s) (map (\(p,e') -> (p, applySubst e' (subPat s p))) cs)
    where subPat s' = S.fold (\v -> M.insert v (Var v)) s' . freeVars

