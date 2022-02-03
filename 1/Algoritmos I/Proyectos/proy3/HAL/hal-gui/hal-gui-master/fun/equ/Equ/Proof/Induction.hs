{-# Language OverloadedStrings #-}
module Equ.Proof.Induction (
    createIndHypothesis
    ) where

import Equ.Proof.Proof
import Equ.Proof.Condition
import qualified Equ.PreExpr as PE
import Equ.Syntax
import Equ.Types
import Equ.Expr
import Equ.TypeChecker(unifyTest)
import Equ.Theories(relToOp, createHypothesis)
import Equ.Rule(Relation)

import qualified Data.Map as Map
import Data.Text

-- | Dada la relacion de una prueba, el foco inicial, el foco final, un pattern y una
--   variable sobre la q se hace inducción, se construye la hipótesis inductiva.

-- Asume que el pattern es un constructor inductivo.
createIndHypothesis :: Relation -> PE.Focus -> PE.Focus -> PE.Focus -> Variable 
                       -> Text -> [Hypothesis]
createIndHypothesis rel f1 f2 p x nombre =  case PE.toExpr p of
             (PE.UnOp _ e@(PE.Var _)) -> [createIndHyp e]
             (PE.BinOp op e1 e2) -> case tType op of
                        t1 :-> t2 :-> _ ->
                            case (unifyTest t1 tyVarInd,unifyTest t2 tyVarInd) of
                                 (True,True) ->  [createIndHyp e1, createIndHyp e2]
                                 (True,False) -> [createIndHyp e1]
                                 (False,True) -> [createIndHyp e2]
                                 _ -> []
                        _ -> []
             _ -> []
    where hypIndExpr expr = PE.BinOp (relToOp rel) 
                                     (PE.applySubst (PE.toExpr f1) subst)
                                     (PE.applySubst (PE.toExpr f2) subst)
              where subst = Map.singleton x expr
          createIndHyp expr = createHypothesis nombre (Expr $ hypIndExpr expr) (GenConditions [InductiveHypothesis expr])
          tyVarInd = varTy x
