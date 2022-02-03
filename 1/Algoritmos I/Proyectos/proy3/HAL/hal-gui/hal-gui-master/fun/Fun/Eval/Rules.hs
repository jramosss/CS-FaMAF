
----------------------------------------------------------------------------
-- |
-- Module      :  $Headers$
-- Copyright   :  (c) Proyecto Theona, 2012-2013
--                (c) Alejandro Gadea, Emmanuel Gunther, Miguel Pagano
-- License     :  <license>
-- 
-- Maintainer  :  miguel.pagano+theona@gmail.com
-- Stability   :  experimental
-- Portability :  portable
--
-- Reglas de evaluación.
-- 
----------------------------------------------------------------------------

{-# LANGUAGE OverloadedStrings #-}
module Fun.Eval.Rules ( EvalRule
                      , binOpRules
                      , unOpRules
                      , ifRules
                      , matchRule
                      , matchRules
                      , matchRuleTrace
                      , matchRulesTrace
                      ) where

import Equ.Theories.Arith
import Equ.Theories.FOL
import Equ.Theories.List
import Equ.Expr
import Equ.PreExpr
import Equ.Types
import Equ.Matching

import Data.Maybe(catMaybes)
import Control.Monad


import Prelude hiding(pred,sum,length,concat,take,drop,and,or)

-- | Para cada teoría definimos las reglas de cada operador

-- | Arith: Operadores Sucesor, Suma, Producto, Predecesor, Resta

data EvalRule = EvalRule {
        lexpr :: Expr
      , rexpr :: Expr
      , name :: String
}
instance Show EvalRule where
    show = name

-- | Clasificamos las reglas según la forma de las expresiones
binOpRules :: [EvalRule]
binOpRules = [ePlusZero,ePlusSucc,eProdZero,eProdSucc,eMinZero,eMinSucc,
              eAndTrue,eAndFalse,eOrTrue,eOrFalse,eImplTrue,
              eImplFalse,eConsecTrue,eConsecFalse,eEquivTrue,eEquivFalse,
              eEqTrue,eEqFalseZero,eEqFalseSucc,eEqGen,eLessTrue,eLessFalse,eLessGen,
              eDiscrepTrue,eDiscrepFalse,eConcatEmpty,eConcatNotEmpty,
              eIndexZero,eIndexSucc,eTakeEmpty,eTakeZero,eTakeSucc,
              eDropEmpty,eDropZero,eDropSucc]

unOpRules :: [EvalRule]
unOpRules = [ePredSucc,
             eNegTrue,eNegFalse,
             eLongEmpty,eLongNotEmpty]

ifRules :: [EvalRule]
ifRules = [eIfTrue,eIfFalse]

-- | Dada una expresión e y una regla r, realiza matching entre la expresión
--   izquierda de r y e, retornando la substitución aplicada a la expresion derecha
--   de la regla.
matchRule :: PreExpr -> EvalRule -> Maybe PreExpr
matchRule e r =
    either (const Nothing)
           (-- ver qué pasa con el otro elemento que retorna match)
            \(subst,_) -> return (rexpr r) >>=
            \(Expr re) -> return (applySubst re subst))
           (match lpreexpr e)
           
    where Expr lpreexpr = lexpr r
    
-- | Dada una expresión e y una lista de reglas rs, llama a matchRule
--   con cada regla, retornando la substitución de la primera que haya
--   tenido éxito.
matchRules :: PreExpr -> [EvalRule] -> Maybe PreExpr
matchRules e rs = matchRules' e rs matchRule

              
              
matchRuleTrace :: PreExpr -> EvalRule -> Maybe (PreExpr,String)
matchRuleTrace e r = matchRule e r >>= \e' -> return (e',name r)

-- | Versión de matchRules pero que también devuelve la regla
--   que se aplicó
matchRulesTrace :: PreExpr -> [EvalRule] -> Maybe (PreExpr,String)
matchRulesTrace e rs = matchRules' e rs matchRuleTrace
    
    
matchRules' :: Show a => PreExpr -> [EvalRule] -> (PreExpr -> EvalRule -> Maybe a) -> Maybe a
matchRules' e rs f = return (catMaybes $ map (f e) rs) >>= \ls ->
                     if null ls then Nothing else Just (head ls)

                              
-- Variables para las reglas:
varZ1,varZ2,varZ3 :: Expr
varZ1 = Expr $ Var $ var "z1" $ TyVar "A"
varZ2 = Expr $ Var $ var "z2" $ TyVar "A"
varZ3 = Expr $ Var $ var "z3" $ TyVar "A"

-- Reglas:

-- *** NATURALES ***

-- | SUMA
ePlusZero :: EvalRule
ePlusZero = EvalRule {
                lexpr = sum zero varZ1
              , rexpr = varZ1
              , name =  "E-PLUSCERO"
}

ePlusSucc :: EvalRule
ePlusSucc = EvalRule {
                lexpr = sum (successor varZ1) varZ2
              , rexpr = successor (sum varZ1 varZ2)
              , name =  "E-PLUSSUCC"
}


-- | PRODUCTO
eProdZero :: EvalRule
eProdZero = EvalRule {
                lexpr = prod zero varZ1
              , rexpr = zero
              , name =  "E-PRODCERO"
}

eProdSucc :: EvalRule
eProdSucc = EvalRule {
                lexpr = prod (successor varZ1) varZ2
              , rexpr = sum (prod varZ1 varZ2) varZ2
              , name =  "E-PRODSUCC"
}

-- | PREDECESOR
ePredSucc :: EvalRule
ePredSucc = EvalRule {
                lexpr = pred (successor varZ1)
              , rexpr = varZ1
              , name =  "E-PREDSUCC"
}

-- | RESTA
eMinZero :: EvalRule
eMinZero = EvalRule {
                lexpr = substr varZ1 zero
              , rexpr = varZ1
              , name =  "E-MINCERO"
}

eMinSucc :: EvalRule
eMinSucc = EvalRule {
                lexpr = substr varZ1 (successor varZ2)
              , rexpr = pred (substr varZ1 varZ2)
              , name =  "E-MINSUCC"
}

-- *** BOOLEANOS ***

-- | NEGACION
eNegTrue :: EvalRule
eNegTrue = EvalRule {
                lexpr = neg true
              , rexpr = false
              , name =  "E-NEGTRUE"
}

eNegFalse :: EvalRule
eNegFalse = EvalRule {
                lexpr = neg false
              , rexpr = true
              , name =  "E-NEGFALSE"
}

-- | CONJUNCION
eAndTrue :: EvalRule
eAndTrue = EvalRule {
                lexpr = and true varZ1
              , rexpr = varZ1
              , name =  "E-ANDTRUE"
}

eAndFalse :: EvalRule
eAndFalse = EvalRule {
                lexpr = and false varZ1
              , rexpr = false
              , name =  "E-ANDFALSE"
}

-- | DISJUNCION
eOrTrue :: EvalRule
eOrTrue = EvalRule {
                lexpr = or true varZ1
              , rexpr = true
              , name =  "E-ORTRUE"
}

eOrFalse :: EvalRule
eOrFalse = EvalRule {
                lexpr = or false varZ1
              , rexpr = varZ1
              , name =  "E-ORFALSE"
}

-- | IMPLICACION
eImplTrue :: EvalRule
eImplTrue = EvalRule {
                lexpr = impl true varZ1
              , rexpr = varZ1
              , name =  "E-IMPLTRUE"
}

eImplFalse :: EvalRule
eImplFalse = EvalRule {
                lexpr = impl false varZ1
              , rexpr = true
              , name =  "E-IMPLFALSE"
}

-- | CONSECUENCIA
eConsecTrue :: EvalRule
eConsecTrue = EvalRule {
                lexpr = conseq true varZ1
              , rexpr = true
              , name =  "E-CONSECTRUE"
}

eConsecFalse :: EvalRule
eConsecFalse = EvalRule {
                lexpr = conseq false varZ1
              , rexpr = neg varZ1
              , name =  "E-CONSECFALSE"
}

-- | EQUIVALENCIA
eEquivTrue :: EvalRule
eEquivTrue = EvalRule {
                lexpr = equiv true varZ1
              , rexpr = varZ1
              , name =  "E-EQUIVTRUE"
}

eEquivFalse :: EvalRule
eEquivFalse = EvalRule {
                lexpr = equiv false varZ1
              , rexpr = neg varZ1
              , name =  "E-EQUIVTRUE"
}

-- | DISCREPANCIA
eDiscrepTrue :: EvalRule
eDiscrepTrue = EvalRule {
                lexpr = discrep true varZ1
              , rexpr = neg varZ1
              , name =  "E-EQUIVTRUE"
}

eDiscrepFalse :: EvalRule
eDiscrepFalse = EvalRule {
                lexpr = discrep false varZ1
              , rexpr = varZ1
              , name =  "E-EQUIVTRUE"
}

-- | IGUALDAD
eEqTrue :: EvalRule
eEqTrue = EvalRule {
                lexpr = equal zero zero
              , rexpr = true
              , name =  "E-EQTRUE"
}

eEqFalseZero :: EvalRule 
eEqFalseZero = EvalRule {
                lexpr = equal zero (successor varZ1)
              , rexpr = false
              , name =  "E-EQFALSECERO"
}

eEqFalseSucc :: EvalRule 
eEqFalseSucc = EvalRule {
                lexpr = equal (successor varZ1) zero
              , rexpr = false
              , name =  "E-EQFALSESUCC"
}

eEqGen :: EvalRule 
eEqGen = EvalRule {
                lexpr = equal (successor varZ1) (successor varZ2)
              , rexpr = equal varZ1 varZ2
              , name =  "E-EQGEN"
}

-- | MENOR

eLessTrue :: EvalRule
eLessTrue = EvalRule {
                lexpr = less zero (successor varZ1)
              , rexpr = true
              , name =  "E-LESSTRUE"
}

eLessFalse :: EvalRule
eLessFalse = EvalRule {
                lexpr = less varZ1 zero
              , rexpr = false
              , name =  "E-LESSFALSE"
}

eLessGen :: EvalRule
eLessGen = EvalRule {
                lexpr = less (successor varZ1) (successor varZ2)
              , rexpr = less varZ1 varZ2
              , name =  "E-LESSGEN"
}




-- *** LISTAS ***

-- | Longitud
eLongEmpty :: EvalRule
eLongEmpty = EvalRule {
                lexpr = length emptyList
              , rexpr = zero
              , name =  "E-LONGEMPTY"
}

eLongNotEmpty :: EvalRule
eLongNotEmpty = EvalRule {
                lexpr = length (append varZ1 varZ2)
              , rexpr = successor (length varZ2)
              , name =  "E-LONGNOTEMPTY"
}

-- | Concatenación
eConcatEmpty :: EvalRule
eConcatEmpty = EvalRule {
                lexpr = concat emptyList varZ1
              , rexpr = varZ1
              , name =  "E-CONCATEMPTY"
}

eConcatNotEmpty :: EvalRule
eConcatNotEmpty = EvalRule {
                lexpr = concat (append varZ1 varZ2) varZ3
              , rexpr = append varZ1 (concat varZ2 varZ3)
              , name =  "E-CONCATNOTEMPTY"
}

-- | Indexado
eIndexZero :: EvalRule
eIndexZero = EvalRule {
                lexpr = index (append varZ1 varZ2) zero
              , rexpr = varZ1
              , name =  "E-INDEXCERO"
}

eIndexSucc :: EvalRule
eIndexSucc = EvalRule {
                lexpr = index (append varZ1 varZ2) (successor varZ3)
              , rexpr = index varZ2 varZ3
              , name =  "E-INDEXSUCC"
}

-- | Take
eTakeEmpty :: EvalRule
eTakeEmpty = EvalRule {
                lexpr = take emptyList varZ1
              , rexpr = emptyList
              , name =  "E-TAKEEMPTY"
}

eTakeZero :: EvalRule
eTakeZero = EvalRule {
                lexpr = take varZ1 zero
              , rexpr = emptyList
              , name =  "E-TAKECERO"
}

eTakeSucc :: EvalRule
eTakeSucc = EvalRule {
                lexpr = take (append varZ1 varZ2) (successor varZ3)
              , rexpr = append varZ1 (take varZ2 varZ3)
              , name =  "E-TAKESUCC"
}

-- | Drop
eDropEmpty :: EvalRule
eDropEmpty = EvalRule {
                lexpr = drop emptyList varZ1
              , rexpr = emptyList
              , name =  "E-DROPEMPTY"
}

eDropZero :: EvalRule
eDropZero = EvalRule {
                lexpr = drop varZ1 zero
              , rexpr = varZ1
              , name =  "E-DROPCERO"
}

eDropSucc :: EvalRule
eDropSucc = EvalRule {
                lexpr = drop (append varZ1 varZ2) (successor varZ3)
              , rexpr = take varZ2 varZ3
              , name =  "E-DROPSUCC"
}


-- IF

-- | Constructor de Expresión If
ifexpr :: Expr -> Expr -> Expr -> Expr
ifexpr (Expr b) (Expr e1) (Expr e2) = Expr $ If b e1 e2

eIfTrue :: EvalRule
eIfTrue = EvalRule {
                lexpr = ifexpr true varZ1 varZ2
              , rexpr = varZ1
              , name =  "E-IFTRUE"
}

eIfFalse :: EvalRule
eIfFalse = EvalRule {
                lexpr = ifexpr false varZ1 varZ2
              , rexpr = varZ2
              , name =  "E-IFFALSE"
}
