{-# Language OverloadedStrings #-}
module Equ.Theories.Common where

import Equ.Syntax
import Equ.Types
import Equ.Expr
import Equ.PreExpr
import Equ.PreExpr.Symbols(natSucc)
import Equ.Theories.AbsName
import Prelude hiding (and,or)


-- | Constante True
folTrue :: Constant
folTrue = Constant { conRepr = "True"
                   , conName = CTrue
                   , conTy = tyBool
                   }

-- | Constante False  
folFalse :: Constant          
folFalse = Constant { conRepr = "False"
                    , conName = CFalse
                    , conTy = tyBool
                    }
                     


-- | Equivalencia &#8801;
folEquiv :: Operator
folEquiv = Operator { opRepr = "≡"
                    , opName = Equival
                    , opTy = tyBool :-> tyBool :-> tyBool
                    , opAssoc = ARight
                    , opNotationTy = NInfix
                    , opPrec = 1
                    , opGlyphs = ["=="]
                    }

-- | Igualdad =
folEqual :: Operator
folEqual = Operator { opRepr = "="
                    , opName = Equal
                    , opTy = tyVar "A" :-> tyVar "A" :-> tyBool
                    , opAssoc = ARight
                    , opNotationTy = NInfix
                    , opPrec = 5
                    , opGlyphs = []
                    }
   
-- | Menor estricto
lessOper :: Operator
lessOper = Operator { opRepr = "<"
                    , opName = LessThan
                    , opTy = TyAtom ATyNat :-> TyAtom ATyNat :-> tyBool
                    , opAssoc = ALeft
                    , opNotationTy = NInfix
                    , opPrec = 5
                    , opGlyphs = []
                    }
                    
-- | Menor o igual
lessOrEqOper :: Operator
lessOrEqOper = Operator { opRepr = "≤"
                    , opName = LessOrEqThan
                    , opTy =  TyAtom ATyNat :-> TyAtom ATyNat :-> tyBool
                    , opAssoc = ALeft
                    , opNotationTy = NInfix
                    , opPrec = 5
                    , opGlyphs = []
                    }
                    
                    

-- | Conjuncion &#8743;
folAnd :: Operator
folAnd = Operator { opRepr = "∧"
                  , opName = And
                  , opTy = tyBool :-> tyBool :-> tyBool
                  , opAssoc = ALeft
                  , opNotationTy = NInfix
                  , opPrec = 3
                  , opGlyphs = ["&&","/\\"]
                  }

-- | Disyuncion &#8744;
folOr :: Operator
folOr = Operator { opRepr = "∨"
                 , opName = Or
                 , opTy = tyBool :-> tyBool :-> tyBool
                 , opAssoc = ALeft
                 , opNotationTy = NInfix
                 , opPrec = 3
                 , opGlyphs = ["||","\\/"]
                 }
                 
                 
-- | Cuantificador "That"
thatQuant :: Quantifier
thatQuant = Quantifier { quantRepr = "ε"
                       , quantName = ThatQuant
                       , quantTy = tyVar "A" :-> tyVar "A"
                       }

-- | Igualdad
equal :: Expr -> Expr -> Expr
equal (Expr a) (Expr b) = Expr $ BinOp folEqual a b

-- | Menor o Igual
lessOrEq :: Expr -> Expr -> Expr
lessOrEq (Expr a) (Expr b) = Expr $ BinOp lessOrEqOper a b

-- | Menor
less :: Expr -> Expr -> Expr
less (Expr a) (Expr b) = Expr $ BinOp lessOper a b

-- | Constructor de Constantes logicas
true :: Expr
true = Expr $ Con $ folTrue

false :: Expr
false = Expr $ Con $ folFalse

-- | And
and :: Expr -> Expr -> Expr
and (Expr p) (Expr q) = Expr $ BinOp folAnd p q

-- | Or
or :: Expr -> Expr -> Expr
or (Expr p) (Expr q) = Expr $ BinOp folOr p q


-- Expresión case
caseExpr :: Expr -> [(Expr,Expr)] -> Expr
caseExpr (Expr p) ps =
    Expr $ Case p (unexpr ps)
    
    where unexpr = map (\(Expr e,Expr f) -> (e,f))



-- | Constructor de sucesor.
-- PRE: La expresión n es del tipo adecuado
-- Esta aqui porque lo necesitamos para la regla Separacion de termino
successor :: Expr -> Expr
successor (Expr n) = Expr $ UnOp natSucc n


-- | Equivalencia
equiv :: Expr -> Expr -> Expr
equiv (Expr p) (Expr q) = Expr $ BinOp folEquiv p q

-- Combinadores para axiomas usuales.
symmetryEquiv :: (Expr -> Expr -> Expr) -> Expr -> Expr -> Expr
symmetryEquiv op e e' = (e `op` e') `equiv` (e' `op` e)

-- | Asociatividad: @(e op e') op e'' = e op (e' op e'')@.
associativityEquiv :: (Expr -> Expr -> Expr) -> Expr -> Expr -> Expr -> Expr 
associativityEquiv op e e' e'' = ((e `op` e') `op` e'') `equiv` (e `op` (e' `op` e''))

-- | Neutro a izquierda: @n op e = e@.
leftNeutralEquiv :: (Expr -> Expr -> Expr) -> Expr -> Expr -> Expr
leftNeutralEquiv op n e = (n `op` e) `equiv` e

-- | Neutro a derecha: @e op n = e@.
rightNeutralEquiv :: (Expr -> Expr -> Expr) -> Expr -> Expr -> Expr
rightNeutralEquiv op n e = (e `op` n) `equiv` e


-- | Simetria: @e op e' = e' op e@.
symmetryEqual :: (Expr -> Expr -> Expr) -> Expr -> Expr -> Expr
symmetryEqual op e e' = (e `op` e') `equal` (e' `op` e)

-- | Asociatividad: @(e op e') op e'' = e op (e' op e'')@.
associativityEqual :: (Expr -> Expr -> Expr) -> Expr -> Expr -> Expr -> Expr 
associativityEqual op e e' e'' = ((e `op` e') `op` e'') `equal` (e `op` (e' `op` e''))



-- | Neutro a izquierda: @n op e = e@.
leftNeutral :: (Expr -> Expr -> Expr) -> Expr -> Expr -> Expr
leftNeutral op n e = (n `op` e) `equal` e

-- | Neutro a derecha: @n op e = e@.
rightNeutral :: (Expr -> Expr -> Expr) -> Expr -> Expr -> Expr
rightNeutral op n e = (e `op` n) `equal` e

-- | Absorbente a izquierda: @a op e = a@.
leftAbs ::  (Expr -> Expr -> Expr) -> Expr -> Expr -> Expr
leftAbs op a e = (a `op` e) `equal` a


-- | Absorbente a derecha: @e op a = a@.
rightAbs ::  (Expr -> Expr -> Expr) -> Expr -> Expr -> Expr
rightAbs op a e = (e `op` a) `equal` a

{-| Expresiones para la construcción de axiomas para cuantificadores en general.
    En cada teoria se puede llamar a estas funciones para construir los axiomas
    particulares. -}



-- | Rango Vacío para un cuantificador.
{- Esta funcion crea la expresion para un axioma de rango vacio. Toma una funcion
   que construye la expresion cuantificada, una funcion que crea la relación del cuantificador
   (por ejemplo equiv para el para todo, equal para la sumatoria). una variable, el termino y
   el elemento neutro del cuantificador. La forma general de la regla es:
   < Q v : False : term > rel neuter
   -}
emptyRange :: (Variable -> Expr -> Expr -> Expr) -> (Expr -> Expr -> Expr) ->
              Variable -> Expr -> Expr -> Expr
emptyRange quant r v term neuter = r (quant v false term) neuter

-- | Rango Unitario para un cuantificador
{- Esta funcion crea la expresion para un axioma de rango unitario. Toma una funcion
   que construye la expresion cuantificada, una funcion que crea la relación del cuantificador
   (por ejemplo equiv para el para todo, equal para la sumatoria). una variable, una expresion
   y el termino. La forma general de la regla es:
   < Q v : v = e : term > rel term[v:=e] 
   La expresión general no dice nada sobre la expresion de la derecha. Se chequea por
   medio de condiciones que esa expresion es la correpondiente.-}
unitRange :: (Variable -> Expr -> Expr -> Expr) -> (Expr -> Expr -> Expr) ->
             Variable -> Expr -> Expr -> Expr -> Expr
unitRange quant rel v e term derExpr = rel (quant v qrange term) derExpr
    where qrange = (Expr $ Var v) `equal` e
--           subst = let (Expr pe) = e in
--                     M.singleton v pe

          
-- Particion de rango.
{- Regla general:
        < Q v : or R S : T > rel < Q v : R : T > oper < Q v : S : T >
-}
partRange :: (Variable -> Expr -> Expr -> Expr) -> (Expr -> Expr -> Expr) ->
             (Expr -> Expr -> Expr) -> Variable -> Expr -> Expr -> Expr -> Expr
partRange quant re oper v r s t = re (quant v (or r s) t) (oper (quant v r t) (quant v s t))

    
-- PARTICION DE RANGO GENERALIZADA: Necesito importar el cuantificador existencial.

-- | Regla del término.
{- Forma general:
   < Q v : range : t op g > rel < Q v : range : t > op < Q v : range : g >
   -}
termRule :: (Variable -> Expr -> Expr -> Expr) -> (Expr -> Expr -> Expr) ->
            (Expr -> Expr -> Expr) -> Variable -> Expr -> Expr -> Expr -> Expr
termRule quant r oper v qrange term1 term2 =
    r (quant v qrange (oper term1 term2)) (oper (quant v qrange term1)
                                                  (quant v qrange term2))
-- | Regla del término constante
{- Forma general:
   < Q v : range : term > rel term
   Notar que en term no debe ocurrir libremente la variable v. El rango debe ser no vacio.
   Por ultimo el operador debe ser idempotente.
   -}
   
constTermRule :: (Variable -> Expr -> Expr -> Expr) -> (Expr -> Expr -> Expr) ->
                 Variable -> Expr -> Expr -> Expr
constTermRule quant re v r c = re (quant v r c) c

-- | Distributividad a izquierda en cuantificadores
{- Puede usarse cuando tenemos un operador que es distributivo a izquierda 
   con respecto al operador cuantificado y se cumple : El rango es no vacío o 
   el elemento neutro del operador cuantificado existe y es absorvente para el otro
   operador (o ambas cosas). Forma general:
   < Q i : range : x oper T >   rel   x oper < Q i : R : T >
   
   -}
distLeftQuant :: (Variable -> Expr -> Expr -> Expr) -> (Expr -> Expr -> Expr) ->
                 (Expr -> Expr -> Expr) -> Variable -> Expr -> Expr -> Expr -> Expr
distLeftQuant quant r oper v qrange x term =
    r (quant v qrange (oper x term)) (oper x (quant v qrange term))

    
-- | Distributividad a derecha en cuantificadores
distRightQuant :: (Variable -> Expr -> Expr -> Expr) -> (Expr -> Expr -> Expr) ->
                 (Expr -> Expr -> Expr) -> Variable -> Expr -> Expr -> Expr -> Expr
distRightQuant quant r oper v qrange x term =
    r (quant v qrange (oper term x)) (oper (quant v qrange term) x)


-- | Reinidizado
{- Forma general: 
   < Q i : succ var1 <= i and i < succ var2 : varTerm1 > 
   ==
   < Q i : var1 <= i and i < var2 : varTerm2 > 
   donde varTerm2 = varTerm1 donde reemplazas i por (succ i)
-}
reindex :: (Variable -> Expr -> Expr -> Expr) -> (Expr -> Expr -> Expr) ->
           Variable -> Expr ->  Expr  -> Expr -> Expr -> Expr
reindex quant r varQ var1 var2 varTerm1 varTerm2 =
    r (quant varQ range1 term1) (quant varQ range2 term2)
    
    where range1 = and ((successor var1) `lessOrEq` exprVarQ) (exprVarQ `less` (successor var2))
          term1 = varTerm1
          range2 = and (var1 `lessOrEq` exprVarQ) (exprVarQ `less` var2)
          term2 = varTerm2
          exprVarQ = Expr $ Var varQ
 
    
-- | Regla de Anidado
{- De nuevo acá tenemos condiciones para poder aplicar la regla.
   En los predicados R y S, en R no debe ocurrir una de las variables cuantificadas.
   Forma general:
   < Q v,w : R.v && S.v.w : T.v.w > rel < Q v : R.v : < Q w : S.v.w : T.v.w > >
   
   Notar que la manera que tenemos de representar cuantificaciones dobles es:
   < Q v,w : R : T > equivale a < Q v : True : < Q w : R : T > >
   -}
nestedRule :: (Variable -> Expr -> Expr -> Expr) -> (Expr -> Expr -> Expr) ->
              Variable -> Variable -> Expr -> Expr -> Expr -> Expr
nestedRule quant re v w r s term =
    re (quant v true (quant w (and r s) term)) 
        (quant v r (quant w s term))
   
changeVar :: (Variable -> Expr -> Expr -> Expr) -> (Expr -> Expr -> Expr) ->
              Variable -> Variable -> Expr -> Expr -> Expr -> Expr -> Expr
changeVar quant re v w r t r' t' =
    re (quant v r t) (quant w r' t')

    
-- Separación de Término
{- 
    < Q i : m <= i < succ n : t > rel t[i -> m] oper < Q i : m <= i < n : t [i->i+1]>
-}
termSep1 :: (Variable -> Expr -> Expr -> Expr) -> (Expr -> Expr -> Expr) ->
           (Expr -> Expr -> Expr) ->
           Variable -> Expr -> Expr -> Expr -> Expr -> Expr -> Expr
termSep1 quant r oper v m n t1 t2 t3 =
    r (quant v ((m `lessOrEq` exprV) `and` (exprV `less` successor n)) t1)
        (oper t2 (quant v ((m `lessOrEq` exprV) `and` (exprV `less` n)) t3))
    where exprV = Expr $ Var v
    
{- 
    < Q i : m <= i < succ n : t > rel < Q i : m <= i < n : t> oper t[i -> n]
-}
termSepLast :: (Variable -> Expr -> Expr -> Expr) -> (Expr -> Expr -> Expr) ->
           (Expr -> Expr -> Expr) ->
           Variable -> Expr -> Expr -> Expr -> Expr -> Expr
termSepLast quant r oper v m n t1 t2 =
    r (quant v ((m `lessOrEq` exprV) `and` (exprV `less` successor n)) t1)
        (oper (quant v ((m `lessOrEq` exprV) `and` (exprV `less` n)) t1) t2)
    where exprV = Expr $ Var v
    
    
    
-- | Extensión de applySubst a Expr
applySubst' :: Expr -> ExprSubst -> Expr
applySubst' (Expr p) = Expr . applySubst p

isBoolean :: PreExpr -> Maybe Bool
isBoolean (Con t) = case conName t of
                      CTrue -> Just True
                      CFalse -> Just False
                      _ -> Nothing
isBoolean _ = Nothing

