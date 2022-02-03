-- | El modulo de constructores de listas y sus s&#237;mbolos de
-- funci&#243;n.
{-# Language OverloadedStrings #-}
module Equ.Theories.List 
    ( -- * Constructores y operadores.
      listEmpty
    , listApp 
    , listIndex
    , listConcat
    , listLength
    , listTake
    , listDrop
    -- ** Listas de constructores y operadores
    , theoryConstantsList
    , theoryOperatorsList
    , theoryQuantifiersList
    -- ** Lista de axiomas de la teoria
    , theoryAxiomList
    -- * Versión tipada de operadores.
    , emptyList
    , append
    , concat
    , length
    , take
    , drop
    , index
    )
    where
    
import Prelude hiding (concat,take,drop,length,sum)
import Equ.Syntax
import Equ.Types
import Equ.Expr
import Equ.PreExpr
import Equ.PreExpr.Symbols
import Equ.Rule
import Equ.Theories.AbsName
import Equ.Theories.Common
import Equ.Proof.Condition

import Data.Text (Text)

import Equ.Theories.Arith (zero,successor)


-- | Constantes de listas.
theoryConstantsList :: [Constant]
theoryConstantsList = [listEmpty]
-- | Operadores de listas.
theoryOperatorsList :: [Operator]
theoryOperatorsList = [listApp,listConcat,listDrop,listIndex,listLength,listTake]
-- | Quantificadores de listas.
theoryQuantifiersList :: [Quantifier]
theoryQuantifiersList = []

-- | Constructor de variable de tipo lista polimorfica; el primer string es
-- el nombre de la variable, el segundo el nombre de la variable de tipo
varList :: Text -> String -> Expr
varList s t = Expr $ Var $ listVar s t
    where listVar v ty = var v $ tyListVar ty

-- | Constructor de lista vacia
emptyList :: Expr
emptyList = Expr $ Con $ listEmpty

-- | Constructor de insercion por izquierda
-- PRE: Las expresiones son del tipo adecuado
append :: Expr -> Expr -> Expr
append (Expr x) (Expr xs) = Expr $ BinOp listApp x xs

-- | Constructor de concatenacion
concat :: Expr -> Expr -> Expr
concat (Expr xs) (Expr ys) = Expr $ BinOp listConcat xs ys

-- | Constructor de length
length :: Expr -> Expr
length (Expr xs) = Expr $ UnOp listLength xs

-- | Constructor de take
take :: Expr -> Expr -> Expr
take (Expr xs) (Expr n) = Expr $ BinOp listTake xs n

-- | Constructor de drop
drop :: Expr -> Expr -> Expr
drop (Expr xs) (Expr n) = Expr $ BinOp listDrop xs n

-- | Constructor de index
index :: Expr -> Expr -> Expr
index (Expr xs) (Expr n) = Expr $ BinOp listIndex xs n


-- Regla para la definicion de Concatenar (++)

{- 

    xs ++ ys = case xs of
                [] -> ys
                (z |> zs) -> z  |> (zs ++ ys)

   -}
defConcat :: (Text,Expr,Condition)
defConcat = ( "Definición de Concatenar",
              (varXS `concat` varYS) `equal` 
              (caseExpr varXS 
                       [(emptyList,varYS),
                        (varZ `append` varZS,varZ `append` (varZS `concat` varYS))])
              ,
              GenConditions []
            )
    where varZ = Expr $ Var $ var "z" $ tyVar "A"
          varXS = varList "xs" "A"
          varYS = varList "ys" "A"
          varZS = varList "zs" "A"


-- Reglas para la definicion de length (#)

{- 
    #xs = case xs of
                [] -> 0
                (y |> ys) -> 1 + # ys
-}
defLength :: (Text,Expr,Condition)
defLength = ( "Definición de Longitud"
            , (length varXS) `equal` 
              (caseExpr varXS
                    [(emptyList,zero),
                    (varZ `append` varZS,successor (length varZS))])
                 , GenConditions []
            )
    where varZ = Expr $ Var $ var "z" $ tyVar "A"
          varXS = varList "xs" "A"
          varZS = varList "zs" "A"

-- NOTA: En el libro Calculo de Programas, se incluyen otras reglas
-- para la definicion de length con respecto a las operaciones concat,
-- take y drop. Se opt&#243; por incluir solo las que involucran
-- constructores, las demas pueden derivarse.


-- Reglas para la definicion de take.

{- 

    take xs n = case n of
                    0 -> []
                    succ m -> case xs of
                                  [] -> []
                                  y | ys -> y |> (take ys m)
                                  

-}


defTake :: (Text,Expr,Condition)
defTake = ( "Definición de Take"
          , (take varXS varN) `equal`
            (caseExpr varN
                [(zero,emptyList),
                 (successor varM,caseExpr varXS
                                    [(emptyList,emptyList),
                                     (varZ `append` varZS,varZ `append` (take varZS varM))])])
          , GenConditions []
          )
          
    where varZ = Expr $ Var $ var "z" $ tyVar "A"
          varXS = varList "xs" "A"
          varZS = varList "zs" "A"
          varN = Expr $ Var $ var "n" $ TyAtom ATyNat
          varM = Expr $ Var $ var "m" $ TyAtom ATyNat
                                    

          

-- Reglas para la definicion de drop

{-     drop xs n = case n of
                    0 -> xs
                    succ m -> case xs of
                                  [] -> []
                                  y | ys -> (take ys m)
                                  
-}
defDrop :: (Text,Expr,Condition)
defDrop = ( "Definición de Drop"
          , (drop varXS varN) `equal`
            (caseExpr varN
                [(zero,varXS),
                 (successor varM,caseExpr varXS
                                    [(emptyList,emptyList),
                                     (varZ `append` varZS,take varZS varM)])])
          , GenConditions []
          )
          
    where varZ = Expr $ Var $ var "z" $ tyVar "A"
          varXS = varList "xs" "A"
          varZS = varList "zs" "A"
          varN = Expr $ Var $ var "n" $ TyAtom ATyNat
          varM = Expr $ Var $ var "m" $ TyAtom ATyNat
          
-- Reglas para la definicion de Index

{-
    (x|>xs).n = case n of
                    0 -> x
                    succ m -> xs.m
-}

defIndex :: (Text,Expr,Condition)
defIndex = ( "Definición de Indexar"
          , (index (varX `append` varXS) varN) `equal`
            (caseExpr varN
                [(zero,varX),
                 (successor varM,index varXS varM)])
          , GenConditions []
          )
          
    where varX = Expr $ Var $ var "x" $ tyVar "A"
          varXS = varList "xs" "A"
          varN = Expr $ Var $ var "n" $ TyAtom ATyNat
          varM = Expr $ Var $ var "m" $ TyAtom ATyNat


theoryAxiomList :: [(Text,Expr,Condition)]
theoryAxiomList = [ 
                    defConcat
                  -- Cardinal
                  , defLength
                  -- Tomar n elementos
                  , defTake
                  -- Tirar n elementos
                  , defDrop
                  -- Proyeccion n-esimo elemento
                  , defIndex                    
                  ]