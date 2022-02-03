{-# Language OverloadedStrings #-}
-- | Expresiones de ejemplos usados en matching y en parser.
module TestSuite.Tests.Samples where

import Equ.Types
import Equ.Parser
import Equ.Syntax
import Equ.PreExpr
import Equ.Theories.FOL hiding (true, false)
import Equ.Theories.List
import Equ.Theories.Arith hiding (zero)
import Data.Text (pack)


p :: Variable
p = Variable { varName = pack "p"
             , varTy = TyUnknown
             }

q :: Variable
q = Variable { varName = pack "q"
             , varTy = TyUnknown
             }

x :: Variable
x = Variable { varName = pack "x"
             , varTy = TyUnknown
             }

w :: Variable
w = Variable { varName = pack "w"
             , varTy = TyUnknown
             }

z :: Variable
z = Variable { varName = pack "z"
              , varTy = TyUnknown
              }

xs :: Variable
xs = Variable { varName = pack "xs"
              , varTy = TyUnknown
              }

ys :: Variable
ys = Variable { varName = pack "ys"
              , varTy = TyUnknown
              }
              
s :: Func
s = Func { funcName = pack "S"
         , funcTy = tyVar "VInt0"
         }

g :: Func
g = Func { funcName = pack "G"
         , funcTy = tyVar "VInt0"
         }
         
f :: Func
f = Func { funcName = pack "F"
         , funcTy = tyVar "VInt0"
         }

-- Variables a utilizar para los demas casos de tests.
v0 :: Variable
v0 = var "v0" TyUnknown
y :: Variable
y = var "y" TyUnknown

-- True
true :: PreExpr
true = Con $ folTrue

-- False
false :: PreExpr
false = Con $ folFalse

-- 0
zero :: PreExpr
zero = Con $ natZero

-- 1
one :: PreExpr
one = intToCon 1

-- 2
two :: PreExpr
two = intToCon 2

-- p v p
pVp :: PreExpr
pVp = BinOp folOr (Var p) (Var p)

-- p v q
pVq :: PreExpr
pVq = BinOp folOr (Var p) (Var q)

-- True v False
trueVfalse :: PreExpr
trueVfalse = BinOp folOr true false

-- S@y
sAppy :: PreExpr
sAppy = App (Fun s) (Var y)

-- F@x
fAppx :: PreExpr
fAppx = App (Fun f) (Var x)

-- S@(0)
sApp0 :: PreExpr
sApp0 = App (Fun s) zero

-- (x+S@0)
xPlussApp0 :: PreExpr
xPlussApp0 = Paren $ (BinOp natSum (Var x) sApp0)

-- S@((x+S@0))
sApp1 :: PreExpr
sApp1 = App (Fun s) xPlussApp0

-- x + S@y + z
xPlusSyPlusZ :: PreExpr
xPlusSyPlusZ = BinOp natSum (BinOp natSum (Var x) sAppy) (Var z)

-- S@y + S@((x+S@0)) + z
sAppyPlusSomePlusz :: PreExpr
sAppyPlusSomePlusz = BinOp natSum (BinOp natSum sAppy sApp1) (Var z)

-- Dada una preExpresion genera una lista con esa sola preExpresion.
makeList1 :: PreExpr -> PreExpr
makeList1 x = BinOp listApp x (Con listEmpty)

-- Dado un par de preExpresiones genera la lista que las contiene.
makeList2 :: PreExpr -> PreExpr -> PreExpr
makeList2 x y = BinOp listApp x $ BinOp listApp y (Con listEmpty)

-- Dado un par de preExpresiones les "infija" el operador concatenar de listas.
makeListConcat :: PreExpr -> PreExpr -> PreExpr
makeListConcat x y = BinOp listConcat x y

-- [x]
listX :: PreExpr
listX = makeList1 (Var x)

-- [y,w]
listYW :: PreExpr
listYW = makeList2 (Var y) (Var w)

-- [x] ++ [y,w]
listXConcatListYW :: PreExpr
listXConcatListYW = makeListConcat listX listYW

-- (#([x] ++ [y,w])) 
lengthListXYW :: PreExpr
lengthListXYW = Paren $ UnOp listLength $ Paren listXConcatListYW

-- (#([x] ++ [y,w])) + z
lengthListPlusz :: PreExpr
lengthListPlusz = BinOp natSum lengthListXYW (Var z)

-- [0]
listZero :: PreExpr
listZero = makeList1 zero

-- [1,2]
listOneTwo :: PreExpr
listOneTwo = makeList2 one two

-- [0] ++ [1,2]
listZeroConcatOneTwo :: PreExpr
listZeroConcatOneTwo = makeListConcat listZero listOneTwo

-- (#([0] ++ [1,2])) + 1
lengthListPlusOne :: PreExpr
lengthListPlusOne = BinOp natSum 
                          (Paren $ UnOp listLength $ Paren listZeroConcatOneTwo) 
                          one

-- Dadas tres preExpresiones, la primera de la forma (Var _), generamos la 
-- la preExpresion de la forma 〈∀ v : pe : pe'〉
makeForAll :: PreExpr -> PreExpr -> PreExpr -> PreExpr
makeForAll (Var v) pe pe' = Quant folForall v pe pe'

-- Dadas tres preExpresiones, la primera de la forma (Var _), generamos la 
-- la preExpresion de la forma 〈∃ v : pe : pe'〉
makeExist :: PreExpr -> PreExpr -> PreExpr -> PreExpr
makeExist (Var v) pe pe' = Quant folExist  v pe pe'

makeDrop :: PreExpr -> PreExpr -> PreExpr
makeDrop pe pe' = BinOp listDrop pe pe'

-- x = F@x
equ0 :: PreExpr
equ0 = BinOp folEqual (Var x) fAppx

-- xs↓1 = ys↓1
equ1 :: PreExpr
equ1 = BinOp folEqual (makeDrop (Var xs) one) (makeDrop (Var ys) one)

-- 〈∀ x : x = F@x: xs↓1 = ys↓1〉
forAll0 :: PreExpr
forAll0 = makeForAll (Var x) equ0 equ1

exist0 :: PreExpr
exist0 = makeExist (Var x) equ0 equ1
