-- | Las expresiones son pre-expresiones bien tipadas. La verificación
-- del tipo de la expresion y sus terminos se lleva adelante por el
-- algoritmo principal del modulo 'Typechecker'. El algoritmo de
-- unificación que es central para la construcción y verificación de
-- demostraciones sobre expresiones (instancias del tipo de datos
-- Proof), asumen que las expresiones manipuladas están bien
-- tipadas.
module Equ.Expr where
import Equ.PreExpr(PreExpr,holePreExpr)
import Data.Serialize(Serialize, get, put)

import Control.Applicative ((<$>))
import Test.QuickCheck(Arbitrary, arbitrary)

-- | Las expresiones son pre-expresiones bien tipadas. Es decir,
-- ning&#250;n constituyente de una expresi&#243;n puede tener TyUnknown como
-- tipo.
newtype Expr = Expr PreExpr

instance Show Expr where 
    show (Expr e) = show e

instance Eq Expr where
    (Expr e1) == (Expr e2) = e1 == e2

instance Arbitrary Expr where
    arbitrary = Expr <$> arbitrary

instance Serialize Expr where
    put (Expr e) = put e
    get = Expr <$> get

-- | Retorna la preExpresi&#243;n que constituye la expresi&#243;n.
getPreExpr :: Expr -> PreExpr
getPreExpr (Expr e) = e

holeExpr :: Expr
holeExpr = Expr holePreExpr
