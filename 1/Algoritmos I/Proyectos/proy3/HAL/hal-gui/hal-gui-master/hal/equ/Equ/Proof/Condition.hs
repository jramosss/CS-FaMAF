module Equ.Proof.Condition where

import Data.Serialize(Serialize, get, getWord8, put, putWord8, encode, decode)
import Control.Applicative ((<$>), (<*>))
import Test.QuickCheck
import Equ.PreExpr
import Equ.Syntax

-- | Para aplicar un paso de prueba, puede ser necesario validar algunas condiciones.
--   GenConditions son condiciones sobre la aplicación de una regla, en la cual se realiza
--   la reescritura normalmente, y luego se chequea alguna condición extra. Pueden ser varias.
--   SpecialCondition es una condición especial para algún axioma, en el cual el proceso de 
--   aplicación de la regla es diferente al general.
data Condition = GenConditions [GCondition]
    deriving (Eq,Show)    
 
-- |GCondition son condiciones que se chequean al aplicar un axioma luego de 
--   realizar la reescritura.
data GCondition = VarNotInExpr Variable PreExpr -- ^ La variable no ocurre en la expresion
                 | NotEmptyRange PreExpr -- ^ El Rango de cuantificación es no vacío (distinto de False).
                 | InductiveHypothesis PreExpr -- ^ En la hipótesis inductiva, la reescritura no se hace con cualquier variable
                                              -- sino que tiene que ser exactamente con la misma variable del pattern.
                 | ReplacedExpr PreExpr PreExpr Variable PreExpr -- ^ ReplacedExpr p q v r chequea que p es igual a q donde se reemplaza
                                                                 -- v por r
    deriving (Eq,Show)
 
instance Arbitrary Condition where
    arbitrary = oneof [ GenConditions <$> arbitrary
                       ]
                   
instance Serialize Condition where
    put (GenConditions lc) = putWord8 0 >> put lc
    
    get = getWord8 >>= \tag ->
        case tag of
             0 -> GenConditions <$> get
                   
                   
instance Arbitrary GCondition where
    arbitrary = oneof [ VarNotInExpr <$> arbitrary <*> arbitrary
                      , NotEmptyRange <$> arbitrary
                      , InductiveHypothesis <$> arbitrary
                      , ReplacedExpr <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
                      ]
                   
instance Serialize GCondition where
    put (VarNotInExpr v p) = putWord8 0 >> put v >> put p
    put (NotEmptyRange p) = putWord8 1 >> put p
    put (InductiveHypothesis v)= putWord8 2 >> put v
    put (ReplacedExpr p q v t) = putWord8 3 >> put p >> put q >> put v >> put t
    
    get = getWord8 >>= \tag_ ->
        case tag_ of
             0 -> VarNotInExpr <$> get <*> get
             1 -> NotEmptyRange <$> get
             2 -> InductiveHypothesis <$> get
             3 -> ReplacedExpr <$> get <*> get <*> get <*> get
                      