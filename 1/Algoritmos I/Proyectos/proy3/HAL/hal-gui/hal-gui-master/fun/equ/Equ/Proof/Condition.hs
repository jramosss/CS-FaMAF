module Equ.Proof.Condition where

import Data.Serialize(Serialize, get, getWord8, put, putWord8)
import Control.Applicative ((<$>), (<*>))
import qualified Data.Set as Set
import qualified Data.Map as M
import Test.QuickCheck

import Equ.PreExpr
import qualified Equ.Theories.Common as C (folFalse)
import Equ.Matching(VariableRename)

-- | Para aplicar un paso de prueba, puede ser necesario validar algunas condiciones.
--   GenConditions son condiciones sobre la aplicación de una regla, en la cual se realiza
--   la reescritura normalmente, y luego se chequea alguna condición extra. Pueden ser varias.
--   SpecialCondition es una condición especial para algún axioma, en el cual el proceso de 
--   aplicación de la regla es diferente al general.
data Condition = GenConditions [GCondition]
    deriving (Eq,Show)    
 
-- | GCondition son condiciones que se chequean al aplicar un axioma
-- luego de realizar la reescritura.
data GCondition = VarNotInExpr Variable PreExpr -- ^ La variable no ocurre en la expresion
                 | NotEmptyRange PreExpr -- ^ El Rango de cuantificación es no vacío (distinto de False).
                 | InductiveHypothesis PreExpr -- ^ En la hipótesis inductiva, la reescritura no se hace con cualquier variable
                                              -- sino que tiene que ser exactamente con la misma variable del pattern.
                 | ReplacedExpr PreExpr PreExpr Variable PreExpr -- ^ ReplacedExpr p q v r chequea que p es igual a q donde se reemplaza
                                                                 -- v por r
    deriving (Eq,Show)
 
instance Arbitrary Condition where
    arbitrary = GenConditions <$> arbitrary
                   
instance Serialize Condition where
    put (GenConditions lc) = putWord8 0 >> put lc
    
    get = getWord8 >>= \tag ->
        case tag of
             0 -> GenConditions <$> get
             _ -> fail $ "Condition " ++ show tag

                   
                   
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
    
    get = getWord8 >>= \tag ->
        case tag of
             0 -> VarNotInExpr <$> get <*> get
             1 -> NotEmptyRange <$> get
             2 -> InductiveHypothesis <$> get
             3 -> ReplacedExpr <$> get <*> get <*> get <*> get
             _ -> fail $ "SerializeErr GCondition " ++ show tag

-- | Condicion vacía
noCondition :: Condition
noCondition = GenConditions []

-- | Dada una GCondition devuelve un predicado sobre pre-expresiones.
conditionFunction :: GCondition -> (ExprSubst,VariableRename) -> PreExpr -> Bool
conditionFunction (VarNotInExpr v p) (subst,_) _ = not $ Set.member v (freeVars $ applySubst p subst)
conditionFunction (InductiveHypothesis pattern) (subst,_) _ =
    -- En la hipótesis inductiva, solo podemos validar la reecritura si
    -- la expresión por la q se quiere reemplazar la variable inductiva
    -- es la misma. Ejemplo: si la HI es "x es par", solo podemos aplicarla
    -- a la expresión "x es par" y no a "(x+1) es par". Por eso pedimos que 
    -- la substitución de reescritura asigne x -> x.
    case pattern of
         Var v -> applySubst (Var v) subst == Var v
         _ -> False
conditionFunction (NotEmptyRange ptn) (subst,_) _ = applySubst ptn subst /= Con C.folFalse 
                          
{- q es igual a p donde reemplazamos x por n.
   Esta condicion se utiliza en cuantificadores. x es la variable cuantificada.
   Al chequear el reemplazo en expresiones, vemos el renombre que hizo el matching
   a la variable cuantificada. -}
conditionFunction (ReplacedExpr q p x n) (subst,rnm) _ =
            maybe False (\v -> 
            applySubst q subst == applySubst (applySubst p subst) 
                                        (M.singleton v (applySubst (n' rnm) subst))
            ) $ M.lookup x rnm
        where n' = applySubst n . M.map Var
            
getGenConditions :: Condition -> [GCondition]
getGenConditions (GenConditions lc) = lc
