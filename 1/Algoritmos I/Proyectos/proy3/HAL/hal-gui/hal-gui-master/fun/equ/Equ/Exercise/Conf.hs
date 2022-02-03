module Equ.Exercise.Conf where

import Equ.Theories (Grouped)
import Equ.Proof hiding (Simple, Focus, Cases)

import qualified Data.Set as S (Set, empty)

import Control.Applicative ((<$>), (<*>))
import Data.Serialize (Serialize, get, getWord8, put, putWord8)

{- Auto: se hace automáticamente al parsear
   Manual: no se infiere nada, se hace a mano.
   Infer: se puede usar el botón “Inferir” en la caja del árbol.
-}
data TypeCheck = Auto 
               | Manual 
               | Infer
    deriving (Enum, Eq)

instance Show TypeCheck where
    show Auto   = "Inferencia de tipos al parsear"
    show Manual = "Ingreso de tipos por estudiante"
    show Infer  = "Inferencia de tipos después de parsear"

instance Serialize TypeCheck where
    put = putWord8 . toEnum . fromEnum
    
    get = getWord8 >>= \tag ->
          if tag < 3 then return . toEnum . fromEnum $ tag
          else fail $ "SerializeErr TypeCheck " ++ show tag

-- Tipo de re-escritura.
data RewriteMode = Simple -- ^ Directa.
                 | List     -- ^ Se puede ver la lista de resultados.
                 | Focus -- ^ Se debe decir dónde se debe aplicar la regla.
    deriving (Enum, Eq)

instance Show RewriteMode where
    show Simple = "Se usa la regla para comprobar un paso"
    show List   = "Se puede ver la lista de resultados al aplicar una regla"
    show Focus  = "Se debe indicar dónde aplicar la regla"

instance Serialize RewriteMode where
    put = putWord8 . toEnum . fromEnum

    get = getWord8 >>= \tag ->
          if tag < 3 then return . toEnum . fromEnum $ tag
          else fail $ "SerializeErr RewriteMode " ++ show tag

-- Tipo de prueba.
data TypeProof = Direct -- ^ Prueba directa.
               | Cases  -- ^ Prueba por casos.
               | Induction -- ^ Prueba por inducción.
               | Deduction -- ^ Usando metateorema de la deducción.
    deriving (Enum, Eq)

instance Show TypeProof where
    show Direct = "Prueba directa"
    show Cases = "Prueba distinguiendo casos"
    show Induction = "Prueba por inducción"
    show Deduction = "Prueba con el meta-teorema de la deducción"

instance Serialize TypeProof where
    put = putWord8 . toEnum . fromEnum

    get = getWord8 >>= \tag ->
          if tag < 3 then return . toEnum . fromEnum $ tag
          else fail $ "SerializeErr TypeProof " ++ show tag

data Explicit = Initial 
              | Relation 
              | Final
    deriving (Show, Eq, Ord, Enum)



instance Serialize Explicit where
    put = putWord8 . toEnum . fromEnum

    get = getWord8 >>= \tag ->
          if tag < 3 then return . toEnum . fromEnum $ tag
          else fail $ "SerializeErr Explicit " ++ show tag

-- Conjunto de informacion a mostar relacionada con el objetivo del ejercicio.
type ExplicitInfo = S.Set Explicit

-- Configuracion de un ejercicio.
data ExerciseConf = ExerciseConf { eConfTypeProof :: TypeProof
                                 , eConfExplicit :: ExplicitInfo
                                 , eConfRewriteMode :: RewriteMode
                                 , eConfTypeCheck :: TypeCheck
                                 , eConfAvaibleTheories :: Grouped Axiom
                                 }

instance Show ExerciseConf where
    show exerConf = show (eConfTypeProof exerConf) ++ " " ++ 
                    show (eConfRewriteMode exerConf) ++ " " ++
                    show (eConfTypeCheck exerConf) ++ " " ++
                    show (eConfExplicit exerConf)  ++ " " ++
                    show (map (fst) $ eConfAvaibleTheories exerConf)

instance Serialize ExerciseConf where
    put (ExerciseConf tp ei rw tc ga) = put tp >> put ei >> put rw >>
                                        put tc >> put ga

    get = ExerciseConf <$> get <*> get <*> get <*> get <*> get

createExerciseConf :: ExerciseConf
createExerciseConf = ExerciseConf Direct S.empty Simple Auto []

typeCheckOptionList :: [TypeCheck]
typeCheckOptionList = [Auto, Manual, Infer]

rewriteModeOptionList :: [RewriteMode]
rewriteModeOptionList = [Simple, List, Focus]

typeProofOptionList :: [TypeProof]
typeProofOptionList = [Direct, Cases, Induction, Deduction]

explicitOptionList :: [Explicit]
explicitOptionList = [Initial, Relation, Final]
