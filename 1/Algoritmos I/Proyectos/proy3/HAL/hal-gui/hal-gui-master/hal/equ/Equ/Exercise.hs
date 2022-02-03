{-# Language TypeSynonymInstances #-}
   -- | Este m&#243;dulo define la noci&#243;n de un ejercicio en equ.
module Equ.Exercise where

import Equ.Exercise.Conf (TypeCheck, ExerciseConf, createExerciseConf)

import Equ.Expr
import Equ.Proof.Annot
import qualified Equ.Rule as R
import Equ.Proof

import Data.Text hiding (null)
import qualified Data.Map as M 

import Control.Applicative ((<$>), (<*>))
import Data.Serialize (Serialize, get, getWord8, put, putWord8)

-- Enunciado del ejercicio.
data Statement = Statement { title :: Text
                           , stat :: Text
                           , initExpr :: Expr
                           , hints :: Text
                           } deriving Show

instance Serialize Statement where
    put (Statement title stat initExpr hints) = 
        put title >> put stat >> put initExpr >> put hints
    
    get = Statement <$> get <*> get <*> get <*> get

-- Representacion del ejercicio en equ.
data Exercise = Exercise { exerConf :: ExerciseConf
                         , exerStatement :: Statement
                         , exerProof :: Maybe Proof
                         , exerAnnots :: Maybe ProofAnnotation
                         }

instance Serialize Exercise where
    put (Exercise exerConf exerStat exerProof exerAnnots) =
        put exerConf >> put exerStat >> put exerProof >> put exerAnnots
        
    get = Exercise <$> get <*> get <*> get <*> get

instance Show Exercise where
    show exer = show (exerConf exer) ++ " " ++
                show (exerStatement exer) ++ " " ++
                show (exerProof exer)

-- | Genera un enunciado vacio. Todos los campos de texto en blanco y la
-- expresi´on inicial es la correspondiente expresi´on inicial de la prueba.
createStatement :: Expr -> Statement
createStatement e = Statement empty empty e empty

-- | Crea un ejercicio a partir de una configuraci´on y un enunciado.
-- En el cual la prueba es un hueco con el contexto y relaci´on propio de la
-- configuraci´on del ejercicio.
createExercise :: Expr -> Maybe ProofAnnotation -> Exercise
createExercise e mpa = Exercise exerConf stmnt Nothing mpa
    where
        exerConf :: ExerciseConf
        exerConf = createExerciseConf
        stmnt :: Statement
        stmnt = createStatement e
