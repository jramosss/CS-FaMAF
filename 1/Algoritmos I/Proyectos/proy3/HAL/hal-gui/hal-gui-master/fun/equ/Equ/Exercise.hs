{-# Language TypeSynonymInstances #-}
   -- | Este m&#243;dulo define la noci&#243;n de un ejercicio en equ.
module Equ.Exercise where

import Equ.Exercise.Conf (ExerciseConf, createExerciseConf)

import Equ.Expr
import Equ.Proof.Annot
import Equ.Proof

import Data.Text hiding (null)

import Control.Applicative ((<$>), (<*>))
import Data.Serialize (Serialize, get, put)

-- Enunciado del ejercicio.
data Statement = Statement { title :: Text
                           , stat :: Text
                           , initExpr :: Expr
                           , hints :: Text
                           } deriving Show

instance Serialize Statement where
    put (Statement t s i h) = 
        put t >> put s >> put i >> put h
    
    get = Statement <$> get <*> get <*> get <*> get

-- Representacion del ejercicio en equ.
data Exercise = Exercise { exerConf :: ExerciseConf
                         , exerStatement :: Statement
                         , exerProof :: Maybe Proof
                         , exerAnnots :: Maybe ProofAnnotation
                         }

instance Serialize Exercise where
    put (Exercise conf st prf ann) =
        put conf >> put st >> put prf >> put ann
        
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
createExercise e mpa = Exercise conf stmnt Nothing mpa
    where
        conf :: ExerciseConf
        conf = createExerciseConf
        stmnt :: Statement
        stmnt = createStatement e
