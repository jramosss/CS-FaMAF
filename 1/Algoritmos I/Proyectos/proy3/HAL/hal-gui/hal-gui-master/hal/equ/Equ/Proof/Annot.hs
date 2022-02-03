{-# Language FlexibleInstances,TypeSynonymInstances #-}
-- | Este m´odulo define la noci´on de las anotaciones para una prueba en equ.
module Equ.Proof.Annot where

import Equ.Expr
import Equ.Proof.Proof
import Equ.Proof.Zipper (ProofFocus',toProofFocus)
import Equ.Proof.ListedProof
import qualified Equ.Rule as R

import Data.Text hiding (null)
import qualified Data.Map as M 

import Control.Applicative ((<$>), (<*>))
import Data.Serialize (Serialize, get, getWord8, put, putWord8)

-- | Anotacin para una expresi´on.
type Annotation = Text

-- | Conjunto de anotaciones para una prueba.
type ProofAnnotation = Proof' () () () Annotation

type ProofFocusAnnots = ProofFocus' () () () Annotation

type ListedAnnots = ListedProof' () () () Annotation

instance Show ProofAnnotation where
    show (Hole _ _ a a') = "Hole " ++ show a ++ " " ++ show a'
    show (Simple _ _ a a' _) = "Simple " ++ show a ++ " " ++ show a'
    show (Trans _ _ a a' a'' p p') = "Trans " ++ show a ++ " " ++ show a' ++ " " ++
                                     show a'' ++ " " ++ show p ++ " " ++ show p'
    show _ = ""

-- | Una prueba vacia de anotaciones.
emptyProofAnnots :: ProofFocusAnnots
emptyProofAnnots = toProofFocus $ Hole () () empty empty

-- | Agrega un paso vacio para una prueba de anotaciones.
addEmptyStepAnnots :: ProofFocusAnnots -> ProofFocusAnnots
addEmptyStepAnnots (p@(Hole _ _ a a'),path) = 
    (Trans () () a empty a' (Hole () () a empty) (Hole () () empty a'),path)
-- Si le pasamos una prueba simple, la considera un hueco
addEmptyStepAnnots (p@(Simple _ _ a a' _),path) = 
    (Trans () () a empty a' (Hole () () a empty) (Hole () () empty a'),path)
addEmptyStepAnnots p = p

instance Show ListedAnnots where
    show lProof = show (pList lProof) ++ " | Index: " ++ show (selIndex lProof)
