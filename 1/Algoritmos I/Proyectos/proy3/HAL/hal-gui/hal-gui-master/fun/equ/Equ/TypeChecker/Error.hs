----------------------------------------------------------------------------
-- |
-- Module      :  $Header$
-- Copyright   :  (c) Proyecto Theona, 2012-2013
--                (c) Alejandro Gadea, Emmanuel Gunther, Miguel Pagano
-- License     :  <license>
--
-- Maintainer  :  miguel.pagano+theona@gmail.com
-- Stability   :  experimental
-- Portability :  portable
--
-- Definición de los erores del type-checker.
--
----------------------------------------------------------------------------
{-# Language ExistentialQuantification #-}
module Equ.TypeChecker.Error where
import Equ.Syntax
import Equ.Types

import Data.Text(unpack)

-- | Errores de type-checking.
data TyErr = ErrNotExpected Type Type -- ^ El tipo inferido/obtenido (primer
                                      -- argumento) no es el mismo que el 
                                      -- esperado (segundo argumento).

           -- | Una variable tiene un tipo distinto al asignado por el
           -- contexto.
           | forall s . Syntactic s => ErrClashTypes s [Type]
           | forall s . Syntactic s => ErrNoType s
           | ErrUnification Type Type
           | ErrMatching Type Type
           | ErrMerge
           | ErrNotFunType Type -- ^ Se esperaba un tipo funcional
           | ErrArity Type Int -- ^ Se aplicaron menos o más argumentos que los esperados

instance Eq TyErr where
    (ErrNotExpected t t') == (ErrNotExpected t'' t''') = t == t'' && t' == t'''
    (ErrClashTypes _ l) == (ErrClashTypes _ l') = l == l'
    (ErrUnification t t') == (ErrUnification t'' t''') = t == t'' && t' == t'''
    _ == _ = False

instance Show TyErr where
    show (ErrNotExpected t t') = "Se esperaba el tipo " ++ show t ++ " y no el tipo " ++ show t'
    show (ErrClashTypes s ts) = unpack (tRepr s) ++ " sólo puede tener un tipo" ++ show ts
    show (ErrNoType s) = "No hay información de tipo para: " ++ unpack (tRepr s) 
    show (ErrUnification t t') = "Los tipos " ++ show t ++ " y " ++ show t' ++ " no se pueden unificar"
    show (ErrMatching t t') = "El tipo " ++ show t ++ " no matchea con " ++ show t'
    show ErrMerge = "No se pudieron combinar dos sustituciones"
    show (ErrNotFunType t) = "Se esperaba un tipo funcional, pero el tipo es: " ++ show t
    show (ErrArity t n) = "El tipo " ++ show t ++ "espera una cantidad distinta de argumentos, que la provista (" ++ show n ++ ")"
