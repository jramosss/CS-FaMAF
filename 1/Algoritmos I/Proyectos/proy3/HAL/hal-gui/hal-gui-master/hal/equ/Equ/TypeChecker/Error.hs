-- | Definición de los erores del type-checker.
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
           | ErrUnification Type Type [(TyVarName,Type)]


instance Eq TyErr where
    (ErrNotExpected t t') == (ErrNotExpected t'' t''') = t == t'' && t' == t'''
    (ErrClashTypes _ l) == (ErrClashTypes _ l') = l == l'
    (ErrUnification t t' s) == (ErrUnification t'' t''' s') = t == t''   &&
                                                              t' == t''' &&
                                                              s == s'
    _ == _ = False

instance Show TyErr where
    show (ErrNotExpected t t') = "Se esperaba el tipo " ++ show t ++ " y no el tipo " ++ show t'
    show (ErrClashTypes s ts) = unpack (tRepr s) ++ " sólo puede tener un tipo" ++ show ts
    show (ErrNoType s) = "No hay información de tipo para: " ++ unpack (tRepr s) 
    show (ErrUnification t t' s) = "Los tipos de " ++ show s ++ ": " ++ show t ++ " y " ++ show t' ++ " no se pueden unificar"
