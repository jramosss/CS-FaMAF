
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
-- Errores que pueden ocurrir en la verificación de corrección de una
-- función respecto de su especificación.
-- 
----------------------------------------------------------------------------
module Fun.Verification.Error where

import Equ.PreExpr
import Equ.Expr

type ErrInVerif d = ([VerificationError],d)

-- | Errores sobre las derivaciones.
data VerificationError = InvalidStartOfProof PreExpr PreExpr
                       | InvalidEndOfProof PreExpr PreExpr
                       | MissingSpecHypInProof Expr
    deriving Eq

instance Show VerificationError where
    show (InvalidStartOfProof eInF eInp) = 
            "La expresión incial " ++ show eInp ++ 
            " de la prueba no se corresponde la función " ++ show eInF
    show (InvalidEndOfProof eInF eInp) = 
            "La expresión final, " ++ show eInp ++ 
            " de la prueba no se corresponde con la definición de la función " 
            ++ show eInF
    show (MissingSpecHypInProof hyp) =
            "La prueba no contiene la hipotesis " ++ show hyp
