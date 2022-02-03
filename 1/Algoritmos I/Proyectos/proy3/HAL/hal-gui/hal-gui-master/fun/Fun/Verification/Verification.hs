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
-- Tipo de datos para verificaciones de que una función satisface su
-- especificación.
-- 
----------------------------------------------------------------------------
module Fun.Verification.Verification where

import Fun.Decl

import Equ.Proof

-- | Una derivación contiene una especificación, un programa y la prueba
--   de que ambos son equivalentes.
data Verification = Verification { spec  :: SpecDecl
                                 , prog  :: FunDecl
                                 , proof :: Proof
                                 }
    deriving Eq

instance Show Verification where
    show d = "\n\nVerif\nSpec: " ++ show (spec d) ++
             "\nProg: " ++ show (prog d) 
