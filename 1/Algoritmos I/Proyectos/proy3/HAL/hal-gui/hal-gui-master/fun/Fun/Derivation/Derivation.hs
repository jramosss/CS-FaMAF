
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
-- Declaración del tipo de datos correspondiente a la derivación de un
-- programa a partir de su especificación.
-- 
----------------------------------------------------------------------------
module Fun.Derivation.Derivation where

import Fun.Decl
import Fun.Derivation.Error
import Fun.Decl.Error

import Data.Monoid

import Control.Lens

-- | Una derivación contiene una especificación, un programa y la prueba
--   de que ambos son equivalentes.
data Derivation = Derivation { deriv :: DerivDecl
                             , derivPos :: DeclPos
                             , spec :: SpecDecl
                             , prog :: Maybe FunDecl
                             }
    deriving Eq


derivDer :: Lens' Derivation DerivDecl
derivDer = lens g s
    where g (Derivation der _ _ _) = der
          s drv der = drv {deriv = der }


instance Show Derivation where
    show d = unlines [ "Derivación\nSpec: " ++ show (spec d) 
                     , "Deriv: " ++ show (deriv d)
                     , "Prog: " ++ show (prog d)
                     ]

type EDeriv' a = Either ([DerivationError], DerivDecl) a
type EDeriv = EDeriv' Derivation
             
whenDer :: Bool -> ([DerivationError],DerivDecl) -> EDeriv' ()
whenDer b e | b = return ()
            | otherwise = Left e

instance (Monoid m) => Monoid (Either a m) where
  mempty = Right mempty
  mappend (Left e) _ = Left e
  mappend _ (Left e) = Left e
  mappend (Right a) (Right a') = Right (a `mappend` a')
