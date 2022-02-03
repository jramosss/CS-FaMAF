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
-- Definimos la noción de módulo de fun.
-- 
----------------------------------------------------------------------------
{-# Language TemplateHaskell #-}

module Fun.Module where

import Fun.Verification
import Fun.Declarations
import Fun.Derivation

import Control.Lens

import Data.Text (Text)
import Data.Function

type ModName = Text

data InvalidDeclsAndVerifs =  
        InvalidDeclsAndVerifs { decls  :: InvalidDeclarations
                              , verifs :: [ErrInVerif Verification]
                              }
    deriving Show

emptyInDeclsVerifs :: InvalidDeclsAndVerifs
emptyInDeclsVerifs = InvalidDeclsAndVerifs emptyInDecls []


data Import = Import ModName
    deriving (Eq, Show)

                            
data Module = Module { _modName       :: ModName
                     , _imports       :: [Import]
                     , _validDecls    :: Declarations
                     , _invalidDecls  :: InvalidDeclsAndVerifs
                     , _verifications :: [Verification]
                     , _derivations   :: [Derivation]
                     }


$(makeLenses ''Module)

imName :: Lens' Import ModName
imName = lens (\(Import m) -> m) (\_ -> Import ) 

instance Eq Module where
    (==) = (==) `on` (^. modName)

instance Show Module where
    show m = unlines [ ""
                     , "========LoadModule========="
                     , "ModName: " ++ show (_modName m)
                     , "Imports: " ++ show (_imports m)
                     , ""
                     , "Decls: " ++ show (_validDecls m)
                     , ""
                     , "Verifications : " ++ show (_verifications m)
                     , "Derivations : " ++ show (_derivations m)
                     , "Invalid Decls : " ++ show (_invalidDecls m)
                     , "=========================="
                     ]


allDeclsValid :: Module -> Bool
allDeclsValid m = 
    let invd = decls (_invalidDecls m) in
        inSpecs invd == [] && inFunctions invd == [] &&
        inTheorems invd == [] && inProps invd == [] &&
        inVals invd == [] && inVals invd == [] &&
        inDerivs invd == [] &&
        verifs (_invalidDecls m) == []
