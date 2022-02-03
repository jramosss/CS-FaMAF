
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
-- Errores posibles para un módulo.
-- 
----------------------------------------------------------------------------
module Fun.Module.Error where

import Fun.Verification
import Fun.Decl.Error
import Fun.Parser
import Fun.Derivation

import Fun.Decl (SpecDecl, FunDecl, ValDecl, ThmDecl, DerivDecl)

import Data.Text (Text,unpack)

data InModuleError = InModuleError {
                          mName :: Text
                        , mErrSpecs :: [ErrInDecl SpecDecl]
                        , mErrFuns  :: [ErrInDecl FunDecl]
                        , mErrVals  :: [ErrInDecl ValDecl]
                        , mErrThm   :: [ErrInDecl ThmDecl]
                        , mErrVer   :: [ErrInVerif Verification]
                        , mErrDeriv :: [ErrInDeriv DerivDecl]
                        , mErrTC    :: String
                        }

data ModuleError = ModuleParseError TextFilePath ParseError -- ^ Error de parseo
                 | ModuleErrorFileDoesntExist Text          -- ^ No existe el archivo
                 | ModuleCycleImport [Text]                 -- ^ Imports circulares
                 | ModuleError InModuleError

instance Show ModuleError where
    show (ModuleParseError fp perr) = "Error de parseo en " ++ show (unpack fp) 
                                    ++ ":\n" ++ show perr
    show (ModuleErrorFileDoesntExist t) = 
        "No existe el archivo correspondiente a este nombre de módulo: " ++ unpack t
    show (ModuleCycleImport []) = "Error imposible!"
    show (ModuleCycleImport (mn:mns)) = 
        unlines [ "Import ciclico:\n\tbegin in -> " ++ show (unpack mn)
                , foldr (\mn' s -> s ++ "\n\timport -> " ++ show (unpack mn')) "" (init mns)
                , "\tend in -> " ++ show (last mns)
                ]
    show (ModuleError m) = unlines [ "\n=============ErrorsInModule=========="
                                   , "Modulo: " ++ show (mName m)
                                   , "Specs con error: " ++  show (mErrSpecs m)
                                   , "Funs con error: " ++  show (mErrFuns m)
                                   , "Vals con error: " ++  show (mErrVals m)
                                   , "Thms con error: " ++  show (mErrThm m)
                                   , "Vers con error: " ++  show (mErrVer m)
                                   , "Ders con error : " ++ show (mErrDeriv m)
                                   , "Errores en el type-checking : " ++ mErrTC m
                                   , "===================================="
                                   ]

createError :: Text -> [ErrInDecl SpecDecl] -> [ErrInDecl FunDecl] ->
                [ErrInDecl ValDecl] -> [ErrInDecl ThmDecl] ->
                [ErrInVerif Verification] -> [ErrInDeriv DerivDecl] -> String  ->
                ModuleError
createError name errSpecs errFuns errVals errThm errVer errDeriv tcErr = ModuleError $ InModuleError name errSpecs errFuns errVals errThm errVer errDeriv tcErr
