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
-- Representación de errores para declaraciones.
-- 
----------------------------------------------------------------------------
module Fun.Decl.Error where

import Equ.Syntax
import Equ.PreExpr
import Equ.Proof

import Data.Text (Text,unpack)

import Text.Parsec.Pos

data DeclPos = DeclPos { begin :: SourcePos
                       , end   :: SourcePos
                       , moduleName :: Text
                       }
    deriving Eq

instance Show DeclPos where
    show = show . sourceLine . begin

data ErrInDecl d = ErrInDecl { ePos  :: DeclPos
                             , errs  :: [DeclError]
                             , eDecl :: d
                             } 
    deriving Eq
    
instance Show (ErrInDecl d) where
    show = unlines . map show . errs

-- | Errores sobre las declaraciones.
data DeclError = NotInScopeVar Variable
               | InvalidPrgDeclaration
               | InvalidProofForThm ProofError
               | DuplicateName Text
               | ArgDuplicated Text
               | InvalidExprForThm PreExpr PreExpr
    deriving Eq
    
instance Show DeclError where
    show (NotInScopeVar v) = "Declaración " ++ show v ++ " fuera de alcance."
    show InvalidPrgDeclaration = "La función no declara un programa valido."
    show (InvalidProofForThm perr) = "Prueba invalida: " ++ show perr ++ "."
    show (DuplicateName t) = "Nombre duplicado: " ++ unpack t ++ "."
    show (ArgDuplicated t) = "Argumento duplicado: " ++ unpack t ++ "."
    show (InvalidExprForThm e e') = "La expresión del teorema ("++show e ++") no se corresponde con la expresión probada (" ++show e' ++ ")."
