
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
-- Una teoria en Fun agrupa todo lo referido a un tipo de dato
-- inductivo, conteniendo los operadores y las funciones definidas
-- para ese tipo, y los axiomas y teoremas que permitiran hacer
-- transiciones de programas en los que esté involucrado el tipo.
-- 
----------------------------------------------------------------------------

module Fun.Theory where

import Fun.Decl

import Equ.PreExpr
import Equ.Proof
import Equ.IndType

import Data.Text

-- | Teoría correspondiente a un tipo inductivo.
data Theory = Theory {
               tname :: Text
             , indType :: [IndType] 
             , operators :: [OpDecl] -- En esta lista no se incluye a los constructores.
             , quantifiers :: [Quantifier]
             , axioms :: [Axiom]
             , theorytheorems :: [Theorem]
            }

            
            
