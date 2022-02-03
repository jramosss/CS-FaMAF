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
-- 
-- 
----------------------------------------------------------------------------
{-# Language OverloadedStrings #-}
module Fun.FunTheories.FOL(
       folTheory
     , bool
     ) where

import qualified Equ.Theories.FOL as EquFOL
import qualified Equ.Theories as EquTh
import Equ.PreExpr
import Equ.Proof (Theorem,Axiom)
import Equ.Types

import Equ.IndTypes (bool)
import Fun.Theory
import Fun.Decl

{- Lógica -}

varP,varQ :: Variable
varP = var "p" (TyAtom ATyBool)
varQ = var "q" (TyAtom ATyBool)


{- or p q = case p of
              False -> q
              True  -> True
-}

folOrExpr :: PreExpr
folOrExpr = Case (Var varP)
             [ (Con EquFOL.folFalse, Var varQ)
             , (Con EquFOL.folTrue,Con EquFOL.folTrue)
             ]

folOr :: OpDecl 
folOr = OpDecl EquFOL.folOr [varP,varQ] folOrExpr
                           
{- and p q = case n of
                 False -> Flase
                 True -> q
                 -}
folAndExpr :: PreExpr
folAndExpr = Case (Var varP) [ (Con EquFOL.folFalse, Con EquFOL.folFalse)
                             , (Con EquFOL.folTrue,Con EquFOL.folTrue)
                              ]

folAnd :: OpDecl
folAnd = OpDecl EquFOL.folAnd [varP,varQ] folAndExpr

boolQuantifiers :: [Quantifier]
boolQuantifiers = EquFOL.theoryQuantifiersList

boolAxioms :: [Axiom]
boolAxioms = EquTh.listAxioms

boolTheorems :: [Theorem]
boolTheorems = []

folTheory :: Theory
folTheory = Theory {
            tname = "Lógica"
          , indType = [bool]
          , operators = [folOr, folAnd]
          , quantifiers = boolQuantifiers
          , axioms = boolAxioms
          , theorytheorems = boolTheorems
          }
