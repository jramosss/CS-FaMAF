
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
module Fun.FunTheories.Arith(
       arithTheory
     , natural
     ) where

import qualified Equ.Theories.Arith as EquArith
import qualified Equ.Theories as EquTh
import Equ.Proof(Axiom,Theorem)
import Equ.PreExpr
import Equ.Types

import Equ.IndTypes(natural)
import Fun.Theory
import Fun.Decl


{- ARITMETICA -}
varN,varM,varT :: Variable
varN = var "n" (TyAtom ATyNat)
varM = var "m" (TyAtom ATyNat)
varT = var "t" (TyAtom ATyNat)


{- sum n m = case n of
                0 -> m
                (succ t) -> succ (sum n m)
-}

natSumExpr :: PreExpr
natSumExpr = Case (Var varN) 
             [ (Con EquArith.natZero, Var varM)
             , (UnOp EquArith.natSucc (Var varT),
                     UnOp EquArith.natSucc (BinOp EquArith.natSum (Var varT) (Var varM)))
             ]

natSum :: OpDecl 
natSum = OpDecl EquArith.natSum [varN,varM] natSumExpr
                           
{- prod n m = case n of
                 0 -> 0
                 1 -> m
                 (succ t) -> sum t (prod t m)
                 -}
natProdExpr :: PreExpr
natProdExpr = Case (Var varN) [ (Con EquArith.natZero,
                           Con EquArith.natZero)
                        , (UnOp EquArith.natSucc (Con EquArith.natZero),
                           Var varM)
                        , (UnOp EquArith.natSucc (Var varT),
                           BinOp EquArith.natSum (Var varT) (BinOp EquArith.natProd (Var varT) (Var varM)))
                       ]

natProd :: OpDecl
natProd = OpDecl EquArith.natProd [varN,varM] natProdExpr

arithOperators :: [OpDecl]
arithOperators = [natSum,natProd]

arithQuantifiers :: [Quantifier]
arithQuantifiers = EquArith.theoryQuantifiersList

arithAxioms :: [Axiom]
arithAxioms = EquTh.arithAxioms

arithTheorems :: [Theorem]
arithTheorems = []

arithTheory :: Theory
arithTheory = Theory {
              tname = "Aritmética"
            , indType = [natural]
            , operators = arithOperators
            , quantifiers = arithQuantifiers
            , axioms = arithAxioms
            , theorytheorems = arithTheorems
            }
            
            
            
