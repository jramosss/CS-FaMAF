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
module Fun.FunTheories.List(
       listTheory
     , list
     ) where

import qualified Equ.Theories.Arith as EquArith
import qualified Equ.Theories.List as EquList
import qualified Equ.Theories as EquTh
import Equ.PreExpr
import Equ.Types
import Equ.Proof (Theorem,Axiom)


import Equ.IndTypes(list)
import Fun.Theory
import Fun.FunTheories.Arith
import Fun.Decl

varN,varT,varX,varXS,varYS :: Variable
varN = var "n" (TyAtom ATyNat)
varT = var "t" (TyAtom ATyNat)
varX = var "x" (TyVar "A")
varXS = var "xs" (TyList $ TyVar "A")
varYS = var "ys" (TyList $ TyVar "A")
              
              
              
{- index n ys = case xs of
                    (x:xs) -> case n of
                                0 -> x
                                (succ t) -> index t xs
                        -}
listIndex :: OpDecl 
listIndex = OpDecl EquList.listIndex [varN,varYS] exprListIndex
    where 
        exprListIndex :: PreExpr
        exprListIndex = Case (Var varYS) 
                                [ (BinOp EquList.listApp (Var varX) (Var varXS)
                                , exprListIndex')]
                    
        exprListIndex' :: PreExpr
        exprListIndex' = Case (Var varN) 
                                [(Con EquArith.natZero, Var varX)
                            , (UnOp EquArith.natSucc (Var varT),
                                BinOp EquList.listIndex (Var varT) (Var varXS))
                            ]


listLength :: OpDecl
listLength = OpDecl EquList.listLength [varYS] lenFun
    where lenFun :: PreExpr
          lenFun = Case (Var varYS)
                   [ (Con EquList.listEmpty,Con EquArith.natZero)
                   , (cons (Var varX) (Var varXS),suc (UnOp EquList.listLength (Var varXS)))
                   ]          

          suc = UnOp EquArith.natSucc
          cons = BinOp EquList.listApp


{- concat xs ys = case xs of
                      [] -> ys
                      (x:xs) -> x:(concat xs ys)
                      -}
                            
-- listConcat :: (Operator,[Variable],PreExpr)
-- listConcat = (EquList.listConcat,

-- FALTA DEFINIR EL RESTO DE LOS OPERADORES.
                            

listQuantifiers :: [Quantifier]                
listQuantifiers = EquList.theoryQuantifiersList

listAxioms :: [Axiom]
listAxioms = EquTh.listAxioms

listTheorems :: [Theorem]
listTheorems = []

listTheory :: Theory
listTheory = Theory {
             tname = "Listas"
           , indType = [list,natural]
           , operators = [listLength,listIndex]
           , quantifiers = listQuantifiers
           , axioms = listAxioms
           , theorytheorems = listTheorems
           }
           
