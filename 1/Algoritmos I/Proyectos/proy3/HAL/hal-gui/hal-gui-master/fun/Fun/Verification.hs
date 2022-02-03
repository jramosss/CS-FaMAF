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
-- Define la noción de corrección para una derivación.
-- 
----------------------------------------------------------------------------
module Fun.Verification (
      Verification (..)
    , module Fun.Verification.Error
    , createVerifications
    , checkVerification
    )
    where

import Fun.Decl
import Fun.Declarations
import Fun.Verification.Verification
import Fun.Verification.Error

import Equ.Proof
import qualified Equ.PreExpr as PE

import Data.List as L (map, find)
import Data.Maybe (fromJust,catMaybes)

-- | Crea una verificación, esto es, una especificación de una función,
-- la función y una prueba de corrección de que la función se derivo en
-- base a la especificación.
createVerifications:: Declarations -> Declarations -> [Verification]
createVerifications decls _ = do
                let vSpecs = bare specs decls
                let vFuns = bare functions decls
                let vThm = bare theorems decls
                let der = createVer vSpecs vFuns vThm
                catMaybes der
    where
        createVer :: [SpecDecl] -> [FunDecl] -> [ThmDecl] -> [Maybe Verification]
        createVer spcs funcs thms = 
            L.map (\s -> L.find (equalFun s) funcs >>= \f ->
                         L.find (equalThm f) thms >>= \(Thm theo _) ->
                        Just (Verification s f (thProof theo))) spcs
        equalFun :: SpecDecl -> FunDecl -> Bool
        equalFun s f = getFuncDecl s == getFuncDecl f &&
                       getVarsDecl s == getVarsDecl f
        equalThm :: FunDecl -> ThmDecl -> Bool
        equalThm f t = (Just $ getNameDecl t) == getFunDerivingFrom f

-- | Funcion que dada una derivacion dice si es válida o no.
checkVerification :: Verification -> 
                     Either ([VerificationError], Verification) Verification 
checkVerification d = 
            case (checkStartExpr, checkEndExpr) of
                ([],[]) -> return d
                (sErr,eErr) -> Left (sErr ++ eErr, d)
    where
        prf :: Proof
        prf = proof d
        prg :: PE.PreExpr
        prg = fromJust . getExprDecl $ prog d
        checkStartExpr :: [VerificationError]
        checkStartExpr = do
                let Fun f vs _ _ = prog d
                let fWithArgs = foldl PE.preExprApp (PE.Var f) (map PE.Var vs)
                let Right ei = getStart prf
                if fWithArgs == PE.toExpr ei 
                   then []
                   else [InvalidStartOfProof fWithArgs (PE.toExpr ei)]
        checkEndExpr :: [VerificationError]
        checkEndExpr = do
                let Right ef = getEnd prf
                if prg == PE.toExpr ef
                   then []
                   else [InvalidEndOfProof prg (PE.toExpr ef)]
        
