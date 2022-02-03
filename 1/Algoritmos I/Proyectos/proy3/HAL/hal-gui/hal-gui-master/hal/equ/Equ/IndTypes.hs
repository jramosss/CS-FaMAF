{-# Language OverloadedStrings #-}
module Equ.IndTypes (
      natural
    , list
    , bool
    , getIndType
    , constrList
    )
    where

import Equ.IndType
import Equ.Types
import Equ.PreExpr.Symbols(tyListVar)
import qualified Equ.Theories.Arith as EquArith
import qualified Equ.Theories.List as EquList
import qualified Equ.Theories.FOL as EquFOL

import Data.Maybe(fromJust)



natural :: IndType
natural = fromJust $ createIndType "Natural" (TyAtom ATyNat) [EquArith.natZero] [] [EquArith.natSucc]

list :: IndType
list = fromJust $ createIndType "Lista" (tyListVar "A") [EquList.listEmpty] [] [EquList.listApp]
            
bool :: IndType
bool =  fromJust $ createIndType "Bool" (TyAtom ATyBool) [EquFOL.folTrue, EquFOL.folFalse] [] []


-- | Funcion que asocia un tipo comun de Equ con un Tipo Inductivo de Fun
getIndType :: Type -> Maybe IndType
getIndType (TyAtom ATyNat) = Just natural
getIndType (TyAtom ATyBool) = Just bool
getIndType (TyList t) = Just list
getIndType _ = Nothing

-- | Constructores de todos los tipos, útil para el parser de patrones.
constrList =  indConstructors natural ++ indConstructors list
                   
