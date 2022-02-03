{-# Language OverloadedStrings #-}
module Equ.IndType where

import Equ.Types
import Equ.TypeChecker.Unification
import Equ.Syntax
import Equ.PreExpr hiding (isVar)

import Data.Text (Text)
import Data.Function (on)

-- | Un tipo inductivo permite realizar pattern-matching. Está
-- relacionado con una teoria.
data IndType = IndType {
            name :: Text
          , ty :: Type
          , constants :: [Constant]
          , baseConstructors :: [Operator]
          , indConstructors :: [Operator]
}
    deriving Show

instance Eq IndType where
    (==) = (==) `on` ty

-- | Un tipo inductivo valido debe ser construido con la siguiente funcion.
-- | Si es el tipo que se quiere construir, t, es válido devuelve Just t. Otro caso Nothing.
createIndType :: Text -> Type -> [Constant] -> [Operator] -> [Operator] -> Maybe IndType
createIndType nm t consts baseConstrs indConstrs = 
    if validConstrs consts baseConstrs indConstrs t
    then Just $ IndType { name = nm
                        , ty = t
                        , constants = consts
                        , baseConstructors = baseConstrs
                        , indConstructors = indConstrs
                        }
    else Nothing
                                       
-- | Controla que ciertas constantes y operadores sean los
-- constructores del tipo apropiado.
validConstrs :: [Constant] -> [Operator] -> [Operator] -> Type -> Bool
validConstrs consts baseConstrs indConstrs t = all (isConstant t) consts &&
                                               all (isBaseConstructor t) baseConstrs &&
                                               all (isIndConstructor t) indConstrs

isValidIndType :: IndType -> Bool
isValidIndType it = validConstrs consts baseConstrs indConstrs (ty it)
    where baseConstrs = baseConstructors it
          indConstrs = indConstructors it
          consts = constants it
          

isVarTyped :: PreExpr -> Type -> Bool
isVarTyped (Var v) t = unifyTest t (varTy v)
isVarTyped _ _ = False
          
isConstant :: Type -> Constant -> Bool
isConstant t c = unifyTest t (conTy c)
          
isBaseConstructor :: Type -> Operator -> Bool
isBaseConstructor t op = case opTy op of
                            t1 :-> t2 -> unifyTest t t2 && not (typeContains t1 t)
                            _ -> False
          
isIndConstructor :: Type -> Operator -> Bool
isIndConstructor t op = case opTy op of
                            t1 :-> t2 -> checkReturnType t t2 && (typeContains t1 t || typeContains' t2 t)
                            _ -> False          
          
isConstantPattern :: Type -> PreExpr -> Bool
isConstantPattern t (Con c) = isConstant t c
isConstantPattern _ _ = False

isBaseConstPattern :: Type -> PreExpr -> Bool
isBaseConstPattern = isConstructorPattern isBaseConstructor

isIndConstPattern :: Type -> PreExpr -> Bool
isIndConstPattern = isConstructorPattern isIndConstructor
          
isConstructorPattern :: (Type -> Operator -> Bool) -> Type -> PreExpr -> Bool
isConstructorPattern f t (UnOp op e) = f t op && isVarTyped e t
isConstructorPattern f t (BinOp op e1 e2) = f t op &&
    case opTy op of
        t1 :-> t2 :-> _ -> isVarTyped e1 t1 && isVarTyped e2 t2
        _ -> False
isConstructorPattern _ _ _ = False


splitByConst :: Show b => Type -> [(PreExpr, b)] -> ([(PreExpr, b)],[(PreExpr, b)],[(PreExpr, b)]) 
splitByConst t l = foldl go ([],[],[]) l
    where go (ks,us,bs) p@(e,_) | isConstantPattern t e  = (p:ks,us,bs)
                                | isBaseConstPattern t e = (ks,p:us,bs)
                                | isIndConstPattern t e  = (ks,us,p:bs)
                                | otherwise = (ks,us,bs)


isConstructor :: IndType -> Operator -> Bool
isConstructor t op =  op `elem` baseConstructors t ++ indConstructors t

