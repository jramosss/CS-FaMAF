{-# Language OverloadedStrings #-}
module Equ.IndType where

import Equ.Types
import Equ.Syntax
import Equ.PreExpr hiding (isVar)
import Equ.TypeChecker(unifyTest,getType)

import Data.Text (Text)
import Data.Function (on)

import System.IO.Unsafe (unsafePerformIO)

-- | Un tipo inductivo permite realizar Pattern Matching. Está relacionado con una teoria.
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
createIndType name t constants bcons icons = 
    if validConstructors constants bcons icons t
        then Just $ IndType {
                        name = name
                      , ty = t
                      , constants = constants
                      , baseConstructors = bcons
                      , indConstructors = icons
                    }
        else Nothing
                                       
             
validConstructors :: [Constant] -> [Operator] -> [Operator] -> Type -> Bool
validConstructors constants bcons indcons t =
       (and $ map (flip isConstant t) constants) &&
       (and $ map (flip isBaseConstructor t) bcons) &&
       (and $ map (flip isIndConstructor t) indcons)
             
isValidIndType :: IndType -> Bool
isValidIndType it = 
    validConstructors consts bcons icons (ty it)
    
    where bcons = baseConstructors it
          icons = indConstructors it
          consts = constants it
          

isVar :: PreExpr -> Type -> Bool
isVar (Var v) t = unifyTest t (varTy v)
isVar _ _ = False
          
isConstant :: Constant -> Type -> Bool
isConstant c t = unifyTest t (conTy c)
          
isBaseConstructor :: Operator -> Type -> Bool
isBaseConstructor op t = case opTy op of
                            t1 :-> t2 -> unifyTest t t2 && not (typeContains t1 t)
                            otherwise -> False
          
isIndConstructor :: Operator -> Type -> Bool
isIndConstructor op t = case opTy op of
                            t1 :-> t2 -> checkReturnType t t2 && (typeContains t1 t || typeContains' t2 t)--(unifyTest t t2) && (typeContains t1 t)
                            otherwise -> False
          -- typeContains t t' es true si t es t' o es de tipo funcion y contiene a t', ejemplo:
          -- typeContains (t1 :-> t2 :-> t3) t1 = true
          -- typeContains (t1 :-> t2 :-> t3) t4 = false
          
    where checkReturnType t t' = 
            case t' of
                t1 :-> t2 -> checkReturnType t t2
                t'' -> unifyTest t t'' 
          
          typeContains' t t' = 
            case t of
                t1 :-> t2 -> (unifyTest t1 t') || (typeContains' t2 t')
                t'' -> False
          
typeContains :: Type -> Type -> Bool
typeContains t t' = case t of
                        t1 :-> t2 -> typeContains t1 t' || typeContains t2 t'
                        t'' -> unifyTest t'' t'
          
isConstantPattern :: PreExpr -> Type -> Bool
isConstantPattern (Con c) t = isConstant c t
isConstantPattern _ _ = False

isBaseConstPattern :: PreExpr -> Type -> Bool
isBaseConstPattern = isConstructorPattern isBaseConstructor

isIndConstPattern :: PreExpr -> Type -> Bool
isIndConstPattern pe t = unsafePerformIO (putStrLn ("\nisIndConstPattern "++show pe++
                                                    ", "++show t) >>
                                          return (isConstructorPattern isIndConstructor pe t))
          
isConstructorPattern :: (Operator -> Type -> Bool) -> PreExpr -> Type -> Bool
isConstructorPattern f (UnOp op e) t = f op t && isVar e t
isConstructorPattern f (BinOp op e1 e2) t = f op t &&
    case opTy op of
        t1 :-> t2 :-> t3 -> {-unsafePerformIO (putStrLn ("isIndConstructor "++show op ++", "++show t++" = "++
                                                        show (f op t)++
                                                    "\nisVar "++show e1++", "++show t1++" = "++show (isVar e1 t1)++
                                                    "\nisVar "++show e2++", "++show t2++" = "++show (isVar e2 t2)) >>-}
                             isVar e1 t1 && isVar e2 t2
        _ -> False
isConstructorPattern _ _ _ = False


splitByConst :: Show b => Type -> [(PreExpr, b)] -> ([(PreExpr, b)],[(PreExpr, b)],[(PreExpr, b)]) 
splitByConst t l = foldl go ([],[],[]) l
    where go (ks,us,bs) p@(e,_) | isConstantPattern e t  = (p:ks,us,bs)
                                | isBaseConstPattern e t = (ks,p:us,bs)
                                | isIndConstPattern e t  = (ks,us,p:bs)
                                | otherwise = (ks,us,bs)


isConstructor :: IndType -> Operator -> Bool
isConstructor ty op =  op `elem` baseConstructors ty || op `elem` indConstructors ty

