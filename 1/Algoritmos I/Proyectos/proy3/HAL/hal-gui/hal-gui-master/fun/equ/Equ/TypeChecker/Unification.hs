{-# LANGUAGE FlexibleInstances #-}
-- | Transforma una PreExpresi&#243;n en una Expresi&#243;n.
module Equ.TypeChecker.Unification
    ( module Equ.TypeChecker.Error      
      -- * Algoritmo de unificaci&#243;n y variantes
    , mgu
    , nullSubst
    , Types(..)
    , unifyList
    , unifyTest
    , unificate
    , rewrite
    , findVar
    , (@@)
    -- * Algoritmo de matching
    , match
    , match2
    -- * Sustituciones de tipo
    , emptySubst
    , TySubst
    -- * Controles de tipos inductivos
    , checkReturnType
    , typeContains
    , typeContains'
    )
    where


import           Equ.Types
import           Equ.TypeChecker.Error

import qualified Data.Map as M
import qualified Data.Set as S
import           Data.Maybe
import           Data.List
import           Data.Set (Set)

-- | Tipo de la sustituci&#243;n para unificar expresiones de tipo.
type TySubst = M.Map TyVarName Type

class Types t where
      app :: TySubst -> t -> t
      tv  :: t -> Set TyVarName

nullSubst :: TySubst
nullSubst = M.empty

instance Types (Type' TyVarName) where
    app f (TyVar v) = fromMaybe (TyVar v) $ M.lookup v f
    app f (TyList t) = TyList $ app f t
    app f (t :-> t') = app f t :-> app f t'
    app _ (TyAtom a) = TyAtom a
    app _ TyUnknown = TyUnknown
    
    tv (TyVar v) = S.singleton v
    tv (TyList t) = tv t
    tv (t :-> t') = tv t `S.union` tv t'
    tv _ = S.empty


instance Types [Type] where
    app s = map (app s)
    tv = S.unions . map tv

(@@) :: TySubst -> TySubst -> TySubst
s @@ s' = M.fromList $ [(u, rewrite s t) | (u,t) <- M.toList s'] ++ M.toList s

merge :: TySubst -> TySubst -> Either TyErr TySubst
merge s s' = if agree 
             then return $ s `M.union` s'
             else Left $ ErrMerge 
    where agree = all (\(_,t,t') -> t == t') varTys
          varTys = map (\v -> (v,app s (TyVar v),app s' (TyVar v))) 
                       $ M.keys s `intersect` M.keys s'

-- | Algoritmo de unificaci&#243;n. Suponemos que no hay 'TyUnknown'.
mgu :: Type -> Type -> Either TyErr TySubst
mgu (TyAtom t) (TyAtom t') | t == t' = return nullSubst
                           | otherwise = Left $ ErrUnification (TyAtom t) (TyAtom t')
mgu (t :-> t') (r :-> r') = mgu t r >>= \s' ->
                            mgu t' r' >>= \s'' ->
                            return (s' @@ s'')
mgu (TyList t) (TyList t') = mgu t t'
mgu (TyVar v) t' = varBind v t'
mgu t (TyVar v) = varBind v t
mgu t t' = Left $ ErrUnification t t' 

varBind :: TyVarName -> Type -> Either TyErr TySubst
varBind v t | t == TyVar v = return nullSubst
            | v `S.member` tv t = Left $ ErrUnification (TyVar v) t
            | otherwise = return $ M.singleton v t

match :: Type -> Type -> Either TyErr TySubst
match (TyAtom t) (TyAtom t') | t == t' = return nullSubst
                             | otherwise = Left $ ErrMatching (TyAtom t) (TyAtom t')
match (TyList t) (TyList t') = match t t'
match (t :-> t') (r :-> r')  = match t t' >>= \s ->
                               match r r' >>= \s' -> 
                               merge s s'
                               
match (TyVar v) t = return $ M.singleton v t
match t t' = Left $ ErrMatching t t'



-- | Aplicar una sustituci&#243;n (finita) a un variable de tipo.
findVar :: TyVarName -> TySubst -> Type
findVar v = M.findWithDefault (TyVar v) v

-- | Uso de una sustituci&#243;n para reemplazar todas las variables en un
-- tipo.
rewrite :: TySubst -> Type -> Type
rewrite s = (>>= (flip findVar s))


unifyList :: [Type] -> TySubst -> Either TyErr TySubst
unifyList [] s = return s
unifyList [_] s = return s
unifyList (t:t':ts) s = mgu t t' >>= unifyList (t':ts) . (s@@)

-- | Usamos unify para comprobar si existe o no unificaci&#243;n. 
--   Suponemos que no hay 'TyUnknown'.
unifyTest :: Type -> Type -> Bool
unifyTest t t' = either (const False) (const True) $ mgu t t'

-- | Sustituci&#243;n vac&#237;a.
emptySubst :: TySubst
emptySubst = M.empty

unificate :: Type -> Type -> Maybe Type
unificate t t' = either (const Nothing) (return . flip rewrite t) $ mgu t t' 


-- | Esta funcion hace matching de variable con tipo concreto y tambien de tipo
--   concreto con variable.
match2 :: Type -> Type -> Bool
match2 (TyVar _) _ = True
match2 (TyList t) (TyList t') = match2 t t'
match2 (t1 :-> t2) (t1' :-> t2') = match2 t1 t1' && match2 t2 t2'
match2 _ (TyVar _) = True
match2 t t' = t == t'  

-- | Devuelve el tipo que está más a la derecha de la última flecha.
getResType :: Type -> Type
getResType (_ :-> t') = getResType t'
getResType t = t

-- | Verifica si @t@ es unificable con el tipo "resultado" del segundo argumento.
checkReturnType :: Type -> Type -> Bool
checkReturnType t = unifyTest t . getResType


-- typeContains t t' es true si t es t' o es de tipo funcion y contiene a t', ejemplo:
-- typeContains (t1 :-> t2 :-> t3) t1 = true
-- typeContains (t1 :-> t2 :-> t3) t4 = false
typeContains :: Type -> Type -> Bool
typeContains t t' = case t of
                        t1 :-> t2 -> typeContains t1 t' || typeContains t2 t'
                        t'' -> unifyTest t'' t'

 
typeContains' :: Type -> Type -> Bool
typeContains' t t' = case t of
                          t1 :-> t2 -> unifyTest t1 t' || typeContains' t2 t'
                          _ -> False
