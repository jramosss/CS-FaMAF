{-| Este modulo contiene la declaracion de los posibles tipos para
los términos de las (pre-)expresiones. Como en las pre-expresiones,
declaramos un tipo de datos general que nos permite utilizar muchas
funciones e idiomas estándares de Haskell.  -}

{-# Language TypeSynonymInstances,FlexibleInstances #-}
module Equ.Types where

import Data.Text (Text, pack, unpack)
import qualified Data.Text as T (head)
import Data.Char

import Control.Applicative
import Test.QuickCheck(Arbitrary, arbitrary, elements, oneof)
import Data.Monoid
import qualified Data.Foldable as F
import Data.Traversable
import Data.Serialize(Serialize, get, getWord8, put, putWord8)

-- | Tipos de datos atómicos.
data AtomTy = ATyNum  -- ^ Los reales.
            | ATyInt  -- ^ Los enteros.
            | ATyNat  -- ^ Los naturales.
            | ATyBool -- ^ Corresponde a las fórmulas proposicionales.
     deriving (Eq)

instance Show AtomTy where
    show ATyNum = "Num"
    show ATyInt = "Int"
    show ATyNat = "Nat"
    show ATyBool = "Bool"

-- | Las variables de tipo.
type TyVarName = Text

infixr 8 :->

-- | Un tipo polimórfico para tener instancias de Functor y Monad; las
-- variables de tipo se asumen cuantificadas universalmente.
data Type' v = TyUnknown            -- ^ Representa falta de información.
             | TyVar v              -- ^ Variable de tipo.
             | TyList (Type' v)     -- ^ Listas.
             | TyAtom AtomTy        -- ^ Tipos atómicos.
             | Type' v :-> Type' v  -- ^ Espacios de funciones.
    deriving (Eq)

instance Functor Type' where
    fmap f (TyVar v) = TyVar $ f v
    fmap f (TyList t) = TyList $ fmap f t
    fmap f (t :-> t') = fmap f t :-> fmap f t'
    fmap _ (TyAtom a) = TyAtom a
    fmap _ TyUnknown = TyUnknown

instance Applicative Type' where
    pure = TyVar
    _ <*> TyUnknown = TyUnknown
    _ <*> TyAtom a = TyAtom a
    TyVar f <*> TyVar v = TyVar $ f v
    TyAtom a <*> TyVar _ = TyAtom a
    TyUnknown <*> TyVar _ = TyUnknown
    TyList f <*> TyVar v = TyList $ f <*> TyVar v
    (f :-> f') <*> TyVar v = (f <*> TyVar v) :-> (f' <*> TyVar v)
    f <*> TyList t = TyList $ (f <*> t)
    f <*> t :-> t' = (f <*> t) :-> (f <*> t')

instance F.Foldable Type' where
    foldMap f (TyVar e) = f e
    foldMap f (TyList t) = F.foldMap f t
    foldMap f (t :-> t') = F.foldMap f t `mappend` F.foldMap f t'
    foldMap _ _ = mempty


-- TODO: tiene sentido?
instance Traversable Type' where
    traverse f (TyVar e) = TyVar <$> f e
    traverse f (TyList t) = TyList <$> traverse f t
    traverse f (t :-> t') = liftA2 (:->) (traverse f t) (traverse f t')
    traverse _ TyUnknown = pure TyUnknown
    traverse _ (TyAtom a) = pure (TyAtom a)

instance Monad Type' where
    return a = TyVar a
    TyUnknown >>= _ = TyUnknown
    TyAtom t >>= _ = TyAtom t
    TyVar v >>= f = f v
    TyList t >>= f = TyList $ t >>= f
    t :-> t' >>= f = (:->) (t >>= f) (t' >>= f)

instance Serialize TyVarName where
    put = put . unpack
    get = get >>= return . pack

instance Serialize AtomTy where
    put ATyNum = putWord8 0
    put ATyInt = putWord8 1
    put ATyNat = putWord8 2
    put ATyBool = putWord8 3
    
    get = do
    tag_ <- getWord8
    case tag_ of
        0 -> return ATyNum
        1 -> return ATyInt
        2 -> return ATyNat
        3 -> return ATyBool
        _ -> fail $ "SerializeErr AtomTy " ++ show tag_

instance (Serialize a) => Serialize (Type' a) where
    put TyUnknown = putWord8 0
    put (TyVar v) = putWord8 1 >> put v
    put (TyList t) = putWord8 2 >> put t
    put (TyAtom a) = putWord8 3 >> put a
    put (t :-> t') = putWord8 4 >> put t >> put t'

    get = do
    tag_ <- getWord8
    case tag_ of
        0 -> return TyUnknown
        1 -> TyVar <$> get
        2 -> TyList <$> get
        3 -> TyAtom <$> get
        4 -> (:->) <$> get <*> get
        _ -> fail $ "SerializeErr (Type' a) " ++ show tag_

-- | El tipo concreto de nuestras expresiones.
type Type = Type' TyVarName

instance Show Type where
    show TyUnknown = "?"
    show (TyVar v) = unpack v
    show (TyList t) = "[" ++ show t ++ "]"
    show (TyAtom t) = show t
    show (t :-> t') = show t ++ " -> " ++ show t'


-- | Constructor de TyVar
tyVar :: String -> Type
tyVar = TyVar . pack 

-- | Constructor de TyAtom ATyBool
tyBool :: Type
tyBool = TyAtom ATyBool

tyInt :: Type
tyInt = TyAtom ATyInt

-- | Ocurencia de una variable en un tipo.
occurs :: TyVarName -> Type -> Bool
occurs v = F.elem v

-- | Replace the occurrence of a type-variable for a type: 'replace v
-- s t', replaces the occurences of 'v' in 's' for 't'.
tyreplace :: TyVarName -> Type -> Type -> Type
tyreplace v t t' = t' >>= (\w -> if v == w then t else TyVar w) 

tyVarInternal :: Int -> Type
tyVarInternal n = tyVar $ "V" ++ show n

isTyVarInternal :: TyVarName -> Bool
isTyVarInternal = isUpper . T.head 

isTyVar :: Type -> Bool
isTyVar (TyVar _)  = True
isTyVar _ = False

-- -- | Instancia arbitrary para el tipo nombre de variable. 
instance Arbitrary TyVarName where
    arbitrary = 
        elements [(pack . ("t"++) . show) n | n <- [(0::Int)..100]]

-- | Instancia arbitrary para los tipos atómicos.
instance Arbitrary AtomTy where
    arbitrary = elements [ATyNum, ATyInt, ATyNat, ATyBool]
    
-- | Instancia arbitrary para los tipos generales.
instance Arbitrary Type where
    arbitrary = 
        oneof [ TyVar <$> arbitrary
              , TyList <$> arbitrary
              , TyAtom <$> arbitrary
              , (:->) <$> arbitrary <*> arbitrary
              ]

arity :: Type -> Int
arity (_ :-> t') = 1 + arity t'
arity _ = 0

argsTypes :: Type -> [Type]
argsTypes = reverse . go []
    where go :: [Type] -> Type -> [Type]
          go ts (t :-> t') = go (t:ts) t'
          go ts _ = ts

resType :: Type -> Maybe Type
resType (_ :-> t') = Just t'
resType _ = Nothing

exponential :: Type -> [Type] -> Type
exponential = foldr (:->) 
