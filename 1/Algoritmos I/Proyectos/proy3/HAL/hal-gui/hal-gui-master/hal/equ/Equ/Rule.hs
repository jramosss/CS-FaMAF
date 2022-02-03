-- | Este módulo define el tipo de las reglas de re-escritura.
{-# Language OverloadedStrings #-}
module Equ.Rule (
                  Relation (..)
                , Rule (..)
                -- * Constructores de las diferentes relaciones.
                , relEq, relEquiv, relImpl, relCons, relEval, mkrule
                , getRelationFromType
                )
    where
    
import Equ.Expr
import Equ.Types

import Data.Text    
import Data.Serialize

import Control.Applicative ((<$>), (<*>))
import Test.QuickCheck(Arbitrary, arbitrary, elements)

-- | Nombres de las relaciones entre pasos de una demostracion
data RelName = Eq     -- ^ Igualdad polimorfica excepto para formulas
             | Equiv  -- ^ FOL: equivalencia
             | Impl   -- ^ FOL: implicacion
             | Cons   -- ^ FOL: consecuencia
             | Eval   -- ^ FUN: evaluacion
    deriving (Eq, Show, Enum)

instance Arbitrary RelName where
    arbitrary = elements [ Eq
                         , Equiv
                         , Impl
                         , Cons
                         ]

-- | Relaciones entre pasos de una demostracion
data Relation = Relation {
      relRepr :: Text
    , relName :: RelName
    , relSym :: Bool  -- ^ Es la relación simétrica? Este dato es usado en la 
                     -- creación de reglas.
    , relTy   :: Type -- ^ Este es el tipo de las cosas relacionadas.  
    }
    deriving Eq
    
instance Show Relation where
    show = unpack . relRepr

instance Arbitrary Relation where
    arbitrary = Relation <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

-- | Relación de igualdad ; =
relEq :: Relation
relEq = Relation { relRepr = pack "="
                 , relName = Eq
                 , relSym = True
                 , relTy = tyVar "A"
                 }

-- | Relación de equivalencia ; ≡
relEquiv :: Relation
relEquiv = Relation { relRepr = pack "≡"
                    , relName = Equiv
                    , relSym = True
                    , relTy = tyBool
                    }

-- | Relación de implicación ; "⇒"
relImpl :: Relation
relImpl = Relation { relRepr = pack "⇒"
                   , relName = Impl
                   , relSym = False
                   , relTy = tyBool
                   }

-- | relación de consecuencia ; ⇐
relCons :: Relation
relCons = Relation { relRepr = pack "⇐"
                   , relName = Cons
                   , relSym = False
                   , relTy = tyBool
                   }

relEval :: Relation
relEval = Relation (pack "↝") Eval False (tyVar "a")
    
-- | Regla de reescritura
data Rule = Rule {
      lhs :: Expr
    , rhs :: Expr
    , rel :: Relation  
    , name :: Text
    , desc :: Text
    }
    deriving (Show, Eq)


mkrule :: Expr -> Expr -> Relation -> Rule
mkrule e e' r = Rule e e' r "" ""

getRelationFromType :: Type -> Relation
getRelationFromType (TyAtom ATyBool) = relEquiv
getRelationFromType _ = relEq


instance Arbitrary Rule where
    arbitrary = Rule <$> arbitrary <*> arbitrary <*> 
                         arbitrary <*> arbitrary <*> arbitrary

instance Serialize Rule where
    put (Rule ls rs r n d) = put ls >> put rs >> put r >>
                                put n >> put d

    get = Rule <$> get <*> get <*> get <*> get <*> get

instance Serialize RelName where
    put = putWord8 . toEnum . fromEnum
    get = getWord8 >>= return . toEnum . fromEnum

instance Serialize Relation where
    put (Relation r n s t) = put r >> put n >> put s >> put t 
    get = Relation <$> get <*> get <*> get <*> get

