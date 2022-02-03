-- | Declaración de nombres de constantes, operadores y cuantificadores.
-- Esta definición es para tener un mecanismo eficiente de pattern matching
module Equ.Theories.AbsName where

import Test.QuickCheck(Arbitrary, arbitrary, elements)
import Data.Serialize

-- | Nombres de constantes
data ConName = Empty  -- ^ Lista: vacia
               | Zero -- ^ Num (polimorfico): cero
               | CTrue -- ^ FOL: true
               | CFalse -- ^ FOL: false
                 deriving (Show,Eq,Ord)

-- | Nombres de operadores
data OpName = Append    -- ^ Lista: agregar por la izquierda 
              | Index   -- ^ Lista: indexar
              | Length  -- ^ Lista: longitud
              | Concat  -- ^ Lista: concatenar
              | Take    -- ^ Lista: tomar una cantidad de elementos
              | Drop    -- ^ Lista: tirar una cantidad de elementos
              | NatEqual-- ^ Num (polimorfico): igualdad
              | Succ    -- ^ Num (polimorfico): sucesor
              | Sum     -- ^ Num (polimorfico): suma
              | Prod    -- ^ Num (polimorfico): producto
              | Div    -- ^ Num (polimorfico): división
              | Mod    -- ^ Num (polimorfico): resto
              | Pred    -- ^ Nat : Predecesor
              | Dif     -- ^ Nat : Resta
              | NegNum  -- ^ Nat : negación numerica.
              | Equival   -- ^ FOL: Equivalencia
              | Discrep -- ^ FOL: Discrepancia
              | Implic    -- ^ FOL: Implicacion
              | Conseq  -- ^ FOL: Consecuencia
              | And     -- ^ FOL: "y" lógico
              | Or      -- ^ FOL: "o" lógico
              | Neg     -- ^ FOL: Negación
              | Equal
              | LessThan
              | LessOrEqThan
                 deriving (Eq,Ord,Enum,Show)

              
-- | Nombres de cuantificadores
data QuantName = Forall  -- ^ FOL: para todo
               | Exist -- ^ FOL: existe
               | SumQuant -- ^ Arith: Sumatoria
               | ThatQuant -- ^ Todos: Cuantificador That
               | ContQuant -- ^ Arith: Cuantificador Contar
               | MinQuant -- ^ Arith: Cuantificador Mínimo
               | MaxQuant -- ^ Arith: Cuantificador Máximo
                 deriving (Eq,Ord,Show)

-- | Instancia arbitrary para ConName
instance Arbitrary ConName where
    arbitrary = elements [Empty, Zero, CTrue, CFalse]

-- | Instancia arbitrary para OpName
instance Arbitrary OpName where 
    arbitrary = 
        elements [  Append
                    , Index
                    , Length
                    , Concat  
                    , Take
                    , Drop
                    , Succ
                    , Sum
                    , Equival
                    , Discrep
                    , Implic
                    , Conseq
                    , And
                    , Or
                    , Neg
                    ]

-- | Instancia arbitrary para QuantName
instance Arbitrary QuantName where
    arbitrary = elements [Forall, Exist]

instance Serialize ConName where
    put Empty = putWord8 0
    put Zero = putWord8 1
    put CTrue = putWord8 2
    put CFalse = putWord8 3

    get = do
    tag_ <- getWord8
    case tag_ of
        0 -> return Empty
        1 -> return Zero
        2 -> return CTrue
        3 -> return CFalse
        _ -> fail "Problem: Instance Serialize ConName."

instance Serialize OpName where
    put Append   = putWord8 0
    put Index    = putWord8 1
    put Length   = putWord8 2
    put Concat   = putWord8 3
    put Take     = putWord8 4
    put Drop     = putWord8 5
    put NatEqual = putWord8 6
    put Succ     = putWord8 7
    put Sum      = putWord8 8
    put Prod     = putWord8 9
    put Equival  = putWord8 10
    put Discrep  = putWord8 11
    put Implic   = putWord8 12
    put Conseq   = putWord8 13
    put And      = putWord8 14
    put Or       = putWord8 15
    put Neg      = putWord8 16
    put Equal    = putWord8 17
    put Div      = putWord8 18
    put Mod      = putWord8 19
    put Pred     = putWord8 20
    put Dif      = putWord8 21
    put NegNum   = putWord8 19
    put LessThan = putWord8 20
    put LessOrEqThan = putWord8 21

    get = do
    tag_ <- getWord8
    case tag_ of
        0 -> return Append
        1 -> return Index
        2 -> return Length
        3 -> return Concat
        4 -> return Take
        5 -> return Drop
        6 -> return NatEqual
        7 -> return Succ
        8 -> return Sum
        9 -> return Prod
        10 -> return Equival
        11 -> return Discrep
        12 -> return Implic
        13 -> return Conseq
        14 -> return And
        15 -> return Or
        16 -> return Neg
        17 -> return Equal
        18 -> return Div
        19 -> return Mod
        20 -> return Pred
        21 -> return Dif
        22 -> return NegNum
        23 -> return LessThan
        24 -> return LessOrEqThan
        _ -> fail "Problem: Instance Serialize OpName."

instance Serialize QuantName where
    put Forall = putWord8 0
    put Exist = putWord8 1
    put SumQuant = putWord8 2
    put ThatQuant = putWord8 3
    put ContQuant = putWord8 4
    put MinQuant  = putWord8 5
    put MaxQuant  = putWord8 6

    get = do
    tag_ <- getWord8
    case tag_ of
        0 -> return Forall
        1 -> return Exist
        2 -> return SumQuant 
        3 -> return ThatQuant
        4 -> return ContQuant
        5 -> return MinQuant
        6 -> return MaxQuant
        _ -> fail "Problem: Instance Serialize QuantName."

