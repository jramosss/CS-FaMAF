-- TAD Racional

module TADRacional(Racional, ceroQ, masQ, menosQ, porQ, divQ, alaQ, opQ, absQ, invQ) where

import TADNatural
import TADEntero

-- CONSTRUCTORES

data Racional = Fraccion Entero Natural 
            deriving Show

-- CONSTRUCTORES

-- BUENA CONSTRUCCIÓN
-- Fraccion a b está bien construido sii b /= cero

-- ECUACIONES ENTRE CONSTRUCTORES
-- Fraccion a b == Fraccion c d sii a*d == b*c

fraccion a b | b == cero = error "No es un racional"
             | otherwise = Fraccion a b

ceroQ = fraccion ceroE (suc cero)

-- OPERACIONES

masQ, menosQ, porQ, divQ :: Racional -> Racional -> Racional
alaQ :: Racional -> Entero -> Racional
opQ :: Racional -> Racional
absQ :: Racional -> Racional

-- alaQ no se aplica si ambos argumentos son cero
-- cocQ no se aplica si el divisor es cero

-- ECUACIONES

Fraccion a b `masQ` Fraccion c d = Fraccion (ad `masE` cb) bd
                                 where bd = b `por` d
                                       ad = a `porE` posE d
                                       cb = c `porE` posE b

q `menosQ` r = q `masQ` (opQ r)

Fraccion a b `porQ` Fraccion c d = Fraccion ac bd
                                 where bd = b `por` d
                                       ac = a `porE` c

invQ (Fraccion a b) | a == ceroE = error "inverso indefinido"
                    | a < ceroE = Fraccion (negE b) (absE a)
                    | otherwise = Fraccion (posE b) (absE a)

alaQN :: Racional -> Natural -> Racional
Fraccion a b `alaQN` n = Fraccion (a `alaE` n) (b `ala` n)

q `alaQ` z | q == ceroQ && z == ceroE = error "potencia indefinida"
           | z == ceroE = Fraccion (posE (suc cero)) (suc cero)
           | z < ceroE = invQ q `alaQN` absE z
           | otherwise = q `alaQN` absE z

q `divQ` r | r == ceroQ = error "división indefinida"
           | otherwise = q `porQ` invQ r

opQ (Fraccion a b) = Fraccion (opE a) b

absQ (Fraccion a b) = Fraccion (posE (absE a)) b

instance Eq Racional where
  Fraccion a b == Fraccion c d = a `porE` (posE d) == c `porE` (posE b)

instance Ord Racional where
  Fraccion a b <= Fraccion c d = a `porE` (posE d) <= c `porE` (posE b)


