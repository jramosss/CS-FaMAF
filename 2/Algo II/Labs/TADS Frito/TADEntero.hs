-- TAD Entero

module TADEntero(Entero, posE, ceroE, negE, sucE, preE, masE, menosE, porE, cocE, alaE, opE, absE) where

import TADNatural

-- CONSTRUCTORES

data Entero = Neg Natural 
            | Pos Natural
            deriving Show

-- CONSTRUCTORES

-- ECUACIONES ENTRE CONSTRUCTORES
-- Pos cero == Neg cero

posE = Pos
ceroE = Pos cero
negE = Neg

-- OPERACIONES

sucE, preE :: Entero -> Entero
masE, menosE, porE, cocE :: Entero -> Entero -> Entero
alaE :: Entero -> Natural -> Entero
opE :: Entero -> Entero
absE :: Entero -> Natural

-- alaE no se aplica si ambos argumentos son cero
-- cocE no se aplica si el divisor es cero

-- ECUACIONES

sucE (Pos n) = Pos (suc n)
sucE (Neg n) | n == cero = Pos (suc cero)
             | n > cero = Neg (pre n)

preE (Neg n) = Neg (suc n)
preE (Pos n) | n == cero = Neg (suc cero)
             | n > cero = Pos (pre n)

Pos n `masE` Pos m = Pos (n `mas` m)
Pos n `masE` Neg m | n >= m = Pos (n `menos` m)
                   | otherwise = Neg (m `menos` n)
Neg n `masE` Pos m | n >= m = Neg (n `menos` m)
                   | otherwise = Pos (m `menos` n)
Neg n `masE` Neg m = Neg (n `mas` m)
 
z `menosE` y = z `masE` (opE y)

Pos n `porE` Pos m = Pos (n `por` m)
Pos n `porE` Neg m = Neg (n `por` m)
Neg n `porE` Pos m = Neg (n `por` m)
Neg n `porE` Neg m = Pos (n `por` m)

z `alaE` n | z==0 && n==0 = "Potencia indefinida"
           | n==0 = Pos (suc ceroE)
           | otherwise = z `porE` (z `alaE` preE n)

z `cocE` y | y == 0 = "Division indefinida"
Pos z `cocE` Pos y = Pos (z `coc` y)
Pos z `cocE` Neg y = Neg (z `coc` y)
Neg z `cocE` Pos y = Neg (z `coc` y)
Neg z `cocE` Neg y = Pos (z `coc` y)

opE (Pos n) = Neg n
opE (Neg n) = Pos n

absE (Pos n) = n
absE (Neg n) = n

instance Entero Eq where
    Pos n == Pos m = n == m
    Neg n == Neg m = n == m
    Neg n == Pos m = n == ceroE && m == ceroE
    Pos n == Neg m = n == ceroE && m == ceroE

instance Entero Ord where
    Pos n <= Pos m = n <= m
    Pos n <= Neg m = n == ceroE && m == ceroE
    Neg n <= Pos m = True  
    Neg n <= Neg m = m <= n
    
