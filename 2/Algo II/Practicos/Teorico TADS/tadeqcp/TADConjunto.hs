-- TAD Conjunto Finito

module TADConjunto (Conjunto, vacíoC, consC, es_vacíoC, elemC, unionC, interC, singC, sinC, imagenC) where

-- CONSTRUCTORES

data Conjunto e = Vacío | Cons e (Conjunto e)
                deriving Show

-- ECUACIONES ENTRE CONSTRUCTORES
-- Cons e (Cons e c) == Cons e c
-- Cons e (Cons f c) == Cons f (Cons e c)

vacíoC = Vacío
consC = Cons

-- OPERACIONES

es_vacíoC :: Conjunto e -> Bool
elemC :: Eq e => e -> Conjunto e -> Bool
unionC :: Conjunto e -> Conjunto e -> Conjunto e
interC :: Eq e => Conjunto e -> Conjunto e -> Conjunto e
singC :: e -> Conjunto e
sinC :: Eq e => Conjunto e -> e -> Conjunto e
imagenC :: (e -> a) -> Conjunto e -> Conjunto a

-- ECUACIONES

es_vacíoC Vacío = True
es_vacíoC _ = False

e `elemC` Vacío = False
e `elemC` (Cons f c) = e == f || e `elemC` c

Vacío `unionC` d = d
Cons e c `unionC` d = Cons e (c `unionC` d)

Vacío `interC` d = Vacío
Cons e c `interC` d | e `elemC` d = Cons e (c `interC` d)
                    | otherwise = c `interC` d

singC e = consC e vacíoC

sinC Vacío _ = Vacío
sinC (Cons e c) f | e == f = sinC c f
                  | otherwise = Cons e (sinC c f)

f `imagenC` Vacío = Vacío
f `imagenC` Cons e c = Cons (f e) (f `imagenC` c)

instance Eq e => Eq (Conjunto e) where
  Vacío == d = es_vacíoC d
  c@(Cons e _) == d = elemC e d && sinC c e == sinC d e


