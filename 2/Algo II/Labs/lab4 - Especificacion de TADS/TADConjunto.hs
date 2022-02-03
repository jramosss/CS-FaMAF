-- TAD Conjunto Finito

module TADConjunto (Conjunto, vacíoC, consC, es_vacíoC, elemC, inclC, unionC, interC, maxC) where

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
inclC :: Eq e => Conjunto e -> Conjunto e -> Bool
unionC :: Conjunto e -> Conjunto e -> Conjunto e
interC :: Eq e => Conjunto e -> Conjunto e -> Conjunto e
maxC :: Ord e => Conjunto e -> e -- solo se aplica a conjuntos no vacíos

-- ECUACIONES

es_vacíoC Vacío = True
es_vacíoC _ = False

e `elemC` Vacío = False
e `elemC` (Cons f c) = e == f || e `elemC` c

Vacío `inclC` d = True
Cons e c `inclC` d = e `elemC` d && c `inclC` d

Vacío `unionC` d = d
Cons e c `unionC` d = Cons e (c `unionC` d)

Vacío `interC` d = Vacío
Cons e c `interC` d | e `elemC` d = Cons e (c `interC` d)
                    | otherwise = c `interC` d

maxC (Cons e Vacío) = e
maxC (Cons e c) = e `max` maxC c

instance Eq e => Eq (Conjunto e) where
  c == d = c `inclC` d && d `inclC` c


