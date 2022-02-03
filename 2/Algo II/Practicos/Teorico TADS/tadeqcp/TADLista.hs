-- TAD Lista

module TADLista (Lista, vacíaL, consL, es_vacíaL, cabezaL, colaL, concatL, largoL, puntoL, flechaL, snocL) where

import TADNatural

-- CONSTRUCTORES

data Lista e = Vacía | Cons e (Lista e)
             deriving (Show, Eq)

-- ECUACIONES ENTRE CONSTRUCTORES
-- no hay

vacíaL = Vacía
consL = Cons

-- OPERACIONES

es_vacíaL :: Lista e -> Bool
cabezaL :: Lista e -> e -- sólo se aplica a listas no vacías
colaL :: Lista e -> Lista e
concatL :: Lista e -> Lista e -> Lista e
largoL :: Lista e -> Natural
puntoL :: Lista e -> Natural -> e -- sólo se aplica a naturales menores que el largo de la lista
flechaL :: Lista e -> Natural -> Lista e
snocL :: Lista e -> e -> Lista e

-- ECUACIONES

es_vacíaL Vacía = True
es_vacíaL _ = False

cabezaL (Cons e l) = e

colaL (Cons e l) = l

Vacía `concatL` l2 = l2
Cons e l1 `concatL` l2 = Cons e (l1 `concatL`l2)

largoL Vacía = cero
largoL (Cons e l) = suc (largoL l)

Cons e l `puntoL` n | n == cero = e
                    | otherwise = l `puntoL` pre n

Vacía `flechaL` n = Vacía
l@(Cons _ l1) `flechaL` n | n == cero = l
                          | otherwise = l1 `flechaL` pre n

Vacía `snocL` f = Cons f Vacía
Cons e l `snocL` f = Cons e (l `snocL` f)

