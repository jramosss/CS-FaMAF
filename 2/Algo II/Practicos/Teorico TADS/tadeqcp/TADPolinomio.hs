-- TAD Polinomio

module TADPolinomio (Polinomio, nuloP, monomioP, es_nuloP, opP, gradoP, coefP, restoP, masP, evalP) where

import TADNatural
import TADEntero

-- CONSTRUCTORES

data Polinomio = Nulo | Monomio Entero Natural Polinomio
               deriving Show

-- ECUACIONES ENTRE CONSTRUCTORES
-- Monomio c e (Monomio d e p) = Monomio (masE c d) e p
-- Monomio ceroE e p = p
-- Monomio c e (Monomio d f p) = Monomio d f (Monomio c e p)

-- En la notación habitual de monomios como cx^e, estas ecuaciones se
-- escribirían, respectivamente:
-- c x^e + d x^e + p = (c+d) x^e + p
-- 0 x^e + p = p
-- c x^e + d x^f + p = d x^f + c x^e + p

nuloP = Nulo
monomioP = Monomio

-- OPERACIONES

es_nuloP :: Polinomio -> Bool
opP :: Polinomio -> Polinomio
gradoP :: Polinomio -> Natural -- no está definido para el polinomio nulo
coefP :: Polinomio -> Natural -> Entero
restoP :: Polinomio -> Natural -> Polinomio
masP :: Polinomio -> Polinomio -> Polinomio
evalP :: Polinomio -> Entero -> Entero

-- ECUACIONES

es_nuloP Nulo = True
es_nuloP p@(Monomio _ e _) = coefP p e == ceroE && es_nuloP (restoP p e)

opP Nulo = Nulo
opP (Monomio c e p) = Monomio (opE c) e (opP p)

gradoP p | es_nuloP p = error "No se puede obtener el grado del polinomio nulo"
gradoP p@(Monomio _ e q) | coefP p e == ceroE = gradoP (restoP q e)
                         | es_nuloP (restoP q e) = e
                         | otherwise = e `max` gradoP (restoP q e)
 
coefP Nulo n = ceroE
coefP (Monomio c e p) n | e == n = c `masE` coefP p n
                        | otherwise = coefP p n

restoP Nulo n = Nulo
restoP (Monomio c e p) n | e == n = restoP p n
                         | otherwise = Monomio c e (restoP p n)

Nulo `masP` q = q
Monomio c e p `masP` q = Monomio c e (p `masP` q)

Nulo `evalP` z = ceroE
Monomio c e p `evalP` z = (c `porE` (z `alaE` e)) `masE` (p `evalP` z)

instance Eq Polinomio where
  Nulo == q = es_nuloP q
  p@(Monomio c e _) == q = coefP p e == coefP q e && restoP p e == restoP q e

