-- TAD Grafo

module TADGrafo (Grafo, vacíoG, aristaG, es_vacíoG, está_vérticeG, está_aristaG, vérticesG, subG, hay_caminoG) where

import TADConjunto

-- CONSTRUCTORES

data Grafo e = Vacío | Arista e e (Grafo e)
             deriving Show

-- ECUACIONES ENTRE CONSTRUCTORES
-- Arista a b (Arista a b g) == Arista a b g
-- Arista a b (Arista c d g) == Arista c d (Arista a b g)

vacíoG = Vacío
aristaG = Arista

-- OPERACIONES

es_vacíoG :: Grafo e -> Bool
está_vérticeG :: Eq e => e -> Grafo e -> Bool
está_aristaG :: Eq e => e -> e -> Grafo e -> Bool
vérticesG :: Eq e => Grafo e -> Conjunto e
subG :: Eq e => Grafo e -> Grafo e -> Bool
hay_caminoG :: Eq e => e -> e -> Grafo e -> Bool

-- ECUACIONES

es_vacíoG Vacío = True
es_vacíoG _ = False

está_vérticeG v Vacío = False
está_vérticeG v (Arista a b g) = v == a || v == b || está_vérticeG v g

está_aristaG v w Vacío = False
está_aristaG v w (Arista a b g) = (v == a && w == b) || está_aristaG v w g

vérticesG Vacío = vacíoC
vérticesG (Arista a b g) = consC a (consC b (vérticesG g))

subG Vacío h = True
subG (Arista a b g) h = está_aristaG a b h && subG g h

hay_caminoG v w Vacío = (v == w)
hay_caminoG v w (Arista a b g) = (hay_caminoG v w g) || (hay_caminoG a b g && hay_caminoG b w g)
instance Eq e => Eq (Grafo e) where
  g == h = g `subG` h && h `subG` g