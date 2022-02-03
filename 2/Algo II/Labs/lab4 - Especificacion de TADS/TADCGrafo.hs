-- TAD CGrafo

module TADCGrafo (CGrafo, vacíoCG, aristaCG, es_vacíoCG, costoCG, subCG, vérticesCG, costo_caminoCG) where

import TADNatural
import TADCosto
import TADConjunto

-- CONSTRUCTORES

data CGrafo e = Vacío | Arista e e Natural (CGrafo e)
              deriving Show

-- ECUACIONES ENTRE CONSTRUCTORES
-- Arista a b n (Arista a b m g) == Arista a b (n `min` m)g
-- Arista a b n (Arista c d m g) == Arista c d m (Arista a b n g)

vacíoCG = Vacío
aristaCG = Arista

-- OPERACIONES

es_vacíoCG :: CGrafo e -> Bool
costoCG :: Eq e => e -> e -> CGrafo e -> Costo
subCG :: Eq e => CGrafo e -> CGrafo e -> Bool
costo_caminoCG :: Eq e => e -> e -> CGrafo e -> Costo
vérticesCG :: Eq e => CGrafo e -> Conjunto e

-- ECUACIONES

es_vacíoCG Vacío = True
es_vacíoCG (Arista a b n g) = False

costoCG v w Vacío = infinitoC
costoCG v w (Arista a b n g) | v == a && w == b = finitoC n `min` costoCG v w g
                             | otherwise = costoCG v w g

subCG Vacío h = True
subCG (Arista a b n g) h = costoCG a b h <= finitoC n && subCG g h

vérticesCG Vacío = vacíoC
vérticesCG (Arista a b n g) = consC a (consC b (vérticesCG g))

costo_caminoCG v w Vacío | v == w = finitoC cero
                         | otherwise = infinitoC
costo_caminoCG v w (Arista a b n g) = costo_caminoCG v w g `min` (finitoC n `masC` costo_caminoCG v a g `masC` costo_caminoCG b w g)                                      
instance Eq e => Eq (CGrafo e) where
  g == h = g `subCG` h && h `subCG` g