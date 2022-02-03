module TADConjunto (Conjunto, unionC, interC, diffC, es_vacioC, perC) where
    
-- Constructores

data Conjunto e = Vacio | Agg e (Conjunto e)
    deriving (Eq, Ord, Show)

-- Operaciones

unionC, interC :: Conjunto e -> Conjunto e -> Conjunto e 
es_vacioC :: Conjunto e -> Bool
perC :: forall e .Conjunto e -> e -> Bool
sinC :: e -> Conjunto e -> Conjunto e
-- Ecuaciones

es_vacioC Vacio = True
es_vacioC (Agg k _) = False

perC _ Vacio = False
perC x (Agg k p) | x == k = True
                 | otherwise = (perC x p)

Vacio `unionC` p = p
(Agg k q) `unionC` p = Agg k (q `unionC` p)

Vacio `interC` p = p
(Agg k q) `interC` p | (perC k p) = Agg k (q `interC` p)
                     | otherwise = (q `interC` p)

sinC x Vacio = "Error cjt vacio"
sinC x (Agg k p) | k == x = p
                 | otherwise = Agg k (sinC x p)