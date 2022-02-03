module TADConjunto where
data Conjunto e = E | Add e (Conjunto e)
                deriving (Eq,Show)

existe :: Eq e => Conjunto e -> e ->  Bool
existe E _ = False
existe (Add e c) x = e == x || existe c x

interseccion :: Eq e => Conjunto e -> Conjunto e -> Conjunto e
interseccion E x = E
interseccion (Add e c) (Add s k) | existe (e k) = (Add e (interseccion c k)) 
                                 | otherwise = interseccion c k

union :: Conjunto e -> Conjunto e -> Conjunto e
union E x = x
union (Add e c) q = (Add e (union c q))

dif :: Eq e => Conjunto e -> Conjunto e -> Conjunto e
dif E x = x
dif (Add e c) (Add s k) | not (existe (e k)) = (Add e (dif c k)) 
                        | otherwise = dif c k