module Language.ListAssoc where

data ListAssoc a b = Empty
                   | Node a b (ListAssoc a b)
                   
la_long :: Integral c => ListAssoc a b -> c
la_long Empty = 0
la_long (Node _ _ c) = 1 + la_long c

--b2)
la_concat :: (Eq a, Eq b) => ListAssoc a b -> ListAssoc a b -> ListAssoc a b
la_concat Empty k = k
la_concat (Node a b c) d = (Node a b (la_concat c d ))


--b3)
la_pares :: ListAssoc a b -> [(a,b)]
la_pares Empty = []
la_pares (Node a b c) = (a,b) : la_pares c

--b4)
la_busca :: Eq a => ListAssoc a b -> a -> Maybe b
la_busca Empty _ = Nothing
la_busca (Node a b c) x |x==a = Just b 
                        |otherwise = la_busca c x

--b5)
la_aListaDePares :: ListAssoc a b -> [(a,b)]
la_aListaDePares Empty = []
la_aListaDePares (Node a b la) = (a,b): (la_aListaDePares la)

--b6)
la_existe :: Eq a => ListAssoc a b -> a -> Bool
la_existe Empty _ = False
la_existe (Node a _ c) x | x == a = True
                         | otherwise = la_existe c x

--b7)
la_agregar :: Eq a => a -> b -> ListAssoc a b -> ListAssoc a b
la_agregar a b Empty = (Node a b Empty)
la_agregar x y (Node a b c) | x == a = Node x y c
                            | otherwise = Node a b (la_agregar x y c)

la_borrar :: Eq a => a -> ListAssoc a b -> ListAssoc a b
la_borrar _ Empty = Empty
la_borrar x (Node a b c) | x == a = la_borrar x c
                         | otherwise = (Node a b (la_borrar x c))  
