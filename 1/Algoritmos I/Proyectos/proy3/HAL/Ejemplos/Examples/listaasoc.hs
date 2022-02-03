module Language.ListaAsoc where
data ListaAsoc a b = Empty
                  | Node a b (ListaAsoc a b)
                   
la_long :: Integral c => ListaAsoc a b -> c
la_long = undefined

la_aListaDePares :: ListaAsoc a b -> [(a,b)]
la_aListaDePares = undefined

la_existe :: Eq a => ListaAsoc a b -> a -> Bool
la_existe = undefined

la_buscar :: Eq a => ListaAsoc a b -> a -> Maybe b
la_buscar = undefined

la_agregar :: Eq a => a -> b -> ListaAsoc a b -> ListaAsoc a b
la_agregar = undefined

la_borrar :: Eq a => a -> ListaAsoc a b -> ListaAsoc a b
la_borrar = undefined


