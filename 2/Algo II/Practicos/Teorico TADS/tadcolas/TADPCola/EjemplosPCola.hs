import TADPCola

-- EJEMPLOS

primera, segunda, tercera :: PCola Int

primera = vacía
segunda = encolar primera 2
tercera = encolar segunda 5

seg = encolar primera 5
ter = encolar seg 2

todas :: [PCola Int]
todas = vacía : concatMap (\q -> map (encolar q) [0..9]) todas

pcola_sort :: Ord a => [a] -> [a]
pcola_sort as = decolar_todo [] (encolar_todo as)

encolar_todo :: Ord a => [a] -> PCola a
encolar_todo [] = vacía
encolar_todo (a:as) = encolar (encolar_todo as) a

decolar_todo :: Ord a => [a] -> PCola a -> [a]
decolar_todo sorted q = if es_vacía q then sorted
                        else decolar_todo (primero q:sorted) (decolar q)


