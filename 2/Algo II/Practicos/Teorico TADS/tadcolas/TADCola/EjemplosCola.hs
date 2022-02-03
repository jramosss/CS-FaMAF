import TADCola

-- EJEMPLOS

primera = vacía
segunda = encolar primera 2
tercera = encolar segunda 5

todos = vacía : concatMap (\q -> map (encolar q) [0..9]) todos

falso = segunda == decolar tercera

buffer :: Cola String -> [String] -> [String]
buffer q [] = ["Fin del buffer\n"]
buffer q (('p':s):ls) = ("Ingresa \"" ++ s ++ "\" al buffer") : buffer (encolar q s) ls
buffer q (('c':s):ls) | es_vacía q = "Se intenta consumir desde buffer vacío" : buffer q ls
                      | otherwise = ("Sale \"" ++ primero q ++ "\" del buffer") : buffer (decolar q) ls
buffer q (_:ls) = buffer q ls

buff = interact (unlines . buffer vacía . lines)

main = buff

