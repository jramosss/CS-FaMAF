import TADPila

-- EJEMPLOS

primera = vacía
segunda = apilar 2 primera
tercera = apilar 5 segunda

todos = vacía : concatMap (\p -> [apilar ')' p, apilar ']' p, apilar '}' p, apilar '>' p]) todos

cierto = segunda == desapilar tercera
falso = tercera == apilar 5 vacía

-- DELIMITADORES BALANCEADOS

balanceados = ctrl_balanceo vacía

ctrl_balanceo p [] = es_vacía p
ctrl_balanceo p (l:s) | izq l = ctrl_balanceo (apilar (cierre l) p) s
                      | der l && es_vacía p = False
                      | der l && l /= primero p = False
                      | der l && l == primero p = ctrl_balanceo (desapilar p) s
                      | otherwise = ctrl_balanceo p s

izq l = l `elem` ['(','[','{','<']

der l = l `elem` [')',']','}','>']

cierre '(' = ')'
cierre '[' = ']'
cierre '{' = '}'
cierre '<' = '>'

