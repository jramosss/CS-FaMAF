import TADContador

-- EJEMPLOS

primero = inicial
segundo = incrementar primero
tercero = incrementar segundo
cuarto = incrementar tercero
quinto = incrementar cuarto
sexto = incrementar quinto
séptimo = incrementar sexto
octavo = incrementar séptimo
noveno = incrementar octavo
décimo = incrementar noveno

todos = inicial : map incrementar todos

cierto = segundo == decrementar tercero
falso = tercero == decrementar (decrementar sexto)

-- PARÉNTESIS BALANCEADOS

balanceados :: String -> Bool
balanceados = ctrl_balanceo inicial

ctrl_balanceo :: Contador -> String -> Bool
ctrl_balanceo c [] = es_inicial c
ctrl_balanceo c (l:s) | l == '(' = ctrl_balanceo (incrementar c) s
                      | l == ')' && es_inicial c = False
                      | l == ')' = ctrl_balanceo (decrementar c) s
                      | otherwise = ctrl_balanceo c s


