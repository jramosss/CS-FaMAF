import TADNatural

uno = suc cero
dos = suc uno
tres = suc dos

cinco = dos `mas` tres
diez = dos `por` cinco

milveinticuatro = dos `ala` diez

fromInt :: Int -> Natural
fromInt 0 = cero
fromInt n | n > 0 = suc (fromInt (n-1))
          | otherwise = error "No es un número natural"

toInt :: Natural -> Int
toInt n = buscar n 0
        where buscar n m = if n == fromInt m then m
                           else buscar n (m+1)

