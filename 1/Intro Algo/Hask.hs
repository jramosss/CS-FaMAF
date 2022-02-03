--chequearSuma :: (Int, Int, Int) -> Bool
--chequearSuma [] = []
--chequearSuma (x, y, z)| (x+y) == z == True
 --                     | (x+y) /= z == False

cardinal :: [[a]] -> [Int]
cardinal [] = 0
cardinal ((x:xs): ys) = #(x:xs) : cardinal ys