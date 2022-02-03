--Ejercicio 1
-- a)
 esCero :: Int -> Bool
 esCero x = x==0 
           
--b)
 esPositivo :: Int -> Bool 
 esPositivo x = x>0 

--c)
 esVocal :: Char -> Bool
 esVocal x = x== 'a' || x== 'e' || x== 'i' || x== 'o'|| x== 'u'

 --Ejercicio 2
 --a)
 paratodo :: [Bool] -> Bool
 paratodo [] = True
 paratodo (x:xs) = x  && paratodo xs
                  
--b)
 sumatoria :: [Int] -> Int
 sumatoria [] = 0
 sumatoria (x:xs) = x + sumatoria xs

 --c)
 productoria :: [Int] -> Int
 productoria [] = 1
 productoria (x:xs) = x * productoria xs

 --d)
 factorial :: Int -> Int
 factorial 0 =  1
 factorial n = n * factorial (n-1)
 --e)
 promedio :: [Int] -> Int
 promedio [] = 0
 promedio (x:xs) = (x+ sumatoria xs) `div` length (x:xs)

 --Ejercicio 3
 pertenece :: Int -> [Int] -> Bool
 pertenece _ [] = False
 pertenece n (x:xs) | n==x || pertenece n xs
                   

--Ejercicio 4
 encuentra :: Int -> [(Int, String)] -> String
 encuentra _ [] = ""
 encuentra n ((x,y):xs) | (x==n) = y
                        | otherwise = encuentra n xs

--Ejercicio 5
--a)		
 paratodo' :: [a] -> (a -> Bool) -> Bool
 paratodo' [] _ = True
 paratodo' (x:xs) t | ((t x) == True) = paratodo' xs t
                    | otherwise = False

--b)
 existe' :: [a] -> (a -> Bool) -> Bool
 existe' [] _ = False
 existe' (x:xs) t | t x  = True
                  | otherwise = existe' xs t

--c)
 sumatoria' :: [a] -> (a -> Int) -> Int
 sumatoria' [] _ = 0
 sumatoria' (x:xs) t = (t x) + sumatoria' xs t

--d)
 productoria' :: [a] -> (a -> Int) -> Int
 productoria' [] _ = 0
 productoria' (x:xs) t = t x * productoria' xs t

--Ejercicio 6
 paratodo2 :: [Bool] -> Bool
 paratodo2 xs = paratodo' xs id

--Ejercicio 7
--a)
 todosPares :: [Int] -> Bool
 todosPares xs = paratodo' xs (\x -> mod x 2 == 0)

--b)
 hayMultiplo :: Int -> [Int] -> Bool
 hayMultiplo n xs = existe' xs  (\ x -> mod x n == 0)

--c)
 sumaCuadrados :: Int -> Int
 sumaCuadrados n = sumatoria' [0..n] t
                   where t x = x*x

--d) 
 factorial2:: Int -> Int
 factorial2 n = productoria [1..n]

--e)
 multiplicaPares :: [Int] -> Int
 multiplicaPares xs = productoria (filter (\ x -> mod x 2 == 0) xs)

--Ejercicio 8
--a) La funcion "filter" toma un predicado f, una lista xs, y devuelve la lista de todos los elementos que cumplen con el predicado f.
--La funcion "map" toma un elemento f, una lista xs y devuelve la lista obtenida de aplicar f a cada elemento de esta.

-- Esta funcion va a aplicar la funcion succ para cada elemento de la lista xs y va a devolver la lista modificada, osea quedaria [2,-3,7,3,-7]

--Esta funcion va a devolvernos los elementos de la lista xs que sean positivos, osea [1,6,2]

--Ejercicio 9
--a)
 duplica :: [Int] -> [Int]
 duplica [] = []
 duplica (x:xs) = x*2 : duplica xs 
--b)
 duplica2 :: [Int] -> [Int]
 duplica2 xs = map (\ x -> x*2)  xs

--Ejercicio 10
--a) 
 soloPares :: [Int] -> [Int]
 soloPares [] = []
 soloPares (x:xs) | mod x 2 == 0 = x : soloPares xs
                  | otherwise = soloPares xs
--b)
 soloPares2 ::  [Int] -> [Int]
 soloPares2 xs = filter even xs

--Ejercicio 11
--a)
 sumarALista :: Num a => a -> [a] -> [a]
 sumarALista _ [] = []
 sumarALista n (x:xs) = (x+n) : sumarALista n xs

 encabezar :: a -> [[a]] -> [[a]]
 encabezar _ [] = []
 encabezar n (x:xs) = (n :x) : encabezar n xs

 mayoresA :: Ord a => a -> [a] -> [a]
 mayoresA _ [] = []
 mayoresA n (x:xs) | (x>n)  = x : (mayoresA n xs)
                   | otherwise = mayoresA n xs

--b)
 sumarAListamap :: Num a => a -> [a] -> [a]
 sumarAListamap _ [] =  []
 sumarAListamap n (x:xs) = map (\ y -> (y+n)) (x:xs)

 encabezarmap :: a -> [[a]] -> [[a]] 
 encabezarmap _ [] = []
 encabezarmap n (x:xs) = map (\y -> (n:y)) (x:xs)

 mayoresAfilter :: Ord a => a -> [a] -> [a]
 mayoresAfilter _ [] = []
 mayoresAfilter n (x:xs) = filter (\y -> x>n) (x:xs)

--Ejercicio 12
 existe :: Int -> (Int, String) -> Bool
 existe n (a,b)  | a==n = True
                 | otherwise = False

 --findFilter :: Int -> [(Int,String)] -> [(Int,String)]
 --findFilter n ((x,y):xs)= filter (existe n) ((x,y)):xs)

 --encuentra2 :: (a,String) -> String
 --encuentra2 (_,y) = ""
 --encuentra2 (a,y) = 
 --encuentra2 :: Int -> [Int, String] -> String
 --encuentra2 n (
--Ejercicio 16

--a)

--f :: (a,b) ->
--f x = ...
--Esta funcion no tipa por que esta definida como que toma una dupla y solo le brinda un int de argumento

--b)

--f :: (a,b) -> ...
--f (x,y) = ...
--A diferencia de la funcion anterior, esta si esta bien definida ya que pide una tupla y se le esta dando una tupla como argumento

--c)

--f :: [(a,b)] -> ...
--f (a,b) = ...
--Esta funcion no tipa por que en la definicion de la funcion pide una lista de tuplas, y el argumento dado es una unica tupla

--d)

--f :: [(a,b)] -> ...
--f (x:xs) = ...
--Esta funcion no tipa ya que pide una lista de tuplas y se le esta dando una lista de algo que no necesariamente sea una tupla

--e)

--f :: [(a,b)] -> ...
--f ((x,y) : ((a,b) : xs)) = ...
--Esta funcion no cubre todos los casos, estaria bien definida si fuese f ((x,y):xs), ya que la segunda tupla puede ser vacia y el caso que plantea el e implica tener 2 tuplas no vacias

--f)

--f :: [(Int,a)] -> ...
--f [(0,a)] = ...
--Esta funcion no tipa por 2 motivos, esta asumiendo que el entero es 0, por lo tanto no consiera los casos donde Int /= 0 y es una lista de tuplas, por  lo que le falta el resto de las tuplas, osea [(n,a):xs)]

--g)

--f :: [(Int,a]) -> ...
--f ((x,1):xs) = ...
--Esta funcion no tipa ya que no esta considerando los casos donde a /= 1
 
--h)

--f :: [(Int,a)] -> ...
--f ((1,x) : xs) = ...
--
