--Ejercicio 1
--a)
data Carrera = Matematica | Fisica | Computacion | Astronomia | Profesorado
      deriving (Eq,Show)

--b)
titulo :: Carrera -> String
titulo Matematica = "Licenciatura en Matematica"
titulo Fisica     = "Licenciatura en Fisica"
titulo Astronomia = "Licenciatura en Astronomia"
titulo Computacion = "Licenciatura en Ciencias de la Computacion"
titulo Profesorado = "Profesorado en ..."

--c)
titulorec :: Carrera -> String
titulorec c = case c of
                Matematica -> "Licenciatura en Matematica"
                Fisica -> "Licenciatura en Fisica"
                Astronomia -> "Licenciatura en Astronomia"
                Profesorado -> "Profesorado en ..."
                Computacion -> "Licenciatura en Ciencias de la Computacion"

--Ejercicio 2 
--a)
type Ingreso = Int
data Cargo = Titular | Asociado | Adjunto | Asistente | Auxiliar
            deriving (Eq,Show)
data Area = Administrativa | Ensenanza | Economica | Postgrado
            deriving (Eq,Show)
data Rol = Decanx Genero 
         | Docente Cargo 
         | NoDocente Area
         | Estudiante [(Carrera, Ingreso)]
          deriving (Eq,Show)     
         

-- b) Cargo -> Rol
--c)
sosBoleta :: Cargo -> Cargo -> Bool
sosBoleta Titular Titular = True
sosBoleta Asociado Asociado = True
sosBoleta Adjunto Adjunto = True
sosBoleta Asistente Asistente = True
sosBoleta Auxiliar Auxiliar = True
sosBoleta _ _ = False 

sosCareta :: Cargo -> Rol -> Bool
sosCareta x (Docente c)  = sosBoleta c x
sosCareta _ _ = False

cuantos_doc :: [Rol] -> Cargo -> Int
cuantos_doc [] _ = 0
cuantos_doc (x:xs) c | sosCareta c x = (1+cuantos_doc xs c)
                     | otherwise = cuantos_doc xs c

--d)
cuantosdocfilter :: [Rol] -> Cargo -> Int
cuantosdocfilter [] _ = 0
cuantosdocfilter xs c = length (filter (sosCareta  c) xs) 

--e)
data Genero = Hombre | Mujer | Nobinario
           deriving (Eq,Show)

--f)
--Estudiante [(Carrera, Ingreso)]
estudia :: Rol -> Carrera -> Bool
estudia (Estudiante ((Matematica, _):_)) Matematica = True
estudia (Estudiante ((Fisica, _):_)) Fisica = True
estudia (Estudiante ((Astronomia, _):_)) Astronomia = True
estudia (Estudiante ((Computacion, _):_)) Computacion = True
estudia (Estudiante ((Profesorado, _):_)) Profesorado = True
estudia (Estudiante (_:xs)) c = estudia ( Estudiante xs) c
estudia _ _ = False
--Ejercicio 3
--a)
data Persona = Per String String Int Int Int Int Rol
              deriving (Eq,Show)


--b)


--c)
edad :: Persona -> (Int, Int, Int) -> Int
edad (Per _ _ x y z _ _) (d,m,a) | y < m = abs(z - a)
                                 | y == m && x <= d = a - z
                                 | otherwise = a - z - 1
                            
existe :: String -> [Persona] -> Bool
existe _ [] = False
existe apellido ((Per ap _ _ _ _ _ _):xs) = apellido == ap || existe apellido xs 

esRol :: Persona -> Rol
esRol (Per _ _ _ _ _ _ r) = r

est_astronomia :: [Persona] -> [Persona]
est_astronomia [] = []
est_astronomia (x:xs) = filter (\z -> estudia (esRol z) Astronomia) (x:xs) 

padron_nodocente :: [Persona] -> [(String, Int)]
padron_nodocente [] = []
padron_nodocente ((Per ape nom dni _ _ _ (NoDocente _)):xs) = ((ape++ " " ++ nom), dni) : padron_nodocente xs
padron_nodocente (_:xs) = padron_nodocente xs

--Ejercicio 4
--a)	
data Cola = Vacia | Encolada Persona Cola
            deriving (Eq,Show)
atender :: Cola -> Cola
atender Vacia = Vacia
atender (Encolada _ xs) = xs

--b)
encolar :: Persona -> Cola -> Cola
encolar p Vacia = Encolada p Vacia
encolar p (Encolada x xs) = (Encolada x (encolar p xs))

--c)
busca :: Cola -> Cargo -> Persona
busca Vacia _ = error "Nadie cumple esos Requisitos"
busca (Encolada p c) k | (esRol p) == Docente k = p
                       | otherwise = busca c k

--d) El concepto de Cola es semejante al de Lista, ya que sus constructores pueden ser o Vacia, o puede contener un elemento que le va a a agregar a la lista, al igual que Encolada Persona Cola.

--d1)
atenderlista :: [Persona] -> [Persona]
atenderlista [] = []
atenderlista (_:xs) = xs

--d2)
encolarlista :: Persona -> [Persona] -> [Persona]
encolarlista _ [] = []
encolarlista p xs = xs ++ [p]

--d3)
buscalista :: [Persona] -> Cargo -> Persona
buscalista [] _ = error "No hay ninguna persona en la lista"
buscalista (x:xs) c | (esRol x) == Docente c = x
                    | otherwise = buscalista xs c
             
--Ejercicio 5
data ListaAsoc a b = Empty | Nodo a b ( ListaAsoc a b )
                    deriving (Eq,Show)

type Diccionario = ListaAsoc String String
type Padron= ListaAsoc Int String
--a)
--a deberia ser un Int, siendo este el numero de telefono y b deberia ser de tipo String indicando Nombre
type GuiaTelefonica = ListaAsoc Int String

--b1)
la_long :: Integral c => ListaAsoc a b -> c
la_long Empty = 0
la_long (Nodo _ _ c) = 1 + la_long c

--b2)
la_concat :: (Eq a, Eq b) => ListaAsoc a b -> ListaAsoc a b -> ListaAsoc a b
la_concat Empty k = k
la_concat (Nodo a b c) d = (Nodo a b (la_concat c d ))


--b3)
la_pares :: ListaAsoc a b -> [(a,b)]
la_pares Empty = []
la_pares (Nodo a b c) = (a,b) : la_pares c

--b4)
la_busca :: Eq a => ListaAsoc a b -> a -> Maybe b
la_busca Empty _ = Nothing
la_busca (Nodo a b c) x |x==a = Just b 
                        |otherwise = la_busca c x

--b5)
la_aListaDePares :: ListaAsoc a b -> [(a,b)]
la_aListaDePares Empty = []
la_aListaDePares (Nodo a b la) = (a,b): (la_aListaDePares la)

--c) 
encuentra :: Eq a => a -> ListaAsoc a b -> b
encuentra _ Empty = error "Lista Vacia"
encuentra x (Nodo a b c) | x == a = b
                         | otherwise = encuentra x c

skere :: Int -> [Int] -> Int
skere _ [] = 0
skere n (x:xs) =  max 0 skere (n+x) xs



