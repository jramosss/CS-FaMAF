module TADArbol where
data Arbol = Vacio | Arbol (Izquierdo Arbol Derecho)

nodes :: Arbol -> Int
nodes Vacio = 0
nodes Arbol (i r d) = 1 + nodes r

prof :: Arbol -> Arbol -> Int
prof Vacio Vacio = 0
prof Arbol(Izq) Vacio = 
prof Arbol(i r d) x =

nivel :: Arbol -> Int -> List
nivel Vacio _ = 0
nivel Arbol(i r d) x = 
