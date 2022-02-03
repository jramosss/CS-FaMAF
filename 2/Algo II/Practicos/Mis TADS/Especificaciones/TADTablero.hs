module TADTablero where
data Tablero = I | MarcarA Tablero | MarcarB Tablero

is_init :: Tablero -> Bool
is_init I = True
is_init _ = False

marcoA :: Tablero -> Bool
marcoA I = False
marcoA (MarcarA t) = True
marcoA t = marcoA t

marcoB :: Tablero -> Bool
marcoB I = False
marcoB (MarcarB t) = True
marcoB t = marcoB t

contadorA :: Tablero -> Int
contadorA I = 0
contadorA (MarcarA t) = 1 + contadorA t
contadorA t = contadorA t

contadorB :: Tablero -> Int
contadorB I = 0
contadorB (MarcarB t) = 1 + contadorB t
contadorB t = contadorB t

ganaA :: Tablero -> Bool
ganaA I = False
ganaA t = (contadorB t) < (contadorA t) 

ganaB :: Tablero -> Bool
ganaB I = False
ganaB t = (contadorB t) > (contadorA t)

--tie :: Eq Int => Tablero -> Bool
--tie I = True
--tie t = contadorB == contadorA

agregaNA :: Tablero -> Nat -> Tablero
agregaNA t 0 = t
agregaNA I n = MarcarA(agregaNA I (n-1))
agregaNA t n = MarcarA(agregaNA t (n-1))

agregaNB :: Tablero -> Nat -> Tablero
agregaNB t 0 = t
agregaNB I n = MarcarB(agregaNB I (n-1))
agregaNB t n = MarcarB(agregaNB t (n-1))

restaNA :: Tablero -> Nat -> Tablero
restaNA I _ = error "Tablero Vacio"
restaNA t 0 = t
restaNA (MarcarA t) n = restaNA t (n-1)
restaNA (MarcarB t) n = MarcarB(restaNA t (n-1))

restaNB :: Tablero -> Nat -> Tablero
restaNB I _ = error "Tablero Vacio"
restaNB t 0 = t
restaNB (MarcarA t) n = MarcarA(restaNB t (n-1)) -- Como serian los casos aca?
restaNB (MarcarB t) n = (restaNB t (n-1))

instance Tablero Eq where
	MarcarA(MarcarB) = MarcarB(MarcarA)