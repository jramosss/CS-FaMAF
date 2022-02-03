 module TADMc (MC,addcafe,addchoc,addvaso,hscof,hschoc,hsvaso,expcafe,expchoc,cafecant,choccant,vasocant,addccafe,addcchoc,addcvaso) where

--CONSTRUCTORES 
 	data MC = Vacia
 			 | Addcafe (MC)
 			 | Addchoc (MC)
 			 | Addvaso (MC)

vacia = Vacia
acafe = Addcafe
achoc = Addchoc
avaso = Addvaso

--OPERACIONES

hscof, hschoc, hsvaso :: MC -> Bool
expcafe,expchoc :: MC -> MC
cafecant, choccant, vasocant :: MC -> Nat
addccafe, addcchoc, addcvaso :: Nat -> MC -> MC

--ECUACIONES

hscafe Vacia = "La maquina esta vacia"
hscafe (Addcafe p) = True
hscafe (Addchoc p) = Addchoc (hscafe p)
hscafe (Addvaso p) = Addvaso (hscafe p)

hschoc Vacia = "La maquina esta vacia"
hschoc (Addcafe p) = Addcafe (hschoc p)
hschoc (Addchoc p) = True
hschoc (Addvaso p) = Addvaso (hschoc p)

hsvaso Vacia = "La maquina esta vacia"
hsvaso (Addcafe p) = Addcafe (hsvaso p)
hsvaso (Addchoc p) = Addchoc (hsvaso p)
hsvaso (Addvaso p) = True

expelvaso :: MC -> MC
expelvaso Vacia = "Error Maquina vacia"
expelvaso (Addvaso p) = p
expelvaso (Addcafe p) = Addcafe (expelvaso p)
expelvaso (Addchoc p) = Addchoc (expelvaso p)

expcafe Vacia = "Error maquina vacia"
expcafe (Addcafe p) | (hsvaso p) = (expelvaso p)
			   	    | otherwise = "La maquina no tiene vasos"				  	
expcafe (p) | (hscafe p) && (hsvaso p) = expcafe (expelvaso p)
		    | otherwise = "La mquina no tiene ingredientes"

expchoc Vacia = "Error maquina vacia"
expchoc (Addchoc p) | (hsvaso p) = (expelvaso p)
			   	    | otherwise = "La maquina no tiene vasos"				  	
expchoc (p) | (hschoc p) && (hsvaso p) = expchoc (expelvaso p)
		    | otherwise = "La mquina no tiene ingredientes"

cafecant Vacia = 0
cafecant (Addcafe p) = 1+(cafecant p)
cafecant (Addchoc p) = 0 + (cafecant p)
cafecant (Addvaso p) = 0 + (cafecant p)

choccant Vacia = 0
choccant (Addchoc p) = 1 + (choccant p)
choccant (Addvaso p) = 0 + (choccant p)
choccant (Addcafe p) = 0 + (choccant p)

vasocant Vacia = 0
vasocant (Addvaso p) = 1 + (vasocant p)
vasocant (Addcafe p) = 0 + (vasocant p)
vasocant (Addchoc p) = 0 + (vasocant p)

addccafe 0 p = "Debe ingresar otra cantidad"
addccafe n Vacia = Addcafe (addccafe (n-1) Vacia)
addccafe n p = Addcafe (addccafe (n-1) p)

addcchoc 0 p = "Debe ingresar otra cantidad"
addcchoc n Vacia = Addchoc (addcchoc (n-1) Vacia)
addcchoc n p = Addchoc (addcchoc (n-1) p)

addcvaso 0 p = "Debe ingresar otra cantidad"
addcvaso n Vacia = Addvaso (addcvaso (n-1) Vacia)
addcvaso n p = Addvaso (addcvaso (n-1) p)


 instance MC Eq where
 	Addcafe(Addchoc(m)) = Addchoc(Addcafe(m))
 	Addchoc(Addvaso(m)) = Addvaso(Addchoc(m))
 	Addvaso(Addcafe(m)) = Addcafe(Addvaso(m))


















