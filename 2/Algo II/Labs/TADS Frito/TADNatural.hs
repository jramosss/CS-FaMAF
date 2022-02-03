
module TADNatural (Natural,c,suc,mas,por,pot,men,menig,may,mayig,ig) where
    
    data Natural= Cero
                | Sucesor Natural

-- Operaciones
mas:: Natural -> Natural -> Natural
por, pot :: Natural -> Natural -> Natural
ig, may, mayig, men, menig, ig :: Natural -> Natural -> Bool

-- Ecuaciones

Cero `mas` x = x
Sucesor x `mas` y = Sucesor (x`mas`y)

Cero `por` x = Cero
Sucesor Cero `por` x = x
Sucesor x `por`y = Sucesor (x`por`y)

Cero `pot` Cero = "Indeterminado"
x `pot` Cero = Sucesor Cero
Sucesor x `pot` y = Sucesor (x`pot`y)

Cero `ig` Cero = True
Sucesor x `ig` Sucesor y = x `ig` y

Sucesor x `may` Cero = True
Sucesor x `may` y = Sucesor (x `may` y)

Cero `mayig` Cero = True
Sucesor x `mayig` Cero = True
Sucesor x `mayig` y | x `may` y = True    
                    | Sucesor x `ig` y = True
                    | otherwise = False

Sucesor x `men` Cero = False
Sucesor x `men` y = x `men` y

Cero `menig` Cero = True
Sucesor x `menig` Cero = False
Sucesor x `menig` y | x `men` y = True
                    | Sucesor x `ig` y = True
                    | otherwise = False
