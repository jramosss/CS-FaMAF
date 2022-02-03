-- TAD Natural

module TADNatural(Natural, cero, suc, mas, por, ala) where

-- CONSTRUCTORES

data Natural = C 
              | S Natural
              deriving (Eq, Ord, Show)


-- CONSTRUCTORES

cero = C
suc = S

-- OPERACIONES

mas, por, ala :: Natural -> Natural -> Natural

-- ECUACIONES

C `mas` m = m
S n `mas` m = S (n `mas` m)

C `por` m = C
S n `por` m = m `mas` (n `por` m)

C `ala` C = error "potencia indefinida"
n `ala` C = S C
n `ala` S m = n `por` (n `ala` m)


