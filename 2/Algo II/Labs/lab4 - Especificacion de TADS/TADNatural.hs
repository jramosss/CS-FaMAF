-- TAD Natural

module TADNatural(Natural, cero, suc, pre, mas, menos, por, coc, ala) where

-- CONSTRUCTORES

data Natural = C 
              | S Natural
              deriving (Eq, Ord)

-- CONSTRUCTORES

cero = C
suc = S

-- OPERACIONES

pre :: Natural -> Natural
mas, menos, por, coc, ala :: Natural -> Natural -> Natural
-- pre no se aplica al cero
-- menos se aplica solo si el minuendo es mayor o igual que el sustraendo
-- ala no se aplica si ambos argumentos son cero
-- coc no se aplica si el divisor es cero

-- ECUACIONES

pre C = error "predecesor indefinido"
pre (S n) = n

C `mas` m = m
S n `mas` m = S (n `mas` m)

n `menos` C = n
C `menos` S m = error "substracción indefinida"
S n `menos` S m = n `menos` m

C `por` m = C
S n `por` m = m `mas` (n `por` m)

n `coc` C = error "división indefinida"
n `coc` m@(S _) | n >= m = S ((n `menos` m) `coc` m)

C `ala` C = error "potencia indefinida"
n `ala` C = S C
n `ala` S m = n `por` (n `ala` m)

instance Show Natural where
  show n = show (aint n)
         where aint C = 0
               aint (S k) = 1 + aint k

