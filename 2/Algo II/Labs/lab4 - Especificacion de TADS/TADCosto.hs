-- TAD Costo

module TADCosto(Costo, finitoC, infinitoC, masC) where

import TADNatural

-- CONSTRUCTORES

data Costo = Finito Natural 
           | Infinito
           deriving (Eq, Ord, Show)

-- CONSTRUCTORES

finitoC = Finito
infinitoC = Infinito

-- OPERACIONES

masC :: Costo -> Costo -> Costo

-- ECUACIONES

Finito n `masC` Finito m = Finito (n `mas` m)
_ `masC` _ = Infinito



