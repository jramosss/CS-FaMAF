-- TAD Matriz

module TADMatriz (Matriz, iniM, asignarM, tamM, consultarM) where

import Defecto
import TADNatural

instance Defecto Natural where
  defecto = cero

-- CONSTRUCTORES

data Matriz e = Ini Natural | Asignar (Matriz e) Natural Natural e
--              deriving Show

-- ECUACIONES ENTRE CONSTRUCTORES	
-- Asignar (Asignar m i j f) i j e == Asignar m i j e
-- si i /= k o j /= l entonces:
--        Asignar (Asignar m k l f) i j e == Asignar (Asignar m i j e) k l f

iniM :: Natural -> Matriz e
iniM = Ini

asignarM :: Matriz e -> Natural -> Natural -> e -> Matriz e
asignarM m i j e | i < tamM m && j < tamM m = Asignar m i j e
                 | otherwise = error "índice fuera de rango"

-- OPERACIONES

tamM :: Matriz e -> Natural
consultarM :: Defecto e => Matriz e -> Natural -> Natural -> e

-- ECUACIONES

tamM (Ini i) = i
tamM (Asignar m i j e) = tamM m

consultarM (Ini i) j k | j < i && k < i = defecto
                       | otherwise = error "Fuera de rango"
consultarM (Asignar m i j e) k l | k == i && l == j = e
                                 | otherwise = consultarM m k l

elim :: Matriz e -> Natural -> Natural -> Matriz e
elim (Ini i) k l = Ini i
elim (Asignar m i j e) k l | i == k && j == l = elim m k l
                           | otherwise = Asignar (elim m k l) i j e

es_ini :: (Defecto e, Eq e) => Matriz e -> Bool
es_ini (Ini i) = True
es_ini (Asignar m i j e) = e == defecto && es_ini (elim m i j)

instance (Defecto e, Eq e) => Eq (Matriz e) where
  Ini i == m = es_ini m
  Asignar n i j e == m = consultarM m i j == e && elim n i j == elim m i j

instance (Defecto e, Show e) => Show (Matriz e) where
  show m = showfilas m cero cero (tamM m)

showfilas m i j t | i < t && j < t = show (consultarM m i j) ++ " "
                                     ++ showfilas m (suc i) j t
                  | i == t && j < t = "\n" ++ showfilas m cero (suc j) t
                  | j == t = ""

