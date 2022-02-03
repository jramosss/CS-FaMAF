module TADEntero where
import TADNatural
data Entero = Pos Natt | Neg Natt

sucE :: Entero -> Entero
sucE (Pos x) = Pos (succ x)
sucE (Neg x) | x == C = Pos (S (C))
			 | otherwise = Neg (pre x)

preE :: Entero -> Entero
preE (Pos x) | x == C = (Neg (pre x))
 			 | otherwise = Pos (pre x)
preE (Neg x) = Neg (pre x) 

suma :: Entero -> Entero -> Entero
suma (Pos x) (Pos y) = mas x y
suma (Pos x) (Neg y) | x >= y = Pos (menos x y)
					 | otherwise = Neg (menos y x)
suma (Neg x) (Neg y) | y >= x  = Pos (mas x y)
					 | otherwise = Neg (mas x y)

