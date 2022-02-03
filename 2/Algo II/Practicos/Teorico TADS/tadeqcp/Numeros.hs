
import TADNatural
import TADEntero
import TADRacional

toInt :: Natural -> Int
toInt n | n == cero = 0
        | otherwise = toInt (pre n) + 1

fromInt :: Int -> Natural
fromInt 0 = cero
fromInt n | n > 0 = suc (fromInt (n-1))
          | otherwise = error "no es un natural"

toIntE :: Entero -> Int
toIntE z | z < ceroE = -toInt (absE z)
         | otherwise = toInt (absE z)

fromIntE :: Int -> Entero
fromIntE z | z >= 0 = posE (fromInt z)
           | otherwise = negE (fromInt (-z))

instance Show Natural where
  show n = show (toInt n)

instance Show Entero where
  show n = show (toIntE n)

instance Show Racional where
  show (Fraccion a b) = show a ++ "/" ++ show b

