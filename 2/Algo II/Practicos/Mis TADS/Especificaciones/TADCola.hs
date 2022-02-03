module TADCola where
import TADNatural
data Queque e = E | Enqueque (Queque e) e

lastt :: Queque e -> e
lastt E = error "Empty Queque"
lastt (Enqueque (q) e) =  lastt q

dequeque :: Queque e -> Queque e
dequeque E  = error "Empty Queque"
dequeque (Enqueque (q) e) = Enqueque (dequeque (q))

is_empty :: Queque e -> Bool
is_empty E = True
is_empty _ = False

firstt :: Queque e -> e
firstt E = error "Empty Queque"
firstt (Enqueque (q) e) = firstt q  
