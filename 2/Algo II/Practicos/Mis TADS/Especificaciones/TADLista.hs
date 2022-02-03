module TADLista where
import TADNatural   
data List e = E | Cons e (List e)
        deriving (Eq,Show)
insert :: e -> List e -> List e
insert x E = Cons x (E)
--insert x Cons e l = 

is_empty :: List e -> Bool
is_empty E = True
is_empty _ = False

length2 :: List e -> Natt
length2 E = C
length2 (Cons e l) = S (length2 l)

head :: List e -> e
head E = error "Empty List"
head (Cons e l) = e

tail :: List e -> List e
tail E = error "Empty List"
tail (Cons e l) = l

concatt :: List e -> List e -> List e
concatt E x = x
concatt x (Cons e l) = Cons e (concatt x l)

take :: List e -> Natt -> e
take E _ = error "Empty List"
--take Cons e l x = 