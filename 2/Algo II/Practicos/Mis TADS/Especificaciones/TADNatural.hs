module TADNatural where
data Natt = C | S Natt

mas :: Natt -> Natt -> Natt
mas C x = x
mas (S x) y = S (mas x y)

por :: Natt -> Natt -> Natt
por C _ = C
por (S x) y = S(por x y)

ala :: Natt -> Natt -> Natt
ala C C = error "undefined"
ala C x = C
ala x C = (S C)
ala (S x) y = S (ala x y)

minus :: Natt -> Natt -> Bool
minus C C = False
minus C x = True
minus x C = False
--minus S(x) y = 