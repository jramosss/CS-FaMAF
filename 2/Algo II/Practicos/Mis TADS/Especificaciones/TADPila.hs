module TADPila where
data Stack e = I | Apilar e (Stack e)
            deriving (Eq,Show)

headd :: Stack e -> e
headd I = error "Empty Stack"
headd (Apilar e (q)) = e

lastt :: Stack e -> e
lastt I = error "Empty Stack"
lastt (Apilar e s) = lastt s 

initt :: Stack e -> Stack e
initt _ = I

is_init :: Stack e -> Bool
is_init I = True
is_init _ = False

desapilar :: Stack e -> Stack e
desapilar I = error "Empty Stack"
desapilar (Apilar e s) = s

invertir :: Stack e -> Stack e
invertir I = I
invertir (Apilar e s) = (Apilar (lastt s) (s))