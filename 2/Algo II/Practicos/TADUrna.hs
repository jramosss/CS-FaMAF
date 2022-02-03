module TADUrna where
data Urna = Vacia | VotarA Urna | VotarB Urna | VotarN Urna
    deriving(Show)

is_empty :: Urna -> Bool
is_empty Vacia = True
is_empty _ = False

cant_votosA :: Urna -> Int
cant_votosA Vacia = 0
cant_votosA (VotarA u) = 1 + cant_votosA u
cant_votosA (VotarB u) = cant_votosA u

cant_votosB :: Urna -> Int
cant_votosB Vacia = 0
cant_votosB (VotarB u) = 1 + cant_votosB u 
cant_votosB (VotarA u) = cant_votosB u

cant_votos :: Urna -> Int 
cant_votos Vacia = 0
cant_votos u = cant_votosA u + cant_votosB u

hayvotoA :: Urna -> Bool 
hayvotoA Vacia = False
hayvotoA (VotarA u) = True
hayvotoA (VotarB u) = hayvotoA u 

hayvotoB :: Urna -> Bool 
hayvotoB Vacia = False
hayvotoB (VotarB u) = True
hayvotoB (VotarA u) = hayvotoB u 

ganaA :: Urna -> Bool 
ganaA Vacia = False
ganaA u = cant_votosA u > cant_votosB u

ganaB :: Urna -> Bool
ganaB Vacia = False
ganaB u = cant_votosB u > cant_votosA u

tie :: Urna -> Bool
tie Vacia = True
tie u = cant_votosA u == cant_votosB u

anularA :: Urna -> Urna
anularA Vacia = error "Urna Vacia"
anularA (VotarA u) = u
anularA (VotarB u) = anularA (VotarB u)

anularB :: Urna -> Urna
anularB Vacia = error "Urna Vacia"
anularB (VotarB u) = u
anularB (VotarA u) = anularB (VotarA u)

join :: Urna -> Urna -> Urna
join Vacia x = x
join (VotarA u1) u2 = VotarA (join u1 u2) 
join (VotarB u1) u2 = VotarB (join u1 u2)

votarN :: Urna -> Urna
votarN Vacia = (VotarN Vacia)
votarN (VotarA u) = VotarN(VotarA u)
votarN (VotarB u) = VotarB(VotarN u)

--instance Eq => Urna where
--VotarA(VotarB u) == VotarB(VotarA u)