module TADMC where
data CM = E | AddCa CM | AddCh CM | AddCu CM

has_coff :: CM -> Bool
has_coff E = False
has_coff (AddCa c) = True
has_coff (AddCh c) = has_coff c
has_coff (AddCu c) = has_coff c

has_ch :: CM -> Bool
has_ch  E = False
has_ch (AddCh c) = True
has_ch (AddCu c) = has_ch c
has_ch (AddCa c) = has_ch c

has_cu :: CM -> Bool
has_cu E = False
has_cu (AddCu _) = True
has_cu (AddCh c) = has_cu c
has_cu (AddCa c) = has_cu c

is_init :: CM -> Bool
is_init E = True
is_init _ = False

chau_vaso :: CM -> CM
chau_vaso E = error "Empty Machine"
chau_vaso (AddCu c) = c
chau_vaso (AddCh c) = (AddCh (chau_vaso c))
chau_vaso (AddCa c) = (AddCa (chau_vaso c))

chau_cafe :: CM -> CM
chau_cafe E = error "Empty Machine"
chau_cafe (AddCa c) | has_cu c = c
                    | otherwise = error "No hay vaso :("
chau_cafe m | has_cu m && has_coff m = chau_cafe(chau_vaso m)
            | otherwise = error "No hay"

chau_choc :: CM -> CM
chau_choc E = error "Empty Machine"
chau_choc (AddCh c) | has_cu c = c
                    | otherwise = error "No hay vaso :("
chau_choc m | has_cu m && has_coff m = chau_choc(chau_vaso m)
            | otherwise = error "No hay"    


give_coff :: CM -> CM
give_coff E = (AddCa E)
give_coff c = (AddCa (give_coff c))
give_coff (AddCh c) = give_coff c
give_coff (AddCu c) = c 

cuanto_cafe :: CM -> Int 
cuanto_cafe E = 0
cuanto_cafe (AddCa c) = 1 + (cuanto_cafe c)
cuanto_cafe (AddCh c) = cuanto_cafe c
cuanto_cafe (AddCu c) = cuanto_cafe c 

cuanto_choc :: CM -> Int
cuanto_choc E = 0
cuanto_choc (AddCh c) = 1 + cuanto_choc c
cuanto_choc (AddCa c) = cuanto_choc c
cuanto_choc (AddCu c) = cuanto_choc c

cuantos_cu :: CM -> Int
cuantos_cu E = 0
cuantos_cu (AddCu c) = 1 + cuantos_cu c
cuantos_cu (AddCa c) = cuantos_cu c
cuantos_cu (AddCh c) = cuantos_cu c

agregaNCafe :: CM -> Int -> CM
agregaNCafe E n = AddCa (agregaNCafe E n-1)
agregaNCafe m n = (AddCa(agregaNCafe m n-1))

agregaNChoc :: CM -> Int -> CM
agregaNChoc E n = AddCh (agregaNChoc E n-1)
agregaNChoc m n = AddCh (agregaNChoc m n-1)

agregaNCup ::  CM -> Int -> CM
agregaNCup E n =  AddCu (agregaNCup E n-1)
agregaNCup m n =  AddCu (agregaNCup m n-1)

instance CM Eq where
    (AddCa(AddCh(m))) = (AddCh(AddCa(m)))
    (AddCu(AddCh(m))) = (AddCh(AddCu(m)))
    (AddCu(AddCa(m))) = (AddCa(AddCu(m)))