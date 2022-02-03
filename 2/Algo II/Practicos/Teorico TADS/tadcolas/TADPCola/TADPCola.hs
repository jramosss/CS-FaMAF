-- TAD PCola

module TADPCola(PCola, vacía, encolar, es_vacía, primero, decolar) where

-- CONSTRUCTORES

data PCola e = Vacía
             | Encolar (PCola e) e
             deriving Show

-- AXIOMAS
-- Encolar (Encolar q e1) e2 = Encolar (Encolar q e2) e1

vacía :: Ord e => PCola e
vacía = Vacía

encolar :: Ord e => PCola e -> e -> PCola e
encolar = Encolar

-- OPERACIONES

es_vacía :: Ord e => PCola e -> Bool
primero :: Ord e => PCola e -> e        -- se aplica solo a una cola no Vacía
decolar :: Ord e => PCola e -> PCola e  -- se aplica solo a una cola no Vacía

-- ECUACIONES

es_vacía Vacía = True
es_vacía (Encolar q e) = False

primero Vacía = error "No se puede obtener el primero de la cola Vacía"
primero (Encolar q e) | es_vacía q = e
                      | e >= primero q = e
                      | otherwise = primero q

decolar Vacía = error "No se puede decolar la cola Vacía"
decolar (Encolar q e) | es_vacía q = Vacía
                      | e >= primero q = q
                      | otherwise = Encolar (decolar q) e



instance Ord e => Eq (PCola e) where
    Vacía == q2 = es_vacía q2
    q1@(Encolar _ _) == q2 = not (es_vacía q2)
                           && primero q1 == primero q2
                           && decolar q1 == decolar q2

