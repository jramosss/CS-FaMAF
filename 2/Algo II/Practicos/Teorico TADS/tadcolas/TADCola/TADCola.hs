-- TAD Cola

module TADCola(Cola,vacía,encolar,es_vacía,primero,decolar) where

-- CONSTRUCTORES

data Cola e = Vacía
            | Encolar (Cola e) e
            deriving (Eq, Show)

vacía = Vacía
encolar = Encolar

-- OPERACIONES

es_vacía :: Cola e -> Bool
primero :: Cola e -> e        -- se aplica solo a una cola no Vacía
decolar :: Cola e -> Cola e   -- se aplica solo a una cola no Vacía

-- ECUACIONES

es_vacía Vacía = True
es_vacía (Encolar q e) = False

primero Vacía = error "No se puede obtener el primero de la cola Vacía"
primero (Encolar q e) | es_vacía q = e
                      | otherwise = primero q

decolar Vacía = error "No se puede decolar la cola Vacía"
decolar (Encolar q e) | es_vacía q = Vacía
                      | otherwise = Encolar (decolar q) e


