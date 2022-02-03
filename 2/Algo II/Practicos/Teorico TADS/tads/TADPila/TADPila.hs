-- TAD Pila

module TADPila(Pila,vacía,apilar,es_vacía,primero,desapilar) where

-- CONSTRUCTORES

data Pila e = Vacía
            | Apilar e (Pila e)
            deriving (Eq, Show)

-- CONSTRUCTORES

vacía = Vacía
apilar = Apilar

-- OPERACIONES

es_vacía :: Pila e -> Bool
primero :: Pila e -> e        -- se aplica solo a una pila no Vacía
desapilar :: Pila e -> Pila e -- se aplica solo a una pila no Vacía

-- ECUACIONES

es_vacía Vacía = True
es_vacía (Apilar e p) = False

primero Vacía = error "No se puede obtener el primero de la pila Vacía"
primero (Apilar e p) = e

desapilar Vacía = error "No se puede desapilar la pila Vacía"
desapilar (Apilar e p) = p


