-- TAD Contador

module TADContador(Contador,inicial,incrementar,es_inicial,decrementar) where

-- CONSTRUCTORES

data Contador = Inicial
              | Incrementar Contador
              deriving (Eq, Show)


-- CONSTRUCTORES

inicial = Inicial
incrementar = Incrementar

-- OPERACIONES

es_inicial :: Contador -> Bool
decrementar :: Contador -> Contador -- se aplica solo a un Contador que no sea Inicial

-- ECUACIONES

es_inicial Inicial = True
es_inicial (Incrementar c) = False

decrementar Inicial = error "No se puede decrementar el Contador Inicial"
decrementar (Incrementar c) = c


