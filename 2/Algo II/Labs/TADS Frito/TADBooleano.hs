-- TAD Booleano

module TADBooleano(Booleano,v,f,no,y,o,imp,eq) where

    -- Constructores

    data Booleano= V | F
        deriving (Show)

    v = V
    f = F

    -- Operaciones
    no :: Booleano -> Booleano
    y, o, imp, eq :: Booleano -> Booleano -> Booleano

    -- Ecuaciones


    no V = F
    no F = V

    V `y` b = b
    F `y` b = F

    F `o` b = b
    V `o` b = V

    V `imp` F = F
    V `imp` V = V
    F `imp` b = V

    V `eq` F = F
    V `eq` V = V
    F `eq` F = V
    F `eq` V = F