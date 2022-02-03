-- TADTablero
module TADTablero (Tablero, ptoA, ptoB, tabes_cero, scoredA, scoredB, winA, winB, emp, bonusA, bonusB, castigoA, castigoB) where

-- CONSTRUCTORES
	data Tablero = Inicial
	                  | PtoA (Tablero)
                      | PtoB (Tablero)

    Inicial = start
    PtoA = puntoA
    PtoB = puntoB

-- OPERACIONES
    tab_cero, scoredA, scoredB, winA, winB, emp :: Tablero -> Bool
    bonusA, bonusB, castigoA, castigoB :: Nat -> Tablero -> Tablero

-- ECUACIONES

    tab_cero Inicial = True
    tab_cero PtoA p  = False
    tab_cero PtoB p  = False

    scoredA Inicial = False
    scoredA PtoA p = True
    scoredA PtoB p = scoredA p

    scoredB Inicial = False
    scoredB PtoB p = True
    scoredB PtoA p = scoredB p    

    ctaptoA :: Tablero -> Nat
    ctaptoA Inicial = 0
    ctaptoA PtoA p = 1 + (ctaptoA p)
    ctaptoA PtoB p = 0 + (ctaptoA p)

    ctaptoB :: Tablero -> Nat
    ctaptoB Inicial = 0
    ctaptoB PtoB p = 1 + (ctaptoB p)
    ctaptoB PtoA p = 0 + (ctaptoB p)

    winA Inicial = False
    winA p | (ctaptoA p) > (ctaptoB p) = True
           | otherwise = False  

    winB Inicial = False
    winB p | (ctaptoB p) > (ctaptoA) = True
           | otherwise = False

    emp Inicial = True
    emp p | (ctaptoA p) == (ctaptoB p) = True
          | otherwise = False
    
    bonusA 0 p = p
    bonusA n p = PtoA (bonusA (n-1) p)

    bonusB 0 p = p
    bonusB n p = PtoB (bonusB (n-1) p)

    castigoA 0 p = p
    castigoA n Inicial = Inicial
    castigoA n PtoA p = castigoA (n-1) p
    castigoA n PtoB p = PtoB (castigoA n p)

    castigoB 0 p = p
    castigoB n Inicial = Inicial
    castigoB n PtoB p = castigoB (n-1) p
    castigoB n PtoA p = PtoA (castigoB n p)