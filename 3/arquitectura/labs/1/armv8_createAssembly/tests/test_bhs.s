test_b_hs:
    SUBS X6,X7,X5	// Para que salte B.HS se tienen que setea la flag C = 1, no se si este subs lo cumple
    B.HS jump
ADD XZR,XZR,XZR
ADD XZR,XZR,XZR
ADD XZR,XZR,XZR
    CBZ XZR, infloop
ADD XZR,XZR,XZR
ADD XZR,XZR,XZR
ADD XZR,XZR,XZR

jump:
    STUR X11, [X18,#0]
    CBZ XZR, infloop
ADD XZR,XZR,XZR
ADD XZR,XZR,XZR
ADD XZR,XZR,XZR	
    
infloop:
    CBZ XZR, infloop
    

    
