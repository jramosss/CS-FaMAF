test_b_hi:
    SUBS X6,X7,X5	// Para que salte B.HI se tienen que setear las flags C = 1 y Z = 0, no se si este subs lo cumple
    B.HI jump
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
    


    
