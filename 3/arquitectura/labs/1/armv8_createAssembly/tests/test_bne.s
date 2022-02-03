test_b_ne:
    ADD X1, X2, X2      // X1 = 2 + 2
ADD XZR,XZR,XZR
ADD XZR,XZR,XZR
ADD XZR,XZR,XZR
    SUBS X0, X1,X5      // X0 = 5 - 4
    B.NE jump
ADD XZR,XZR,XZR
ADD XZR,XZR,XZR
ADD XZR,XZR,XZR
    STUR X11, [X0,#0]


jump:
    STUR X10, [X0,#0] //Store 10 in position 9 i think
    CBZ XZR, infloop
ADD XZR,XZR,XZR
ADD XZR,XZR,XZR
ADD XZR,XZR,XZR
    
infloop:
    CBZ XZR, infloop
	

