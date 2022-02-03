test_b_ge:
    ADD X1, X2, X4      // X1 = 2 + 4
    ADD X2, X2, X5      // X2 = 2 + 5
ADD XZR,XZR,XZR
ADD XZR,XZR,XZR
ADD XZR,XZR,XZR
    ADDS X3, X2,X1      // X3 = 7 + 6
    B.GE jump
ADD XZR,XZR,XZR
    STUR X11, [X0,#0]


jump:
    STUR X10, [X0,#0] //Store 10 in position 9 i think
    CBZ XZR, infloop
    
infloop:
    CBZ XZR, infloop
	
	

    
