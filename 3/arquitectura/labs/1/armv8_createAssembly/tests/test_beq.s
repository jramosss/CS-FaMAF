
ADD X7, XZR, XZR

test_b_eq:
    STUR X1, [X7, #0]
    ADD X1, X2, X3      // X1 = 2 + 3
ADD XZR,XZR,XZR
ADD XZR,XZR,XZR
ADD XZR,XZR,XZR
    SUBS X0, X1,X5      // X0 = 5 - 5
    B.EQ jump
ADD XZR,XZR,XZR
ADD XZR,XZR,XZR
ADD XZR,XZR,XZR
    STUR X11, [X0,#0]


jump:
    ADD X7, X7, X8
ADD XZR,XZR,XZR
ADD XZR,XZR,XZR
ADD XZR,XZR,XZR
    STUR X10, [X7,#0] //Store 10 in position 9 i think
    ADD X7, X7, X8
    SUBS X0, X1, X4 // X0 <= 5 - 4
    B.EQ test_b_eq
ADD XZR,XZR,XZR
ADD XZR,XZR,XZR
ADD XZR,XZR,XZR
    CBZ XZR, infloop
ADD XZR,XZR,XZR
ADD XZR,XZR,XZR
ADD XZR,XZR,XZR
    
infloop:
    CBZ XZR, infloop
	

    
