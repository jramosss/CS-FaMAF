test_b_lo:
    ADD X1, X2, X4      // X1 = 2 + 4
    ADD X2, X2, X5      // X2 = 2 + 5
ADD XZR,XZR,XZR
ADD XZR,XZR,XZR
ADD XZR,XZR,XZR
    ADDS X3, X1, X2      // X3 = 7 + 6
    B.LO jump					//deberia setear la carry flag C=0
ADD XZR,XZR,XZR
ADD XZR,XZR,XZR
ADD XZR,XZR,XZR
    STUR X11, [X0,#0]
    CBZ XZR, infloop
ADD XZR,XZR,XZR
ADD XZR,XZR,XZR
ADD XZR,XZR,XZR

jump:
    STUR X10, [X0,#0] //Store 10 in position 9 i think
    CBZ XZR, infloop
ADD XZR,XZR,XZR
ADD XZR,XZR,XZR
ADD XZR,XZR,XZR   
    
infloop:
    CBZ XZR, infloop


    
