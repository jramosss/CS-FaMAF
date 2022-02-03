test_b_mi:				// N = 1
    SUBS X0 , X0, X1
    B.MI jump
ADD XZR,XZR,XZR
ADD XZR,XZR.XZR
ADD XZR,XZR,XZR
    CBZ XZR, infloop
ADD XZR,XZR,XZR
ADD XZR,XZR,XZR
ADD XZR,XZR,XZR

jump:
    STUR X10, [X1,#8]
    CBZ XZR, infloop
ADD XZR,XZR,XZR
ADD XZR,XZR,XZR
ADD XZR,XZR,XZR

infloop:
    CBZ XZR, infloop


    
