test_b_pl:				// N = 0
    SUBS X0 , X1, X0
    B.PL jump
ADD XZR,XZR,XZR
ADD XZR,XZR,XZR
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
