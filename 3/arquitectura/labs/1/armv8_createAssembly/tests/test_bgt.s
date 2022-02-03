test_b_gt:
    SUBS X6,X7,X5
    B.GT jump
ADD XZR,XZR,XZR
ADD XZR,XZR,XZR
ADD XZR,XZR,XZR
    CBZ XZR, infloop
ADD XZR,XZR,XZR
ADD XZR,XZR,XZR
ADD XZR,XZR,XZR 


jump:
    STUR X10, [X0,#0] //Store 10 in position 0 i think
    CBZ XZR, infloop
ADD XZR,XZR,XZR
ADD XZR,XZR,XZR
ADD XZR,XZR,XZR
 

infloop:
    CBZ XZR, infloop
