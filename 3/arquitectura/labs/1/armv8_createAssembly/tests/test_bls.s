test_ls:
    SUBS X3,X0,X0				// Z=1 | C=0
    B.LS jump
ADD XZR,XZR,XZR
ADD XZR,XZR,XZR
ADD XZR,XZR,XZR
    CBZ XZR, infloop
ADD XZR,XZR,XZR
ADD XZR,XZR,XZR
ADD XZR,XZR,XZR

jump:
    STUR X1,[X0,#0]
    CBZ XZR, infloop
ADD XZR,XZR,XZR
ADD XZR,XZR,XZR
ADD XZR,XZR,XZR
    
infloop:
    CBZ XZR, infloop
    


    
