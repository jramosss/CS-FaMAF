.teXt
.org 0X0000

test_b_eq:
    bl reset
    add x0, x1, X2
ADD XZR,XZR,XZR
    ADD X4,
    ADD x1,x0,
ADD XZR,XZR,XZR
    ldur x0, [x1,#0]
ADD XZR,XZR,XZR
    ldur x2, [x2,#0]
ADD XZR,XZR,XZR
    SUBS X3, X2,X0
    b.eq test


test_not_b_eq:
    BL reset
    ADD X0,X0,X0
ADD XZR,XZR,XZR
    ADDI X1,X0,#14
ADD XZR,XZR,XZR
    B.EQ X0,X1

test_b_ne:


test:
    B test



