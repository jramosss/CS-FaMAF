.text
.org 0x0000 

	STUR X1, [X0, #0]      // MEM 0:0x1 
	STUR X2, [X0, #8]      // MEM 1:0x2 
	STUR X3, [X16, #0]     // MEM 2:0x3 
	AND X20, X21, X22
ADD XZR, XZR, XZR
ADD XZR, XZR, XZR
ADD XZR, XZR, XZR
	STUR X20, [x0, #24]     // MEM 3: 20
	ORR X20, X21, X22
ADD XZR, XZR, XZR
ADD XZR, XZR, XZR
ADD XZR, XZR, XZR
	STUR X20, [x0, #32]     // MEM 4: 23
	SUB X20, X20, X3
ADD XZR, XZR, XZR
ADD XZR, XZR, XZR
ADD XZR, XZR, XZR
	STUR X20, [x0, #40]     // MEM 5: 17
	LDUR X13, [X0, #0]
ADD XZR, XZR, XZR
ADD XZR, XZR, XZR
ADD XZR, XZR, XZR
	STUR X13, [x0, #48]     // MEM 6: 1
	STUR XZR, [X0, #56]     // MEM 7: 0
	CBZ X0, int1 
ADD XZR, XZR, XZR
ADD XZR, XZR, XZR 
ADD XZR, XZR, XZR
	STUR X21, [X0, #64] // MEM 8: 0(si falla CBZ =21)


int1:
	ADD X1, X30, X30
	ADD X2, XZR, X5
	ADD X3, XZR, X10
ADD XZR, XZR, XZR
ADD XZR, XZR, XZR
ADD XZR, XZR, XZR
	ADD X1, X1, x12
	CBZ XZR, loop1
ADD XZR, XZR, XZR
ADD XZR, XZR, XZR
ADD XZR, XZR, XZR
	

loop1: 
	SUBS X2, X2, X5
    B.EQ loop2            
ADD XZR, XZR, XZR
ADD XZR, XZR, XZR
ADD XZR, XZR, XZR
    STUR X2, [X1,#0]   // No se deberia ejecutar.       
 	ADD X1,X1,X8        

loop2: 
	SUBS X3, X3, X5
	B.NE finloop
ADD XZR, XZR, XZR
ADD XZR, XZR, XZR
ADD XZR, XZR, XZR
	STUR X3, [X1,#0]   // No se deberia ejecutar.       
        ADD X1,X1,X8 
	CBZ XZR, loop2	


test_b_eq:
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
    ADD X0,X0,X0
ADD XZR,XZR,XZR
    ADDI X1,X0,#14
ADD XZR,XZR,XZR
    B.EQ X0,X1

test_b_ne:


test:
    B test



finloop: 
	CBZ XZR, finloop

	



	
	
	
