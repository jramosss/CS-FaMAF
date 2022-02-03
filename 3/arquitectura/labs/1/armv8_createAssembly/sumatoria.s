	.text
	.org 0x0000

	add x1, xzr,xzr
	add x3, xzr,xzr

sumatoria:
	ldur x2,[x1, #0]
	add x3, x2, x3
	addi x2, x2, #4
	cmp x 

infLoop:  
	cbz XZR infLoop
