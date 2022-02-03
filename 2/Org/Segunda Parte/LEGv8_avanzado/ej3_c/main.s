	.arch armv8-a
	.file	"ej3.c"
	.data
        _stack_ptr: .dword _stack_end   // Get the stack pointer value from memmap definition
	baseAddr: .dword 0x40080000
	.text
	.align	2
	.global	ej3
	.type	ej3, %function
	ldr     x9, _stack_ptr  // Load stack pointer to X9
	ldr	x10, baseAddr
	add	x9, x9, x10
	mov	sp, x9
	mov x0, #5 //i =5
	mov x1, #2 //j = 2 
	mov X2, #0 // k =0
	mov X3, #3 // N = 3

ej3:
	sub    sp, sp, #32
	str    x0, [sp, 24]     //i
	str    x1, [sp, 16]    	//j
	str    x2, [sp, 8]    	//k
	str    x3, [sp]        	//N
	ldr    x1, [sp, 24]   	//X1 = i 
	ldr    x0, [sp]        	//X0 = N
	cmp    x1, x0        	//if(i==N){
	cset    w0, eq        	//   w0 = 1}
	and    w1, w0, 255    	//w1 = w0 and 0xff
	ldr    x2, [sp, 16]    	//x2 = j
	ldr    x0, [sp]        	//X0 = N
	cmp    x2, x0        	//if(j==N){
	cset    w0, eq        	//   w0 = 1}
	and    w0, w0, 255    	//w0 = w0 and 0xff
	orr    w0, w1, w0       //w0 = w0 or w1
	and    w0, w0, 255    	//w0 = w0 and 0xff
	cmp    w0, 0       	//if(w0 == 0) {
	beq    .L2        	//    goto L2}
	mov    x0, 2
	str    x0, [sp, 8]
	b    .L3

	.size	ej3, .-ej3
	.ident	"GCC: (Ubuntu/Linaro 7.5.0-3ubuntu1~18.04) 7.5.0"
	.section	.note.GNU-stack,"",@progbits
