.data	
	laberinto: .dword 0x2b2d2d2d2d2d2d2b, 0x2b2d2d2d2d2d2d2d ,  0x20202020207c587c ,  0x7c2d20202020207c ,  0x202b2d2d207c207c ,  0x7c20202d2d2b207c ,  0x207c2020207c207c ,  0x7c202020207c207c ,  0x207c202d2d2b207c ,  0x2b2d2d20207c207c ,  0x207c20202020207c ,  0x7c232020207c2020 ,  0x2d2b2d2d2d2d2d2b ,  0x2b2d2d2d2d2b2d2d 
	estado: .dword 0x4e45204f4745554a, 0x21214f5352554320
	_stack_ptr: .dword _stack_end   // Get the stack pointer value from memmap definition

.text	// Configuracion del Stack Pointer
	ldr     X1, _stack_ptr  
        mov     sp, X1

	// Limpiar X0 y X4 siempre de comenzar el programa
	MOV X0, XZR
	MOV X4, XZR
	//MOV x20, XZR
	
	LDR X0, =laberinto	 //X0 = Dirección base del arreglo "laberinto"

main:
		add x9,x0,17	// Guardo en x9 la direccion de memoria
						// en la cual se encuentra X actualmente
		mov x3, 0x20	// Guardo en X3 el codigo ascii de un .

		bl abajo
		bl abajo
		bl abajo
		bl abajo

		bl derecha
		bl derecha
		bl derecha
		bl derecha

		bl arriba
		bl arriba

		bl izquierda
		bl izquierda

		bl arriba
		bl arriba

		bl derecha
		bl derecha
		bl derecha
		bl derecha

		bl abajo
		bl abajo
		bl abajo
		bl abajo

		bl derecha 
		bl derecha

		bl arriba
		bl arriba
		bl arriba
		bl arriba

		bl derecha 
		bl derecha
		bl derecha 
		bl derecha

		bl abajo
		bl abajo

		bl izquierda
		bl izquierda

		bl abajo
		bl abajo

		bl derecha
		bl derecha

		b ganaste
	b main


/*
Basicamente lo que hago es un swap
Primero asigno a x10 y x9 la direccion de memoria donde se aloja X (0x58)
Guardo en x11 lo que hay en X9(0x58)
Despues hago que x9 guarde la direccion de memoria de la posicion a la que 
quiera avanzar.
Ahora guardo lo que hay en x11(0x58) en x9
Asigno X3(0x20) a X10 (la posicion donde antes se hallaba X)
*/

arriba:
	mov x10,x9
	ldrb w11,[x9],#0
	sub x9,x9,16
	strb w11,[x9,#0]
	strb w3,[x10,#0]
	br x30

abajo:
	mov x10,x9
	ldrb w11,[x9],#0  //Cargo en x11 la X
	add x9,x9,16	  //Posicion a la que quiero llegar
	strb w11,[x9,#0] //Guardo la X en la posicion objetivo
	strb w3,[x10,#0]   //Guardo un . en la posicion recientemente abandonada
	br x30			  // ret al main

izquierda:
	mov x10,x9
	ldrb w11,[x9],#0
	sub x9,x9,1
	strb w11,[x9,#0]
	strb w3,[x10,#0]
	br x30

derecha:
    mov x10,x9
    ldrb w11,[x9],#0
    add x9,x9,1
    strb w11,[x9,#0]
    strb w3,[x10,#0]
    br x30

ganaste:
	ldr x12,=estado
	movz x13,0x4147,lsl 0
	movk x13,0x414e,lsl 16
	movk x13,0x5453,lsl 32
	movk x13,0x2145,lsl 48
	stur x13,[x12,0]
	add x12,x12,8	
	movz x13,0x2d42,lsl 0
	movk x13, 0x0029,lsl 16
	stur x13,[x12,0]

perdiste:
	ldr x12,=estado
	movz x13,0x4550,lsl 0
	movk x13,0x9452,lsl 16
	movk x13,0x4534,lsl 32
	movk x13,0xa455,lsl 48
	stur x13,[x12,0]
	add x12,x12,8	
	movz x13,0x0283,lsl 0
	stur x13,[x12,0]

