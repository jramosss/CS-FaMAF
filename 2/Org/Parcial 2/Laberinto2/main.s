.data	
	/*Laberinto normal */
	laberinto: .dword 0x2b2d2d2d2d2d2d2b, 0x2b2d2d2d2d2d2d2d ,  0x20202020207c587c ,  0x7c2d20202020207c ,  0x202b2d2d207c207c ,  0x7c20202d2d2b207c ,  0x207c2020207c207c ,  0x7c202020207c207c ,  0x207c202d2d2b207c ,  0x2b2d2d20207c207c ,  0x207c20202020207c ,  0x7c232020207c2020 ,  0x2d2b2d2d2d2d2d2b ,  0x2b2d2d2d2d2b2d2d 
	/*Laberinto 2 */
	//laberinto: .dword 0x2b2d2d2d2d2d2d2b, 0x2b2d2d2d2d2d2d2d ,  0x20202020207c207c ,  0x7c2d58202020207c ,  0x202b2d2d207c207c ,  0x7c20202d2d2b207c ,  0x207c2020207c207c ,  0x7c202020207c207c ,  0x207c202d2d2b207c ,  0x2b2d2d20207c207c ,  0x207c20202020207c ,  0x7c232020207c2020 ,  0x2d2b2d2d2d2d2d2b ,  0x2b2d2d2d2d2b2d2d 
	/*Laberinto 3 */
	//laberinto: .dword 0x2b2d2d2d2d2d2d2b, 0x2b2d2d2d2d2d2d2d ,  0x20202020207c207c ,  0x7c2d20202020207c ,  0x202b2d2d207c207c ,  0x7c20202d2d2b207c ,  0x207c2020207c587c ,  0x7c202020207c207c ,  0x207c202d2d2b207c ,  0x2b2d2d20207c207c ,  0x207c20202020207c ,  0x7c232020207c2020 ,  0x2d2b2d2d2d2d2d2b ,  0x2b2d2d2d2d2b2d2d 
	estado: .dword 0x4e45204f4745554a, 0x21214f5352554320
	_stack_ptr: .dword _stack_end   // Get the stack pointer value from memmap definition

.text	// Configuracion del Stack Pointer
	ldr     X1, _stack_ptr  
        mov     sp, X1          

	// Limpiar X0 y X4 siempre de comenzar el programa
	MOV X0, XZR
	MOV X4, XZR
	
	LDR X0, =laberinto	 //X0 = Dirección base del arreglo "laberinto"
/*
La idea es hacer un programa que recorra todo el laberinto
buscando a X, una vez que lo encuentra, sigue los pasos definidos
en el punto 1.
*/


main:
	ldr x5,=corridaP	//Guardo en x5 la direccion de memoria donde se aloja
						//la etiqueta "CorridaP"
	ldr x6,=corrida		//en x6 guardo la dir de "corrida"
	mov x7,0x58
	add x9,x0,17		//Hago que x9 apunte al principio del laberinto

	/* 
	Hago la primera iteracion de "check" en el main, levemente modificada 
	para que no aumente x5 ni x6 y haga la primera instruccion en vez 
	de la segunda directo en caso de b.ne
	*/
	ldrb w10,[x9],#0	
	cmp x10,x7			
	b.eq fstjump			
	br x5
	//Instrucción NOP para acomodar la imagen
	ADD XZR, XZR, XZR
	ADD XZR, XZR, XZR
	ADD XZR, XZR, XZR

	b main

check:
	ldrb w10,[x9],#0	//Cargo en x10 lo que hay en esa posicion del laberinto
	cmp x10,x7			//Comparo con 0x58
	b.eq jump			//Si lo que hay en esa posicion=0x58, encontramos nuestra x
	add x5,x5,4			//Caso contrario, x5+=4
	add x6,x6,4			//Tambien guardo en x6 cuantas instrucciones de "corrida"
						//voy saltando para cuando tenga que cambiar
	br x5				//Ahora salto a x5=corridaP+4, siguiente instruccion

fstjump:				//En caso de que la x este en la primera posicion
						//del laberinto, uso esta etiqueta
	br x6

jump:
	add x6,x6,4
	br x6

corridaP:	//Corrida del "puntero", recorre el laberinto sin modificar nada,
			//Solo verificando si encuentra a X
	b abajoP
	b abajoP
	b abajoP
	b abajoP
	b derechaP
	b derechaP
	b derechaP
	b derechaP
	b arribaP
	b arribaP
	b izquierdaP
	b izquierdaP
	b arribaP
	b arribaP
	b derechaP
	b derechaP
	b derechaP
	b derechaP
	b abajoP
	b abajoP
	b abajoP
	b abajoP
	b derechaP 
	b derechaP
	b arribaP
	b arribaP
	b arribaP
	b arribaP
	b derechaP 
	b derechaP
	b derechaP 
	b derechaP
	b abajoP
	b abajoP
	b izquierdaP
	b izquierdaP
	b abajoP
	b abajoP
	b derechaP
	b derechaP

corrida:	//Corrida normal (ejercicio 1)
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

arribaP:
	sub x9,x9,16
	b check

abajoP:
	add x9,x9,16
	b check

izquierdaP: 
	sub x9,x9,1
	b check

derechaP:
	add x9,x9,1
	b check

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
	strb w11,[x9,#0]  //Guardo la X en la posicion objetivo
	strb w3,[x10,#0]  //Guardo un . en la posicion recientemente abandonada
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

