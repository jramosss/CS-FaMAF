.data	
        //Laberinto normal
        //laberinto: .dword 0x2b2d2d2d2d2d2d2b, 0x2b2d2d2d2d2d2d2d ,  0x20202020207c587c ,  0x7c2d20202020207c ,  0x202b2d2d207c207c ,  0x7c20202d2d2b207c ,  0x207c2020207c207c ,  0x7c202020207c207c ,  0x207c202d2d2b207c ,  0x2b2d2d20207c207c ,  0x207c20202020207c ,  0x7c232020207c2020 ,  0x2d2b2d2d2d2d2d2b ,  0x2b2d2d2d2d2b2d2d 
        laberinto: .dword 0x2b2d2d2d2d2d2d2b, 0x2b2d2d2d2d2d2d2d ,  0x20202020207c207c ,  0x7c2d58202020207c ,  0x202b2d2d207c207c ,  0x7c20202d2d2b207c ,  0x207c2020207c207c ,  0x7c202020207c207c ,  0x207c202d2d2b207c ,  0x2b2d2d20207c207c ,  0x207c20202020207c ,  0x7c232020207c2020 ,  0x2d2b2d2d2d2d2d2b ,  0x2b2d2d2d2d2b2d2d 
        estado: .dword 0x4e45204f4745554a, 0x21214f5352554320
        _stack_ptr: .dword _stack_end   // Get the stack pointer value from memmap deendition

.text	// Configuracion del Stack Pointer
        ldr     x1, _stack_ptr  
        mov     sp, x1          

        // Limpiar X0 y X4 siempre de comenzar el programa
        mov x0, xzr
        mov x4, xzr
	
        ldr x0, =laberinto	 //X0 + 0x0000000040080000=Dirección base del arreglo "laberinto"

        add x9,x0,17    //X9 guarda la direccion de la primera pos del laberinto
        mov x3,#0x1    //asumo que empieza mirando hacia abajo
        mov x17,#0x20
        add x18,x9,16
        mov x23,#0x0
        add x24,x0,17
        /*
        X3 = Facing direction
            0 arriba
            1 abajo
            2 izquierda
            3 derecha
        X5 = north
        X6 = south
        X7 = west
        X8 = east
        X17 = 0x20
        x18 = forward
        X19 = left
        X20 = Right
        X21 = Behind
        X22 = aux
        X23 = PlayerFound flag
         */

/*
Lo que hago en este programa es seguir el algoritmo de la mano izquierda
dado en clase, me la tuve que rebuscar bastante para hacer ciertas cosas.
Lo primero que hago es buscar mi personaje, esto lo hago siguiendo el 
algoritmo dado, pero con un puntero, a la hora de fijarme si puedo doblar
hacia la izquierda,adelante o derecha, comparo 3 cosas, si mi posicion que
estoy checkeando es #,. o X, si es # gane, si es # y no encontre todavia
mi personaje, lo tomo como que perdi, si ya lo encontre, lo tomo como que gane.
Si es . significa que puedo avanzar, y eso hago.
Si es X, significa que encontre mi personaje, entonces marco mi flag X23 
para indicar que ya lo encontre. Apenas lo encuentro, todavia quiero hacer
una movida mas de puntero, para que X9 apunte a mi personaje, entonces uso
"firstandonly" y a partir de ahi empiezo a mover mi personaje de posicion.

Tanto mientras busco el personaje como cuando ya lo encontre, lo que hago es
Guardar en X3 la direccion hacia la que empieza mirando mi personaje 
(por default su direccion es = 1 = sur), a partir de esa informacion, seteo
cual viene a ser mi izquierda,derecha,adelante y atras, y paso a checkear
si puedo ir a alguna de esas en el orden que dice el algoritmo, si puedo 
doblar para cierta direccion, establezco que esa va a ser mi nueva direccion
por lo que mi forward = x18 pasa a ser esa direccion y avanzo hacia ella.
*/

main:
    mov x10,0x0
    mov x11,0x0
    mov x12,0x0
    mov x13,0x0     //Pongo en 0 todas las flags
    mov x14,0x0
    mov x15,0x0
    sub x5,x9,16    //norte
    add x6,x9,16    //sur
    sub x7,x9,1     //oeste
    add x8,x9,1     //este
    //mov x18,0x0
    bl facing       //Me dice como llegar al norte sur este oeste dependiendo mi direccion
    bl checkleft    //si puedo ir a la izquierda
    cmp x13,#0x1
    b.eq forward    //avanzo hacia la izquierda
    bl checkforward //si puedo hacia adelante
    cmp x14,#0x1
    b.eq forward    //voy hacia adelante
    bl checkright   //lo mismo con la derecha
    cmp x15,#0x1
    b.eq forward
    b turnaround    //y si no entre por ninguno de todos esos casos, doy la vuelta
    //Instrucción NOP para acomodar la imagen
    add xzr,  xzr, xzr
    add xzr,  xzr, xzr
    add xzr,  xzr, xzr

	b main

playerfound:
    mov x23,#0x1
    br x29

facing:
    cmp x3,#0x0
    b.eq facingnorth
    cmp x3,#0x1
    b.eq facingsouth
    cmp x3,#0x2
    b.eq facingwest
    cmp x3,#0x3
    b.eq facingeast

facingnorth:        //Etiqueta que setea tus 4 puntos cardinales depende de donde
                    //estes mirando
    mov x18,x5      //forward, X5 = north
    mov x19,x7      //left, X7 = west
    mov x20,x8      //right, X8 = east
    mov x21,x6      //down, X6 = south
    br x30         //vuelvo a check

facingsouth:
    mov x18,x6      //south=forward
    mov x19,x8      //east=left
    mov x20,x7      //west=right
    mov x21,x5      //north=back
    br x30

facingwest:
    mov x18,x7      //west=forward
    mov x19,x6      //south=left
    mov x20,x5      //north=right
    mov x21,x8      //east=back
    br x30

facingeast:
    mov x18,x8      //east=forward
    mov x19,x5      //north=left
    mov x20,x6      //south=right
    mov x21,x7      //west=back
    br x30

checkleft:
    ldrb w12,[x19],#0   //guardo en x12 lo que esta a la izquierda de mi personaje
    cmp x12,#0x23       //si encontre #, gane!
    b.eq checkganaste
    cmp x12,#0x20       //Si es un ., puedo avanzar
    b.eq cangoleft      //seteo flag para decir que puedo avanzar
    ldr x29,=checkleft
    add x29,x29,40      //Seteo punto de retorno, si encontre a mi personaje,
                        //entonces quiero que vaya a cangoleft pero seteando x23 en 1
    cmp x12,#0x58       //Encontre a mi personaje?
    b.eq playerfound
    br x30              //sino, vuelvo al main
    b cangoleft         //unreachable code, solo se puede alcanzar con br x29

/*Lo mismo para las otras etiquetas de check, solo que depende la direccion*/

checkforward:
    ldrb w12,[x18],#0
    cmp x12,#0x23
    b.eq checkganaste
    cmp w12,#0x20       
    b.eq cangoforward   
    ldr x29,=checkforward
    add x29,x29,40      //Seteo punto de retorno, si encontre a mi personaje,
                        //entonces quiero que vaya a cangoleft pero seteando x23 en 1
    cmp x12,#0x58       //Encontre a mi personaje?
    b.eq playerfound 
    br x30           
    b cangoforward

checkright:
    ldrb w12,[x20],#0
    cmp x12,#0x23
    b.eq checkganaste
    cmp x12,#0x20      
    b.eq cangoright     
    ldr x29,=checkleft
    add x29,x29,40      //Seteo punto de retorno, si encontre a mi personaje,
                        //entonces quiero que vaya a cangoleft pero seteando x23 en 1
    cmp x12,#0x58       //Encontre a mi personaje?
    b.eq playerfound
    br x30          
    b cangoright   

turnaround:
    mov x18,x21
    bl getdirTA
    b forward

getdirTA:   //esta etiqueta me da la direccion contraria a la que estoy mirando
    cmp x3,#0x0
    b.eq northopposite
    cmp x3,#0x1
    b.eq southopposite
    cmp x3,#0x2
    b.eq westopposite
    cmp x3,#0x3
    b.eq eastopposite

northopposite:
    mov x3,0x1  //Cambio de direccion
    mov x18,x6  //y de forward
    br x30

southopposite:
    mov x3,0x0
    mov x18,x5
    br x30

westopposite:
    mov x3,0x3
    mov x18,x8
    br x30

eastopposite:
    mov x3,0x2
    mov x18,x7
    br x30

cangoleft:
    add x13,x13,1           //FLAG X12 = 1 => Puedo a la izquierda
    b turnleft

turnleft:
    cmp x3,#0x0         //North
    b.eq northtoleft
    cmp x3,#0x1         //South
    b.eq southtoleft
    cmp x3,#0x2         //West
    b.eq westtoleft
    cmp x3,#0x3         //East
    b.eq easttoleft

northtoleft:
    mov x3,#0x2
    mov x18,x7
    br x30      //vuelvo a main

southtoleft:
    mov x3,#0x3
    mov x18,x8
    br x30

westtoleft:
    mov x3,#0x1
    mov x18,x6
    br x30

easttoleft:
    mov x3,#0x0
    mov x18,x5
    br x30

cangoforward:
    add x14,x14,1
    br x30

cangoright:
    add x15,x15,1
    b turnright

turnright:
    cmp x3,#0x0         //North
    b.eq northtoright
    cmp x3,#0x1         //South
    b.eq southtoright
    cmp x3,#0x2         //West
    b.eq westtoright
    cmp x3,#0x3         //East
    b.eq easttoright

northtoright:
    mov x3,#0x3
    mov x18,x8
    br x30

southtoright:
    mov x3,#0x2
    mov x18,x7
    br x30

westtoright:
    mov x3,#0x0
    mov x18,x5
    br x30

easttoright:
    mov x3,#0x1
    mov x18,x6
    br x30

error:

ganaste:
	ldr x22,=estado
	movz x23,0x4147,lsl 0
	movk x23,0x414e,lsl 16
	movk x23,0x5453,lsl 32
	movk x23,0x2145,lsl 48
	stur x23,[x22,0]
	add x22,x22,8	
	movz x23,0x2d42,lsl 0
	movk x23, 0x0029,lsl 16
	stur x23,[x22,0]
    b end

checkganaste:
    cmp x23,#0x1
    b.eq ganaste
    b perdiste

perdiste:
	ldr x22,=estado
	movz x23,0x4550,lsl 0
	movk x23,0x9452,lsl 16
	movk x23,0x4534,lsl 32
	movk x23,0xa455,lsl 48
	stur x23,[x22,0]
	add x22,x22,8	
	movz x23,0x0283,lsl 0
	stur x23,[x22,0]


forward:
    cmp x23,#0x0
    b.eq pointerforward //Si todavia no encontre mi personaje
                        //solamente muevo el puntero
    add x25,x25,1       
    cmp x25,#0x1        //Si RECIEN lo encuentro, osea que este es el
                        //primer forward despues de que lo encontre, todavia
                        //quiero que se mueve el puntero, para que x9 apunte 
                        //a la X a partir de ahora
    b.eq firstandonly   //por eso se llama "firstandonly"
	mov x10,x9
	ldrb w11,[x9],#0
	mov x9,x18
	//strb w11,[x18,#0]
    strb w11,[x9,#0]
	strb w17,[x10,#0]
    //Aca no es necesario reasignar la direccion, se mantiene
	b main

firstandonly:   //Firstandonly no hace nada, solo deja pasar a pointerforward
                //podria haber puesto "b pointerforward",pero era innecesario

pointerforward:
    mov x9,x18
    b main

    end:


