.data	
    //Original
    //password: .dword 0x4141414141414141, 0x4242424242424242, 0x4343434343434343, 0x4444444444444444 
    //Correcta
    password: .dword 0x6569646e65727041, 0x446f676c416f646e, 0x456f747079724365, 0x4d5241794d53416e
    some1:    .dword 0x5ccc37dea1dac139, 0x35e30b4c16fd0fc8, 0xd1a1109f53e3430 , 0x6c72624c252b0207
    some2:    .dword 0x754758517c43534d, 0x74517f857b7e784d, 0x857a84557f798054, 0x608e8f81897e827d
    status:   .dword 0x6974707972636564, 0x2e2e2e676e   //decrypting...
    _stack_ptr: .dword _stack_end   // Get the stack pointer value from memmap definition
    //4661696c65642121
    //41 70 71 72 73 74 64 75 65 77 64 64 79 41 7b 67 7d 44 65 43 87 88 76 7a 75 73 46 78
    //0x784673757a768887 0x4365447d677b4179 0x6464776575647473 0x7271704100000000

.text   // Configuracion del Stack Pointer
        ldr     x1, _stack_ptr  
        mov     sp, x1     
	
        MOV X0, XZR
        MOV X4, XZR
	
main:
        LDR X0, =password	  //X0 = Dirección base del arreglo "password"
        LDR X1, =some1	  	  //X1 = Dirección base del arreglo "some1"
        LDR X2, =some2	  	  //X2 = Dirección base del arreglo "some2"
        LDR X5, =status	          //X3 = Dirección base del arreglo "status"

        mov x3, 0x20
        add x11, x2, 0x100
             
        mov x8, x3              //x8 = x3
a:      sub x8, x8, 0x1         //x8--        
        add x7, x2, x8          //x7 = some2+x8
        ldurb w20, [x7]         //x20 = *some2[x8]
        sturb w20, [x11]        //&some2+100 = *x20 (some2+100)
        add x11, x11, 0x1       //x11++
        cbnz x8, a              //while (x8 != 0)
        //Cargo some2 pero al reves en x11(&some2+100)

        mov x8, x3              //x8 = x3
        mov x7, xzr             //x7 = 0
        add x15, x2, 0x100      //x15 = some2+100
        mov x11, x15            //x11 = x15
b:      sub x8, x8, 0x1         //x8--
        add x10, x0, x7         //x10 = password+x7
        add x11, x15, x7        //x11 = (some2+100)+x7
        ldurb w20, [x10]        //x20 = *x10
        //cargo password en x20
        ldurb w21, [x11]        //x21 = *x11
        //cargo some2 en x21
        sub x22, x21, x20       //x22 = x21 - x20 - 8
        sub x22, x22, x8
        //cbnz x22, c             //if x22 != 0 goto c
        cbz x8, d               //else goto d
        add x7, x7, 0x1         //x7++
        bl b                    //goto b
        /* 
        Mientras some2[i] == password[i] y x8 != 0:
        x20 = password[i]
        x21 = some2[i]
        si some2[i] != password[i],significa que la contraseña es invalida,
        goto c y doy por fallado el programa
        si x8 == 0, significa que la contraseña es valida,goto d
        */

c:      movk x7, 0x2121, LSL 48 
        movk x7, 0x6465, LSL 32
        movk x7, 0x6c69, LSL 16
        movk x7, 0x6146      
        //Failed!!      
        stur x7, [x5]   //guardo Failed!! en status
        stur xzr, [x5,8]//limpio el resto de status
        bl end                       

d:      eor x7, x7, x7          //XOR a x7 con x7 (= XZR)
        add x15, x2, 0x100      //X15 = some2+100
d1:     add x11, x15, x7        //x11 = x15+x7
        add x12, x1, x7         //x12 = some1+x7
        ldurb w20, [x11]        //x20 = *x11
        //cargo some2 en x20
        ldurb w21, [x12]        //x21 = *x12(some1+x7)
        //cargo some1 en x21
        eor w20, w20, w21       //XOR x20 y x21
        sturb w20, [x12]        //*some1+x7 = x20
        //guardo el resultado del eor en x20 (x20=resultado)
        sub x8, x7, 0x1f        //x8 = x7-1f
        add x7, x7, 0x1         //x7++
        cbnz x8, d1             //while(x8 != 0)
 
end:
        add xzr, xzr, xzr
        B end
