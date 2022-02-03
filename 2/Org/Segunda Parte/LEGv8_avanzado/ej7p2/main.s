.data
        N: .dword 3 
        dirBase: .dword 0x0000000040080000
        A: .dword 1,7,2,44,3,21,1,2,3 // A[N][N]
.text
        ldr X0, =A            // x0 =&A[0][0] (relativo)
        ldr X9, N             // N=3
        ldr X10, dirBase
        add X0, X0, X10    // x0 =&A[0][0] (absoluto)
        add X1, XZR, XZR   // s=0
    	mul X9, X9, X9    // X9= N * N
	lsl x9, x9, 3        // X9= N * N * 8
	add X9, x9, X0    // finalAddr = &A[0][0] + (N*N*8)    
oLoop:  cmp X0,X9           // if(i==finalAddr)
        b.eq oEnd             // goto oEnd;
        ldur X11, [X0,#0]    // X11=A[i]    
        add X1, X1, X11        // s+=A[i]
       	add X0, X0, #8        // i++
        b oLoop
oEnd:



