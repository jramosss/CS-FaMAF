.data
        N: .dword 3 
        dirBase: .dword 0x0000000040080000
        A: .dword 1,2,3,4,5,6,7,8,9 // A[N][N]
.text
        ldr X0, =A      	// x0 =&A[0][0] (relativo)
        ldr X9, N     	        // N=3
        ldr X10, dirBase
        add X0, X0, X10    	// x0 =&A[0][0] (absoluto)
        add X1, XZR, XZR   	// s=0
        add X2, XZR, XZR   	// i=0
	mul X9, X9, X9		// newN = N * N
oLoop:  cmp X2,X9           	// if(i==N)
        b.eq oEnd             	// goto oEnd
        add X12, XZR, X2    	// X12 = i
        lsl x12, x12, #3    	// X12 = i * 8
        add X12, X12, X0    	// X12 = &A[0] + i * 8
        ldur X11, [X12,#0]    	// X11=A[i]    
        add X1, X1, X11        	// s+=A[i]
        add X2, X2, #1        	// i++;
        b oLoop
oEnd:

infloop: B infloop
