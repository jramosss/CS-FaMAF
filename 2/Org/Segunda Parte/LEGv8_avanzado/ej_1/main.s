.data
a: .dword 0x0000000000000001 
//b: .dword 0x0000000000101000        
b: .dword 0x8000000000001000
.text
	LDR X10, a
	LDR X9, b
	SUBS X0, X9, #0        // FLAGS = X9 - 0
	ADD x0, X0,X0
        B.GE else                // Salto a "else" X9 es mayor a cero
        B done                // Salto incondicional a "done"
else:   ORR X10, XZR, #2        // X10 = 0 || 2
done: 

infloop: B infloop
