.data
f: .dword 0x1
o: .dword 0x4000000000000000
c: .dword 0xFFFFFFFFFFFFFFFF   
n: .dword -10 

.text
LDR X0, f
LDR X1, n
LDR X2, c
LDR X3, o

ADDS X9, X0, X0 	// No flag
ADDS X0, X0, X1  	// Negative Flag
ADDS X0, XZR, XZR 	// Zero flag
ADDS X0, X2, #3		// Carry flag
ADDS X0, X3, X3         // Overflow flag

infloop: B infloop
