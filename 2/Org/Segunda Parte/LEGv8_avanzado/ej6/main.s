	.data
	str: .dword 0x754d30616c6f4830, 0x00000000306f646e
	N: .dword 15
	offset: .dword 0x40080000
	.text
	LDR X0, =str
	LDR X10, offset
	LDR X9, N		// N = 15
	ADD X0, X0, X10        // X0 = &str[0]
	ADD X1, XZR, XZR        // found = 0
	ADD X2, XZR, XZR        // i = 0
for:    CMP X2, X9            // comparo i y N
	B.EQ end            // Salto si son iguales
	ADD X11, X0, X2        // X11 = &str[0] + i
	LDURB W12, [X11, #0]    // X12 = str[i] 
	CMP W12, #48        // Verifico si el byte que traje es un 0
	B.NE skip            // Si son distintos no lo cuento
	ADD X1, X1, #1         // Si es un cero, found +=1
skip:	ADD X2, X2, #1        // i = i + 1
	B for
	ADD X0, X0, X0		
end:
infloop: B infloop
