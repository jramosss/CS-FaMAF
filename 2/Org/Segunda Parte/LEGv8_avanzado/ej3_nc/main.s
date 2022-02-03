.text
        mov    x0, #5           // i = 5
        mov    x1, #2           // j = 2
        mov    x3, #3           // N = j
	sub x9, x0, x3     	// comparo i con N y lo guardo en x9
	cbz x9, True        	// Si son iguales salto a else
	sub x10, x1, x3    	// comparo j con N y lo guardo en x10
	cbz x9, True        	// Si son iguales salto a else
    	add x0, x0, #1        	// ++i
    	add x1, x1, #1     	// ++j
    	b end
True:   addi x2, x2, #1    	// ++k    
end: 

