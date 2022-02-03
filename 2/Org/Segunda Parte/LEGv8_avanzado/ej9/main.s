.data
A: .dword 0x00
.text

loop:
    LDR x10, =A
    MOVZ X9, 0xCDEF, LSL 0      
    MOVK X9, 0x89AB, LSL 16      
    MOVK X9, 0x4567, LSL 32      
    MOVK X9, 0x0123, LSL 48      
    STUR X9, [x10, #0]      
    LDURB W0, [X10, #0]
    LDURB W1, [X10, #1]
    LDURB W2, [X10, #2]
    LDURB W3, [X10, #3]
    LDURB W4, [X10, #4]
    LDURB W5, [X10, #5]
    LDURB W6, [X10, #6]
    LDURB W7, [X10, #7]
L: b loop
