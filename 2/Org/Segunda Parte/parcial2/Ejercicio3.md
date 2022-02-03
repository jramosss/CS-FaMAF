> Julian Ramos. Organizacion del computador 2020, Facultad de Matematica, Astronomia, fisica y computacion.
# Ejercicio 3

### Tipo R  
* ADD XZR,XZR,XZR   

opcode     | Rm   | shamt  | Rn   | Rd

10001011000 00000  000000   00000  00000

0x8b000000

---
### Tipo I  
* ADD X9,X0,17 

opcode    |ALU_immediate|Rn   |Rd

1001000100 000000010001 00000 01001

0x91004409

---
### Tipo D  
### LDRB w10,[X9],#0

opcode    | Dt_adress | op | Rn | Rt

00111000010 000000000   00  01001 01010

0x3840012a

---

### Tipo CB
* b.eq jump

opcode  | COND_BR_adress      | Rt
          3 instrucciones
01010100  0000000000000000011   00000

0x74000060

---

### Tipo B
* b derechaP

opcode | COND_BR_adress

000101  00000000000000000000101010

0x1400002a

---

### Tipo IM
* MOVZ X13,0x4550,LSL 0

opcode   |op| MOV_immediate | Rd
    
110100101 00 0001000111000110  01101

0xd28238cd



