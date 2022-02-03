ARMGNU ?= aarch64-linux-gnu

AOPS = --warn --fatal-warnings

asm : main.list

all : asm

clean :
	rm -f *.o
	rm -f *.img
	rm -f *.hex
	rm -f *.list
	rm -f *.elf
	rm -f memory_map.txt

main.o : main.s
	$(ARMGNU)-as $(AOPS) main.s -o main.o

eq.o:
	$(ARMGNU)-as $(AOPS) tests/test_beq.s -o eq.o

eq: memmap eq.o 
	$(ARMGNU)-ld eq.o -T memmap -o eq.elf -M > memory_map.txt
	$(ARMGNU)-objdump -D eq.elf > eq.list

ne.o:
	$(ARMGNU)-as $(AOPS) tests/test_bne.s -o ne.o

ne: memmap ne.o 
	$(ARMGNU)-ld ne.o -T memmap -o ne.elf -M > memory_map.txt
	$(ARMGNU)-objdump -D ne.elf > ne.list

hs.o:
	$(ARMGNU)-as $(AOPS) tests/test_bhs.s -o hs.o

hs: memmap hs.o 
	$(ARMGNU)-ld hs.o -T memmap -o hs.elf -M > memory_map.txt
	$(ARMGNU)-objdump -D hs.elf > hs.list

lo.o:
	$(ARMGNU)-as $(AOPS) tests/test_blo.s -o lo.o

lo: memmap lo.o 
	$(ARMGNU)-ld lo.o -T memmap -o lo.elf -M > memory_map.txt
	$(ARMGNU)-objdump -D lo.elf > lo.list

mi.o:
	$(ARMGNU)-as $(AOPS) tests/test_blo.s -o mi.o

mi: memmap mi.o 
	$(ARMGNU)-ld mi.o -T memmap -o mi.elf -M > memory_map.txt
	$(ARMGNU)-objdump -D mi.elf > mi.list

pl.o:
	$(ARMGNU)-as $(AOPS) tests/test_bpl.s -o pl.o

pl: memmap pl.o 
	$(ARMGNU)-ld pl.o -T memmap -o pl.elf -M > memory_map.txt
	$(ARMGNU)-objdump -D pl.elf > pl.list

#vs:
#	$(ARMGNU)-as $(AOPS) test_bvs.s -o vs.o

#vc:
#	$(ARMGNU)-as $(AOPS) test_bvc.s -o vc.o

hi.o:
	$(ARMGNU)-as $(AOPS) tests/test_bhi.s -o hi.o

hi: memmap hi.o 
	$(ARMGNU)-ld hi.o -T memmap -o hi.elf -M > memory_map.txt
	$(ARMGNU)-objdump -D hi.elf > hi.list

ls.o:
	$(ARMGNU)-as $(AOPS) tests/test_bls.s -o ls.o

ls: memmap ls.o 
	$(ARMGNU)-ld ls.o -T memmap -o ls.elf -M > memory_map.txt
	$(ARMGNU)-objdump -D ls.elf > ls.list

ge.o:
	$(ARMGNU)-as $(AOPS) tests/test_bge.s -o ge.o

ge: memmap ge.o 
	$(ARMGNU)-ld ge.o -T memmap -o ge.elf -M > memory_map.txt
	$(ARMGNU)-objdump -D ge.elf > ge.list

lt.o:
	$(ARMGNU)-as $(AOPS) tests/test_blt.s -o lt.o

lt: memmap lt.o 
	$(ARMGNU)-ld lt.o -T memmap -o lt.elf -M > memory_map.txt
	$(ARMGNU)-objdump -D lt.elf > lt.list

gt.o:
	$(ARMGNU)-as $(AOPS) tests/test_bgt.s -o gt.o

gt: memmap gt.o 
	$(ARMGNU)-ld gt.o -T memmap -o gt.elf -M > memory_map.txt
	$(ARMGNU)-objdump -D gt.elf > gt.list

le.o:
	$(ARMGNU)-as $(AOPS) tests/test_ble.s -o le.o

le: memmap le.o 
	$(ARMGNU)-ld le.o -T memmap -o le.elf -M > memory_map.txt
	$(ARMGNU)-objdump -D le.elf > le.list

main.list: memmap main.o 
	$(ARMGNU)-ld main.o -T memmap -o main.elf -M > memory_map.txt
	$(ARMGNU)-objdump -D main.elf > main.list
	
