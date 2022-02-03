#!/bin/bash

gdb-multiarch -ex "set architecture aarch64" -ex "target remote localhost:1234" --ex "dashboard registers -style list 'x0 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20 x21 x22 x23 x24 x25 x26 x27 x28 x29 x30 sp  pc '"  --ex "add-symbol-file main.o 0x0000000040080000" --ex "stepi 6" --ex "dashboard memory watch 0x0000000040080350 144"
