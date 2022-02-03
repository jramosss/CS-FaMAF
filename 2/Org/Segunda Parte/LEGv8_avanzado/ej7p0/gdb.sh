#!/bin/bash

gdb-multiarch -ex "set architecture aarch64" -ex "target remote localhost:1234" --ex "add-symbol-file main.o 0x0000000040080000" --ex "stepi 6"
