<!---
 -
 - Author   2019 Gonzalo Peralta
 -
 - Revision 209
 -
 -
 -->

Ejercicio 1: 

En esta carpeta vas a encontrar un archivo main.c y un directorio input.
input contiene varios archivos con arrays dentro de ellos.
En cada archivo, el arrar se representa con su size y luego cada uno de los
miembros.

por ejemplo el array = [1,2,3,4,5] es representado en el archivo como:

5
1 2 3 4 5

main.c es tu archivo principal, donde programaras el ejercicio.

Se pide que tu programa principal, lea el array dentro de cada uno de los archivos
dentro de input y que luego de leerlo imprima por pantalla su contenido de la siguiente forma:

[ 1, 2, 3, 4, 5]


Algunas sugerencias:
No esperes hasta el final para compilar, podes ir compilando unidades independientes de tu
codigo.


Las intrucciones para compilar son:

Algunos links interesantes=> https://es.wikibooks.org/wiki/Programaci%C3%B3n_en_C/Compilar_un_programa/Linux

En tu caso, para compilar usa:
gcc -Wall -Werror -Wextra -pedantic -std=c99 -o reader main.c


para correr el programa:
./reader input/archivo

donde archivo es alguno de los archivos que se encuentra dentro de input.

Cambiar el archivo input y ejecutar nuevamente

Ejercicio 2:

Modificar main.c para que en lugar de leer el archivo input, lea cada uno de los miembros del
array por pantalla  y luego los muestre en la misma.


Luego de este lab, el alumno debe ser capaz de:
1. Saber como compilar el programa
2. Tener manejo de las instrucciones basicas del lenguaje de programacion c
3. Tener manejo de standard input y standard output.
4. Tener nociones del manejo de parametros a traves de la funcion principal main()

