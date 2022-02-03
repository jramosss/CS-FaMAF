> Universidad Nacional de Córdoba  --  Facultad de Matemática, Astronomía, Física y Computación  --  Sistemas Operativos 2020

> Grupo Hormigas Constructoras:

> Giuliano, Maximiliano; mail: maxigiuliano18@gmail.com

> Osiecki, Agustin; mail: a.osiecki@mi.unc.edu.ar

> Ramos, Julian; mail: jramostod@gmail.com



# Informe Laboratorio 2: Programación VGA

**Objetivos del laboratorio:**

* Manejar un dispositivo.
* Programar en userspace y kernelspace.
* Manejar el ciclo edición-compilación-ejecución en xv6.
* Agregar syscalls.
* Utilizar puertos para acceder a registros.

---

 ## Parte I

La primera parte consistió en la implementación de una función llamada **vgainit()**, en la cual debiamos lograr la impresión de un pie de pantalla con el mensaje **"SO2020"**. Para esto, investigamos el macro **P2V**, definido en el archivo **memlayout.h** como:

 ```c 
 define KERNBASE 0x80000000

 define P2V(a) ((void *)(((char *) (a)) + KERNBASE))   
 ```

P2V convierte de una dirección física a una dirección virtual en la memoria del kernel. El kernel de xv6 se asegura de que cada page table se asigne (solo para uso del kernel) a la dirección virtual 0x80000000 + X a la dirección física X, y P2V usa estas direcciones. ( Fuente: https://www.cs.virginia.edu/~cr4bd/4414/F2018/paging-and-protection.html ).

Como xv6 inicia por defecto en modo texto, utilizamos el color rojo para el fondo y blanco para los caracteres, y luego buscamos los equivalentes hexadecimales para escribir el mensaje. Asi, **vgainit()** queda de la siguiente manera: 

 ```c 
void
vgainit(void)
{
  *(int *)P2V(0xB8F94) = 0x4353;
  *(int *)P2V(0xB8F96) = 0x434F;
  *(int *)P2V(0xB8F98) = 0x4332;
  *(int *)P2V(0xB8F9A) = 0x4330;
  *(int *)P2V(0xB8F9C) = 0x4332;
  *(int *)P2V(0xB8F9E) = 0x4330;
}  
 ```
Las direcciones señaladas por "P2V" ( i.e. 0xB8F94 ) fueron puestas especificamente para que quedase como un pie de pagina, abajo a la derecha.

---
## Parte II

Para esta etapa, debiamos de extender **console.c** con funciones que nos permitieran realizar el cambio de modo texto a modo gráfico ( y viceversa ), utilizando las configuraciones de 320x200x256 para el modo gráfico, y 80x50 para el modo texto; luego usarlas para realizar el cambio de modo desde **vgainit()**.

Para esto, buscamos informacion al respecto en las fuentes provistas por la catedra. La que nos fue de mas ayuda, fue el código extraido de la pagina _https://files.osdev.org/mirrors/geezer/osd/graphics/modes.c_ , el cual contiene implementaciones de funciones adaptables a xv6, de acceso a puertos y registros. De ahi extrajimos la función 
```c 
write_regs(char *regs);
``` 
la cual toma un arreglo de numeros en hexadecimal, y escribe cada uno de estos en los puertos correspondientes para el cambio de modo. Éstos arreglos también estaban definidos en el código de la página, por lo que tomamos los correspondientes a los modos texto y gráfico a utilizar, y los guardamos en el archivo **modes.h**, para incluirlo en **console.c**.

Así, solo quedó definir 2 funciones que nos permitieran _setear_ un modo o el otro. Éstas son **set_text_mode** y **set_graphic_mode**, definidas como:
 ```c 
void 
set_text_mode (void)
{
  write_regs(textmode);
}

void
set_graphic_mode (void)
{
  write_regs(graphicmode);
}
 ```
 donde los argumentos _textmode_ y _graphicmode_ son los arreglos que contienen la informacion a ser escrita en los puertos por 
 **write_regs()**.

---

## Parte III

### `modeswitch(short):`
La funcion no tiene mayores complicaciones
```c
//Switch to text/graphic mode
void 
modeswitch(short option){
  if (!option)
    set_text_mode();
  else 
    set_graphic_mode();
}
```
(Definida en proc.c)

Lo complicado de esta parte fue ver que archivos habia que modificar para que esto fuera considerada una syscall:

* Implementamos nuestra funcion en `proc.c` (mostrado arriba)
* En `sysproc.c` implementamos como la consola va a llamar a nuestra funcion, adaptando los argumentos pasados por consola para la funcion.
  ```c
  void
  sys_modeswitch(void)
  {
    int option;
    argint(0,&option);
    modeswitch(option);
  }
  ```
  con la funcion `argint` tomamos el n-esimo argumento pasado por consola y se lo asignamos a nuestra variable. 
* Definimos nuestra syscall en `syscall.h: `  `#define SYS_modeswitch 22`
* Asociamos nuestra syscall definida anteriormente, con la funcion que definimos en sysproc.c  en `syscall.c: ` `[SYS_modeswitch] sys_modeswitch`

* Extendemos la visibilidad de nuestra funcion a TODOS los archivos, definiendola como `extern int sys_modeswitch(short);` en `syscall.c`



### `plotpixel(int,int.int):`
El procedimiento es el mismo que el anterior, la definicion cambia.

```c
void
sys_plotpixel(void)
{
   int x;
   int y;
   int color;
   argint(0, &x);
   argint(1, &y);
   argint(2, &color);
   plotpixel(x,y,color);
}
``` 
en `sysproc.c`

```c
void
plotpixel (int x, int y, int color)
{
  int offset;
  offset = (int)0xA0000 + 320*y + x;
  *(uchar *)P2V(offset) = color;
}
``` 
en `proc.c`

## Parte IV:

### Modulo draw.c
Definimos en este archivo funciones para el grafico que queriamos representar, el grafico consiste en la muestra de un cartel de tipo "alerta" que enuncia la frase "DESTROY ALL O.S.".  

Las siguientes funciones son las principales del programa:  

```c
void fondo(void); //Pone todo el fondo de la terminal en negro
void printsquare (int x, int y, int color, int linesize); //dibuja un cuadrado x+linesize * y + linesize pixeles de el color dado
void vertical_line(int x, int y, int length, int color, int linesize); //Dibuja una linea vertical de y + length pixeles del color dado
void horizontal_line(int x, int y, int length, int color, int linesize); //Dibuja una linea horizontal de x+length pixeles del color dado
void diagonal_dec(int x, int y, int length, int color, int linesize);  //Dibuja una linea diagonal decreciente de largo length
void diagonal_inc(int x, int y, int length, int color, int linesize);  //Dibuja una linea diagonal creciente de largo length
void warning_sign(int x, int y,int color);  //Dibuja los signos de warning
```
Las funciones de linea( i.e. `vertical_line()`, `diagonal_dec()`) fueron todas implementadas de forma similar: utilizando ciclos y llamando a `printsquare()`, como se muestra en las siguientes lineas:

```c
void
vertical_line(int x, int y, int length, int color, int linesize)
{
    for (int j = y; j < y + length; j++)
        printsquare(x,j,color,linesize);
}

void
diagonal_dec(int x, int y, int length, int color, int linesize)
{
    int k = 0; 
      while (k < length){
          printsquare(x+k,y+k,color,linesize);
          k++;
      }
}
```

Luego, añadimos funciones que pintaran cada letra, implementadas a partir de las funciones de linea, y ejecutadas luego en las coordenadas y el color especificados, en la funcion `main()`. A modo de ejemplo, la siguiente es la implementacion de la letra D:

```c
void
letraD(int x, int y,int color, int linesize)
{
    vertical_line(x,y,25,color,linesize);
    diagonal_dec(x+3,y,13,color,linesize); //x+3 por el ancho de printsquare
    diagonal_inc(x+3,y+24,11,color,linesize);
}
```
Las funciones de las otras letras son muy similares por lo que las vamos a omitir en este archivo.  
Utilizamos la funcion `int sleep(int)`, definida en el archivo **user.h** para agregar pausas en la ejecución del programa.
Ademas, agregamos carteles de peligro intermitentes, tambien alternados con `sleep()`.

Luego implementamos funciones `firstline()`,`secondline()`,`thirdline()`, para separar en que linea va cada palabra:  
DESTROY  
  ALL  
  O.S.

Dejando asi un codigo mas limpio y modularizado.

---
  
### Conclusion:  
Consideramos que este laboratorio nos sirvio mucho para familiarizanos con xv6 y parte de la implementacion de un sistema operativo.  
Requirio mucha lectura de nuestra parte, tanto de xv6 como de los archivos de ayuda que nos dejo la catedra.
