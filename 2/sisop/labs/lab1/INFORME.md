> Universidad Nacional de Córdoba  Facultad de Matemática, Astronomía, Física y Computación  Sistemas Operativos 2020

> Grupo Hormigas constructoras:

> Giuliano, Maximiliano; mail: maxigiuliano18@gmail.com

> Osiecki, Agustin; mail: a.osiecki@mi.unc.edu.ar

> Ramos, Julian; mail: jramostod@gmail.com


# Informe Laboratorio 1: Crash

**Objetivo de este laboratorio:** 
* Utilizar los mecanismos de ​ concurrencia​ y ​ comunicación​ de ​ gruesa granularidad ​ que
brinda UNIX.

* Comprender que un intérprete de línea de comandos refleja la arquitectura y estructura
interna de las primitivas de comunicación y concurrencia.

* Utilizar ​ **buenas prácticas de programación​:** estilo de código, tipos abstractos de datos
(TAD), prueba unitaria (​ unit testing ​ ), prueba de caja cerrada (​ black box testing​ ),
programación defensiva; así como herramientas de ​ debugging ​ de programas y memoria.

## Introduccion:  

#### En este laboratorio implementamos todos los modulos necesarios para ejecutar comandos simples y comandos anidados con pipes.

---
## **Modularizacion**

### Descripcion general de los modulos:

#### **Command**:
Comenzamos desarrollando el modulo, ya que este contiene todas las funciones y tads para trabajar con un pipeline, tales como:

Por un lado tenemos el **scommand**, que contiene la estructura y funciones necesarias para trabajar con un comando simple.  

Con nuestras funciones podemos:


**Funciones publicas**
* **scommand_new()**: Crear un comando simple 
* **scommand_destroy()**: Destruir un comando simple 
* **scommand_front()**: Obtener el primer elemento de un comando
* **scommand_pop_front()**: Eliminar el primer elemento de un comando
* **scommand_push_back()**: Agregar un elemento al final de la cola 
* **scommand_set_redir_out()**: Establecer el archivo de redireccion externa 
* **scommand_set_redir_in()**: Establecer el archivo de redireccion interna 
* **scommand_is_empty()**: Determinar si el comando es vacio o no
* **scommand_length()**: Retornar la cantidad de argumentos que tiene nuestro comando
* **scommand_get_redir_out()**: Retornar el nombre del archivo de redireccion externa 
* **scommand_get_redir_in()**: Retornar el nombre del archivo de redireccion interna 
* **scommand_to string()**: Retorna el comando en formato string
**scommand_peek_nth()**: Retornar el comando en formato string
* Retornar el n-esimo argumento de un comando, util para iteraciones sobre el comando (Agregada por nosotros)  
**Funciones privadas/estaticas**
* **scommand_char_length()**: Retornar el largo de un comando en caracteres, utilizada para hacer allocs del tamaño adecuado de nuestro comando.

#### **Pipeline**
Por otro lado tenemos el tad **pipeline**, que tiene funciones para trabajar con varios comandos "pipeados":

**Funciones publicas**
* **pipeline_new()**: Crear un pipeline
* **pipeline_destroy(**) Destruir un pipeline
* **pipeline_front()**: Obtener el primer comando de un pipeline
* **pipeline_pop_front()**: Eliminar el primer comando de un pipeline
* **pipeline_set_wait()**: Setear si el pipeline tiene que esperar o no 
* **pipeline_is_empty()**: Determinar si el pipeline es vacio
* **pipeline_length()**: Retornar la cantidad de comandos que tiene un pipeline
* **pipeline_get_wait()**: Determinar si el pipeline espera o no
* **pipeline_to_string()**: Retornar el pipeline en formato string   
**Funciones privadas/estaticas**
* **pipeline_char_length()**: Retorna el largo en caracteres de nuestro pipeline, util para allocar espacio preciso

Ninguna de las funciones nos represento una gran dificultad, ya que la mayoria de funciones de scommand era solamente checkear la precondicion y ejecutar una funcion provista por glib.  
Las unicas funciones que nos generaron algun tipo de problema fueron las _to_string, ya que nos generaban leaks, y tenian que ser liberadas por quien las llamase.

#### **Builtin**:

Luego continuamos con el modulo **builtin** ya que vimos que contenia 2 funciones fundamentales para el modulo execute

No nos detuvimos mucho en este modulo, ya que simplemente era hacer unas comprobaciones y las syscalls correspondientes, pero basicamente tiene dos funciones:

* Determinar si un comando es _interno_, es decir que no realiza forks para su ejecucion (builtin_is_internal)
    ```c
    bool builtin_is_internal(scommand cmd) {
        return strcmp(scommand_front(cmd),"cd")==0
        || strcmp(scommand_front(cmd),"exit") == 0;
    }
    ```
    Solo comprobamos si el comando es _cd_ o _exit_ ya que esto es lo que pedia la consigna.

* Ejecutar un comando interno (builtin_exec()).

#### **Strextra**:  
Contiene una sola funcion: **strmerge()**, que concatena dos strings allocando la memoria suficiente para ambos.

#### **Execute**:
Por otro lado tenemos el **execute**, en donde utilizamos las funciones de los modulos anteriormente nombrados, con el fin de ejecutar un comando.

**Funciones Publicas**
* execute_pipeline(): Ejecuta un pipeline.

**Funciones privadas/estaticas**
* command_build(): arma el comando en un formato char** para ser ejecutable con **execvp()**
* exec_simple_command(): Ejecuta un comando simple
* handle_redirs(): Si existen redirectorios, los abre con open_redir()
* open_redir(): Abre un directorio con las flags correspondientes.


En esta parte del proyecto tuvimos que utilizar muchas syscalls vistas en las clases teoricas, tales como:

* **pipe()**.
* **close() y open()**: Para abrir y cerrar archivos.
* **fork()**: Para correr bloques de codigo en los segmentos hijo.
* **execvp()**: Para ejecutar comandos que no sean builtin.
* **dup2()**: Para cambiar los filedescriptors por las entradas de salida/escritura de nuestro pipe.

#### Crash
Este es el modulo "main", ya que es el encargado de ejecutar todos los modulos anteriormente nombrados, este modulo no nos tomo mucho tiempo, se basaba en funciones simples.

## Decisiones de diseño: 

* Decidimos usar el TAD cola ( Gqueue ) de la libreria Glib a diferencia de la lista porque nos resulto mas facil acceder a la cabeza y cola de una secuencia, y al trabajar con cadenas de texto nos resulto mas eficiente.

* Decidimos utilizar el TAD Gstring de la libreria Glib en conjunto con string.h (sugerido por la catedra) porque nos brinda funciones que nos fueron de mucha utilidad para poder desarrollar la primera parte del proyecto.

* Creamos varios macros ya que vimos que varios segmentos de codigo se repetian. Ademas, algunos como TRY (execute.c) nos ayudaban a hacer debugging, por que este checkeaba si habia habido algun error con la syscall, y en caso afirmativo nos decia donde se habia originado el error.
---

#### Esta fue la parte mas dificil del proyecto, es complicado jugar con pipes y con syscalls, pero nos sirvio mucho para comprender estas mismas.
---

## TAD's

### **scommand_s**: 

Sigue la estructura de lo mencionado en el pdf del proyecto
``` c
typedef struct scommand_s {
    GQueue* queue;
    GString* in;
    GString* out;
}scommand_s;
```

### **pipeline_s**:

Al igual que scommand sigue la estructura de lo mencionado en el pdf del proyecto
```c
typedef struct pipeline_s
{
    GQueue* scomm_queue;
    bool term;
} pipeline_s;
```
---

## Tecnicas de programacion
* **Funciones de librerias**:
    * <gmodule.h>: TAD cola (GQueue) y sus funciones
    * <fcntl.h>: open/close, dup2, fork
    * <wait.h>: wait()
    * <string.h>: strcat()
    * <stdlib.h>: calloc(), free()
    * <sys/types.h>: provee varios tipos utiles

* **Manejo de memoria dinamica**: Para reservar memoria usamos la funcion calloc de la libreria stdlib.h, tambien usamos free para liberar la memoria reservada por calloc.

* **Estilo de codigo**: 
    * Ante un condicional que solo ejecute una instruccion, lo poniamos sin llaves.
    ```c
    if(!pid1)
        exec_simple_command(apipe);
    else{
        if(pipeline_get_wait(apipe))
            waitpid(pid1,NULL,0);
    }
    ```
    * No nos pasamos nunca de las 80 columnas, para facilitar legibilidad.
    
* **Programacion defensiva**: Para evitar errores optamos por usar Asserts en las funciones que eran necesarias, junto con la cantidad necesaria de condicionales para verificar las diferentes situaciones que se nos podian presentar, como por ej: 
    * Si nos llegaban mas de dos comandos
    * Si un comando era vacio 
    * Si un comando no existe (es nulo)

    entre otras verificaciones mas.

---
## Herramientas de programacion
 Para el desarrollo del proyecyo utilizamos el IDE visual studio code, para compilar gcc, para debugear nos aferramos a gdb y para detectar _memory leaks_ usamos valgrind.

---
## Desarrollo  

### **Proceso**:   
Al comienzo del desarrollo del laboratorio nos lanzamos a implementar las funciones de scommand, ya que vimos que estas eran simples, de una linea, usando solo las funciones de glib. Por suerte uno de nosotros ya tenia conocimientos sobre _bitbucket_ y no nos detuvimos mucho en eso.

Luego pasamos al pipeline, donde tampoco tuvimos mayores complicaciones, ya que era usar las funciones antes descriptas y escribir un poco mas de codigo.

Usamos mains hechos por nosotros en las funciones, ya que usar los tests era como matar a un mosquito con un cañon, por que no teniamos todas las funciones hechas todavia.

Despues pasamos a lo que consideramos la "segunda parte" del proyecto, los modulos builtin y execute.
Builtin fue facil, eran funciones simples y no nos tomo mas de 10 minutos, la cruz fue el execute.
En execute estuvimos mucho mas tiempo de lo que pensabamos, entendiendo como manejar los redirs, como usar las syscalls, manejar codigo de errores, etc.

Tambien nos atrasamos mucho por que comenzamos haciendo la implementacion para que pueda ejecutar solo 2 comandos, y luego lo aumentamos a n comandos.

Finalmente hicimos el modulo crash para poder testear bien todas las funciones sin tener que armar un main cada vez que quisieramos probar un comando distinto u otra alternativa.


### **Problemas**:
Los mayores problemas los encontramos en las funciones _to_string() y en execute_pipeline().
to_string era problematica en ambos casos ya que su memoria no podia ser liberada en la misma funcion, sino que por parte de quien lo llamaba, y eso nos generaba leaks siempre, ademas que nosotros allocabamos la memoria necesaria para el comando o el pipeline, en base a la cantidad de argumentos, y no al largo de los mismos, lo cual nos generaba un _aborted corruped size_, lo solucionamos creando las funciones _char_length().

El otro gran problema fue en el execute pipeline, ya que no lograbamos la comunicacion entre pipes.


---
## Conclusiones
El laboratorio nos sirvio mucho para comprender mejor los contenidos vistos en el teorico, como se comportan las syscalls, los forks, la necesidad de cerrar los directorios que se abren, etc.