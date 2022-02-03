# Discreta 2

## AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA

### Que materia detestable

### DIGO, ¿CON UNA NO HACIA FALTA?

Todavia no la aprobe, pero vengo a dejar las cosas que hice.
Lo primero que dice Penazzi cuando presenta es que esta materia no es <s>Discreta 2</s> sino Algoritmos 3, Lo cual tiene bastante sentido por que toda la materia son distintos algoritmos, todos relacionados con grafos, asi que si pensaron que no lo iban a ver nunca mas despues de Discreta 1 lamento decirles que no.

Es muy pesada, cada vez que agarras un algoritmo nuevo cuesta mucho interiorizarlo, pero una vez que lo agarras ya te queda muy grabado en la cabeza.

Todavia no la aprobe asi que no tengo mucho mas para decirles por parte del practico.

## Hablemos del lab

El lab (que esta en [LAB](./lab.txt)) es el mas largo de la carrera, es menos amplio que el de ingenieria, pero en el de ingenieria son 6 personas y en este solo 3.

La definicion del lab es **Renegar** por que:

- Es en C
- No se pueden usar librerias que no sean las standard
- No les dan NADA, nisiquiera la estructura (que es de las cosas que mas tiempo lleva)
- Ya dije que es en C?
- El profe no da ninguna ayuda, solo tira de vez en cuando las cosas que NO hay que hacer, llevandolo al punto que te dice textualmente "HACER ESTO ES FUCKING STUPID".

Lo hice 2 veces al lab, por lo que tengo muchos consejos para darles.

### Consejos:

- Busquen un buen grupo, es muy importante, probablemente de los proyectos mas pesados y largos de la carrera, muy distinto a todos los otros. Conocí mucha gente que tenian voluntad pero su grupo no, asi que se quedaron libres.
- Creen un repo desde el segundo 0, si no saben usar git van a aprender aca.
- No se compliquen mucho con la estructura, es de las cosas que mas tiempo llevan, pero si estan pensando en usar un diccionario o un arbol binario pueden pasar dos cosas:  
   1- Son unos genios incomprendidos y estan a punto de hacer el algoritmo mas eficiente de la tierra  
   2- Se estan complicando mucho  
  Asi que ojo, usen arreglos unidimensionales y bidimensionales, mas alla de eso capaz se les este yendo el mambo.
- Si recien estan arrancando y no saben para donde disparar les dejo un mini roadmap
  - 1- Definir la estructura, asegurarse que sea liviana, que los arreglos no se pasen del millon de millones (por que eso daria heap overflows si no me equivoco). Tambien asegurense que uno pueda tener un vertice, encontrar sus vecinos y saber sus datos.
  - 2- Empezar con el parsing del grafo (Esto toma mucho tiempo) y van a tener que aprender a usar funciones bien primitivas de c como lo son fscanf, atoi, memcmp, etc.
  - 3- Hacer las funciones para extraer datos de un vertice.
  - 4- Empezar con las funciones pedidas.
  - 5- Testing unitario, de cada funcion pedida se hace un test aparte.
- No usen la estructura interna para nada mas que el parsing y las funciones para extraer datos del vertice (Por que no se tiene que hacer, si lo hacen esta mal y Penazzi los mata), todo lo que se tenga que entregar tiene que usar las funciones `Nombre`, `Color`, etc.
- **USEN EL HEAP PARA TODO**, osea hagan alloc para toda la memoria, no pueden dejar nada en el stack por que si o si les da stack overflow.
- Organicen bien la division en carpetas, ni se les ocurra meter todas las funciones y el parser en un solo archivo, no les voy a decir que no se puede, pero por dios saquense eso de la cabeza por que es una practica horrenda y se van a complicar mucho, por que sino se hace mucho lio, les recomiendo que hagan
  - `src/` : Metan todas las funciones relacionadas al manejo de grafo ahi, dividida en varios archivos, uno con cada "tematica", por ejemplo nosotros hicimos coloreo.c que tenia el Greedy y Bipartito.
  - `tests/` : HAGAN TESTS, y les recomendaria que no los hagan en c, que si bien se puede, no se pueden hacer cosas tan copadas como con python. Aprendan a usar la libreria subprocess (que es super simple) y con eso pueden jugar muchisimo.
  - `ADT/TAD/`: Seguramente vayan a necesitar tipos abstractos, tales como una cola (Para bipartito si o si). Ahi metan todas.
  - grafos/: Aca metan todos los archivos de grafos.
  - mains/: Aca tienen que meter todos los main usados para los tests.
- Hagan un archivo `utils.c` en el cual tengan funciones que sepan que se usen en varios archivos, asi no repiten codigo y queda todo mas organizado.
- Asegurense de entender los algoritmos antes de empezar a implementarlos, si se mandan a escribir codigo les va a quedar un algoritmo O(n^2).
- En años pasados, Penazzi dijo que Greedy tenia que correr 1000 veces con 1000 AleatorizarVertices en menos de 15 min con grafos grandes en una computadora de Famaf (4 GB de RAM). Lo cual suena bastante imposible, en 2021 aflojo y dijo que si no corria en 15 minutos no pasaba nada, que podia tardar 20 mientras no fuese un algoritmo O(n^2) o peor.
- Usen TADs, no se resistan por que les hacen la vida mucho mas facil, nosotros usamos:
  - HashTable
  - Queue
  - Set
- Traten que el parsing sea rapido, es importante por que les ahorra MUCHO tiempo de debugging, es una pasta querer probar tu programa y que cada vez que lo tires tarde 6 minutos.
- Usen valgrind, si su programa tiene memory leaks y corre 1000 veces imaginen lo que puede llegar a pasar.
- No se olviden de correr con -O3, nosotros nos olvidamos la primera vez y tarda mucho mas.
- Aprendan a usar Makefile, no es dificil y les ahorra muchisimo, una vez que aprendes a usarlo se hace todo solo.
- **ARRANQUENLO CON MUCHO TIEMPO, TARDA MAS DE 1 MES EN HACERSE.**
