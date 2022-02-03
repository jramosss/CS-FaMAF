# Lab Funcional

El objetivo de este lab es que se familiaricen con el paradigma
funcional. Para ello, van a tener que codificar un programa para
escribir música en Haskell.

A grandes rasgos, el programa debe tener comandos para crear una
canción. La canción consta de un *stack* de fragmentos. Los fragmentos
son a su vez una serie de notas y silencios construidos a partir de
ciertos comandos que se listan abajo. Cuando el programa se termina,
se considera que el primer fragmento del stack es la canción final,
que puede ser el resultado de combinar otros fragmentos. Para
simplificar, las canciones siempre van a estar en 4/4 (pueden omitir
este dato si no saben lo que significa).

Para el trabajo, vamos a utilizar la librería
[Euterpea](http://euterpea.com/). En la parte de links abajo
encontrarán información util.

En música una nota tiene un tono (Do, Re, Mi, Fa, Sol, La, Si), tal
vez *sostenido* o *bemol*, que en Euterpea se escribe según el sistema
anglosajón (A = La, B = Si, C = Do, D = Re, E = Mi, F = Fa, G = sol),
con el sostenido siendo una *s* agregada a la nota (Cs, Ds, ...), y el
bemol siendo una *f*. Además, una nota tiene una *octava* (*pitch* en
inglés) --que es esencialmente un número natural (0, 1, ...)-- y que
determina qué tan alto o bajo debe sonar. Finalmente, una nota tiene
una duración (cuánto tiempo debe sonar): lo que el sistema latino
denomina *redonda* (4 bits), *blanca* (2 bits), *negra* (1 bit),
*corchea* (1/2 bit), *semicorchea* (1/4 bit), etc, en Euterpea se
denomina *whole note*, *half note*, *quarter note*, etc.

También existen los silencios, que duran lo que se indique según el
mismo tipo de notación de duración que las notas.

Como ejemplo, en Euterpea, se puede escribir la cadencia musical Do Re
Re# Silencio Sol en la 2nda octava, todas negras, como:

```haskell
[Note qn (C, 2), Note qn (D, 2), Note qn (Ds, 2), Rest qn, Note qn (G, 2)]
```

El objetivo del laboratorio va a ser crear estructuras para "compilar"
canciones.  Primero vamos a tener un tipo llamado `Song` que va a
representar ciertas operaciones sobre las notas:

- `Fragment ns` crea una canción dada la lista de notas *ns* de tipo
  `[Primitive Pitch]`.

- `Transpose_by i s` transpone todas las notas de *s* en *i*
  semi-tonos. Por ejemplo, si *s* (de tipo `Song`) representa la
  canción `[Note qn (A, 1), Note qn (B, 1), Note qn (C, 1)]`, entonces
  `Transpose_by 1 s` representa la canción `[Note qn (As, 1), Note qn (C,
  1), Note qn (Cs, 1)]` (la adición de `s` a la nota indica
  "sostenido", y el Sí sostenido es un Do).

- `Repeat i s` repite *i* veces la canción *s*.

- `Concat s s'` concatena las canciones *s* y *s´*.

- `Parallel s s'` representa la canción compuesta por la ejecución de
  *s* y *s'* en conjunto.

Por ejemplo, los acordes (Do, Sol) y (Sol, Re) repetidos cuatro veces,
con cada acorde sonando 4 bits, se puede escribir como:

```haskell
chord_c = Parallel (Fragment [Note qn (C, 3)]) (Fragment [Note qn (G, 3)])
chord_g = Parallel (Fragment [Note qn (G, 3)]) (Fragment [Note qn (D, 3)])
Repeat 4 (Concat (Repeat 4 chord_c) (Repeat 4 chord_g))
```

Definido el tipo `Song`, procederemos a definir las siguientes operaciones:

- `unfold s` retorna la canción *s* sin nodos de transporte
  (`Transpose_by`) ni repetición (`Repeat`), es decir, en donde estos
  nodos son reemplazados por la acción que representan (transponer la
  notas o repetir, respectivamente).

- `compute s` toma la canción *s* y produce un elemento de tipo `Music
  Pitch` (de Euterpea). Esta función no debe crear nodos `Modify`, ni
  funciones que las generan (como `transpose`). Puede utilizar `unfold`.

- `time s` computa el tiempo de la canción *s* (en bits). Por ejemplo,
  la canción de la introducción dura 5 cuartos.


## Un stack de canciones

En la canción de ejemplo de mas arriba, la canción con acordes de Do y
Sol, primero hemos creado los acordes y luego compusimos la canción
final. Vamos a crear una nueva estructura que permita hacer esto,
mediante la manipulación de un stack. Pero vamos a aprovechar el
polimorfismo de Haskell para hacerlo de forma genérica; no centrada en
las canciones.

Si pensamos en cómo se hizo la canción, pero se "agregaron" dos
canciones, correspondientes a los acordes de Do y Sol respectivamente,
y luego se "operó" sobre estas canciones. Pensándolo mas
abstractamente, vamos a necesitar un tipo que permita agregar
elementos al stack, y luego referirse a los elementos para operar
sobre ellos.

En concreto, tendremos el tipo `Command a` con operaciones `Add`,
`Use1`, y `Use2`. La primera agrega un elemento en la cabeza del
stack, y las otras dos agregan en la cabeza del stack el resultado de
operar con uno o dos elementos indexados del stack. Luego tendremos
una función: 

```haskell
run :: [Command a] -> [a] -> Maybe [a]
```

Que devolverá algo o no de acuerdo a si pudo o no ejecutar todos los
comandos. Por ejemplo, los siguientes comandos dejan en `song` la
misma canción que la de ejemplo.

```haskell
add_chord_c = Add (Parallel (Fragment [Note qn (C, 3)]) (Fragment [Note qn (G, 3)]))
add_chord_g = Add (Parallel (Fragment [Note qn (G, 3)]) (Fragment [Note qn (D, 3)]))
join_chords = Use2 1 0 (\c g -> Repeat 4 (Concat (Repeat 4 c) (Repeat 4 g)))
song = run [add_chord_c, add_chord_g, join_cords] [] >>= \r-> return (r!!0)
```

Notar el uso de `Use2`: se le pasa los enteros que corresponden a la
posición en el stack de cada pedazo de canción, y luego una función
que toma dos argumentos: en este caso esos argumentos serán los
acordes.


## Escuchando las composiciones

Para los curiosos, pueden escuchar sus creaciones utilizando el
servicio midi `Timidity`.

En ubuntu, por ejemplo, basta hacer:

    sudo apt install timidity -yes

Para ejecutar el servicio:

    timidity -iA -Os

Luego, pueden utilizar la función de Euterpea `devices` para listar
los posibles dispositivos. A mi me muestra:

```
...

Output devices:
  OutputDeviceID 0	Midi Through Port-0
  OutputDeviceID 2	TiMidity port 0
  ...
```

El ID que primero diga `TiMidity` debería andar, en mi caso `2`. Luego:

```haskell
playDev 2 (compute song)
```

Debería hacer sonar la canción construida arriba.


## Consignas

1. Es inaceptable que un programa explote por que alguien se
   equivocó. Para eso está el tipo `Maybe`, que permite indicar cuando
   el programa devuelve algo o cuando hubo un error.

2. Lean la documentación de Haskell acerca de las listas, y eviten
   crear de nuevo funciones que operen sobre las mismas que ya se
   encuentren disponibles. Casi seguro que si están haciendo pattern
   matching con una lista, están repitiendo el código de una función
   de librería.

3. Respetar la estructura de directorio indicada en clase: `bin` para
   ejecutables, `src` para código, `Makefile`, `README.md`, e
   `INSTALL.md`.

4. Comentar en el `README.md` toda decisión de diseño que consideren
   importante mencionar, y en el `INSTALL.md` cómo compilar y ejecutar
   el programa, y si requieren alguna dependencia especial.

5. Su programa debe compilar en el lab. Si por alguna razón no pueden
   cumplir con este requerimiento (por ejemplo, utilizan
   funcionalidades que no se encuentran en la versión del compilador
   del lab), aclaren esto en el `INSTALL.md`.

## Características de la presentación

 * Fecha de entrega: hasta el 07/04/2020 a las 23:59:59.999

 * Deberán crear un tag indicando el release para corregir.

		git tag -a 1.0 -m 'entrega lab 1'

 * Si no está el tag no se corrige. Tampoco se consideran commits
   posteriores al tag.

## Recomendaciones

 * Diseñen pensando en reutilizar código y abstraer funcionalidad
   cuando sea posible.

 * Si un fragmento de código se repite frecuentemente, abstraiganlo
   mediante una función.

 * Sean consistentes con la nomenclatura. Tengan en cuenta que Haskell
   impone que los constructores y los módulos empiecen con mayúscula,
   y las funciones con minúscula.

 * Documenten las funciones y las porciones de código que no sean
   intuitivas.

 * No abusen de los comentarios en el código. Tampoco escatimen.

 * Si la definición de una funcionalidad es ambigua, busquen
   clarificaciones antes de codificar basados en supuestos. Esto es
   responsabilidad de ustedes.

 * En este laboratorio les vamos a corregir estilo de código, que
   debe ser homogéneo y seguir algunos lineamientos básicos. Lo más
   importate es que sea fácil de leer. Les dejamos una guía de estilo
   al final.

## Instalación

Para instalar Euterpea, primero deben instalar Haskell y Cabal, aunque es posible que ya lo tengan en sus sistemas. En un ubuntu 18.04, los siguientes comandos son suficientes:

```bash
    $ sudo apt-get install haskell-platform
    $ sudo apt-get install libasound2-dev
    $ cabal update
    $ cabal install Euterpea
```

## Puntos estrella

Para sumar porotos o divertirse un poco pueden hacer un subconjunto arbitrario de estos puntos:

  * Escribir funciones `parallelMin` y `parallelMax` de tipo 
    `Song -> Song -> Song`, que dadas canciones `s1` y `s2` retorna una nueva
    `Song` con el resultado de componer dos canciones en paralelo
    (`Parallel s1' s2'`), donde `s1'` y `s2'` son como `s1` y `s2`
    pero de forma tal que ambas canciones tengan el mismo tamaño. En
    el caso de `parallelMin`, se corta la canción mas larga para que
    tenga el mismo tamaño que la mas corta, y en `parallelMax` se
    rellena con silencio la mas corta.

  * (Conviene hacer el punto anterior primero. Difícil!) Hacer la función
    `to_tracks` que convierte una `Song` en otra `Song` equivalente
    pero que sólo tenga `Parallel`s anidados al comienzo. Por ejemplo,
    `to_tracks (Concat (Parallel s1 s2) (Parallel s3 s4))`, con `time
    s1 < time s2` y `time s4 < time s3` devolvería `Parallel (Concat
    (Concat s1 r1) s3) (Concat s2 (Concat s4 r4))`, donde `r1` y `r2`
    son silencios por la diferencia entre las sub-canciones.


## Links y datos de interés

 * [Sobre las notas](https://es.wikipedia.org/wiki/Nota_(sonido)).

 * [Sobre la longitud de la nota](https://es.wikipedia.org/wiki/Figura_musical).

 * [Tutorial](https://github.com/Euterpea/Euterpea2-Examples/blob/master/NoteLevel/SimpleMusic.lhs)
   Tutorial rápido para hacer composiciones en Euterpea.

 * [Referencia rápida Euterpea](http://euterpea.com/wp-content/uploads/2016/12/Euterpea_Quick_Reference.pdf)

 * `Maybe` forma una mónada, lo que permite utilizar operadores como
   el `>>=` (y evitar así el pattern-matching). [Aquí]
   (https://www.haskell.org/tutorial/monads.html) hay un tutorial de
   mónadas, pero seguramente no sea el más intuitivo. Si bien no es
   fundamental para el trabajo, ¡es un concepto interesante para
   aprender!

* [Guía de estilo para haskel](https://wiki.haskell.org/Programming_guidelines)
