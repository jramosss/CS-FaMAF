## **Decisiones de diseño**

* Utilizamos la monada Maybe en las funciones `unfold`, `compute`  y `time`, para que en casos de errores no se realice ningun acción. Ej: si el usuario intenta repetir un sonido i-veces siendo i negativo no se realice nada.

* Creamos las funciones auxiliares (pitchear, pitcheopp, disarm y proc) que le permiten al unfold desarmar un `Fragment` e ir aplicando X operacion en cada nota y rearmarlo. Es decir, transformo el `Song` en una lista de `Primitive Pitch` con la funcion `disarm()`, dicha lista es procesada con `pitcheopp()` que se encarga de pitchear cada nota de la lista con `pitchear()` y devolver el `Song` que originalmente habiamos desarmado.


* Tambien implementamos otras funciones secundarias `bindeo` y `main` para poder reproducir tanto un `(Maybe Song)` como un `Song`.

Respecto a la cantidad de commits al final, tuvimos un pequeño problema con los tags, pero ya esta solucionado.


## **Correciones realizadas**

### **Install acomodado**
El orden de instalacion de dependecias y el comando para compilar eran erroneos.


### **Unfold completado** 

Dado que nos habiamos olvidado de abarcar los casos: `unfold( Fragment(....) )`, `unfold( Parallel(....) )` y `unfold( Concat(....) )` fueron agregados correctamente.

Además se completo para que tambien se pueda disminuir una nota, es decir, acepta `Transpose_by (-x) ...`

### **Funciones auxiliares** 
Las funciones auxiliares `pitchear`, `pitcheopp` y `disarm`, renombradas `transponer`, `ensamblar`, y `desarm` respectivamente, fueron modificadas en los casos bases ya que se tenian en cuenta cosas que no se utilizaban.

### **Estilo**
Con respecto a este punto, se mantuvo todo el codigo para no sobrepasar las 80 columnas. Ademas se genero un estilo de codigo para cada funcion, dejandolas de la siguiente manera:

```haskell
-- nombre_de_funcion: descripcion breve

nombre_de_funcion :: A -> B

nombre_de_funcion _ _ = ...
-- (entre cada caso dejamos un espacio para que sea mas legible)
nombre_de_funcion _ _ = ...

...
```

### **En este commit de correcciones 1.1 se agrego lo siguiente:**
Con respecto al uso de conceptos de alto orden, se hizo uso de la funcion **map** para poder trasponer las notas, con esto logramos disminuir una gran cantidad de codigo y funciones auxiliares (ensamblar, trasponer y proc). Dejando un codigo mucho mas legible. El resto de las funciones no se han tocado, con respecto a la correcion 1.0, dado que estan funcionando al 100% y no daba ninguna ventaja implementarlos con map, filter, o alguna otra funcion de alto orden.
