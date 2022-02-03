## Decisiones de diseño

* Utilizamos la monada Maybe en las funciones `unfold`, `compute`  y `time`, para que en casos de errores no se realice ningun acción. Ej: si el usuario intenta repetir un sonido i-veces siendo i negativo no se realice nada.

* Creamos las funciones auxiliares (pitchear, pitcheopp, disarm y proc) que le permiten al unfold desarmar un `Fragment` e ir aplicando X operacion en cada nota y rearmarlo. Es decir, transformo el `Song` en una lista de `Primitive Pitch` con la funcion `disarm()`, dicha lista es procesada con `pitcheopp()` que se encarga de pitchear cada nota de la lista con `pitchear()` y devolver el `Song` que originalmente habiamos desarmado.


* Tambien implementamos otras funciones secundarias `bindeo` y `main` para poder reproducir tanto un `(Maybe Song)` como un `Song`.