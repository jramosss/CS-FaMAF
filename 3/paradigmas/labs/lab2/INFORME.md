>Bernardi Paludi Ramos

> Universidad Nacional de Córdoba \ Facultad de Matemática, Astronomía, Física y Computación \ Paradigmas de la Programación 2020

# Informe Laboratorio 2

## Decisiones de diseño:

* En todas nuestras funciones y endpoints usamos un sistema de checkeo de errores.

* Declaramos todos las variables de clase como `protected` para que no puedan ser modificadas por otras clases/metodos.

* Declaramos `getters` para cada variable de las clases `Client`, `Freelancer`, `Job` y  `Category` 

* Declaramos el rasgo `Person` el cual tiene algunos atributos que tenian en comun `Client` y `Freelancer`

* Declaramos tambien, excepciones para que quede mas claro cual es la excepcion que queremos capturar
---
## Endpoints

Los endpoints estan implementados en el archivo `FreelanceandoServlet.scala` , ahi se pueden encontrar los endpoints:

**Aclaracion** : todos los endpoints tienen una fuerte comprobacion de errores tales como error de tipos o campos faltantes, en cualquier caso, arroja un error 400.

### GET (api/categories)
* Devuelve la lista de categorias existentes, usando el metodo `all` 
de la clase `DatabaseTable`
```scala
    var l = db.categories.all
    var elem : String = "["
    l.foreach(y => elem = elem.concat((s"{id: ${y.getId}," +
                                    s" name: ${y.getValue}")))
```
### GET (api/freelancers)
* Endpoint sobrecargado que puede tomar parametros tanto como no tomarlos, en caso de no tomar parametros, devuelve todos los freelancers en la base de datos(de la misma manera que api/categories), si tiene parametros, aplica filtrado.

### GET (api/freelancers/:id)
* Si existe, Obtiene el freelancer asociado a la id especificada, caso contrario arroja una excepcion que provoca un error 400.

### POST (api/freelancers)
* Agrega el freelancer especificado a la base de datos, en caso de error, arroja una excepcion que provoca un error 400.

### POST (api/clients)
* Crea un cliente con los atributos ingresados

### GET (api/jobs)
* Este endpoint esta sobrecargado para que pueda tomar parametros o no. En caso de no tener parametros, muestra todos los trabajos que existen en la base de datos, en caso contrario, filtra segun los argumentos dados.

### POST (api/jobs)
* Crea un nuevo trabajo con los argumentos dados

### POST (api/posts/pay)
* `client` Paga $`ammount` a `freelancer`
---
## Preguntas

### Parte 1:

####  Mencionen y expliquen al menos dos casos en los que utilizaron herencia para evitar tener que duplicar código.

* Usamos la variable `id` y su metodo getter `getId` de la clase `Model` para todas sus clases hijas

* Creamos el rasgo `Person` ya que vimos que `Client` y `Freelancer` compartian algunos atributos.


#### ¿En qué clase se encargan de controlar que las categorías asignadas a un freelancer efectivamente existan?

* En la clase `DatabaseTable`

#### ¿Por qué es responsabilidad de esa clase y no de otra?

* Creemos que es responsabilidad de esa clase ya que es accesible por todas las clases hijas de `Model` y es ahi donde se encuentran todas las
funciones para operar sobre nuestros modelos

### Parte 3

#### ¿Qué concepto de la programación orientada a objetos es el que les permite que un mismo endpoint como api/freelancers tome distintos parámetros?

* La sobrecarga, ya que hay mas de un metodo con el mismo nombre pero con diferentes argumentos a tomar

```scala
get("/api/freelancers") //Ningun parametro
get("/api/freelancers/:id") //Parametro ID
```

### Parte 4

#### ¿Dónde colocaron el código con la lógica de la acción pagar? ¿Lo dividieron entre varios objetos, o pusieron todo en un mismo lugar?

* Todo el codigo se basa en la funcion `pay` de  `Client`.


#### ¿Dónde tendrían que agregar código si se decidiera llamar a una API externa como MercadoPago para realizar la transacción?
