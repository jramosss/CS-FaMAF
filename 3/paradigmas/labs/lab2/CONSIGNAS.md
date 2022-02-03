> Universidad Nacional de Córdoba \ Facultad de Matemática, Astronomía, Física y Computación \ Paradigmas de la Programación 2020

# Laboratorio 2 - Programación Orientada a Objetos

## Introducción
### ¿Qué es un backend?
Las aplicaciones web se dividen usualmente en dos grandes partes: frontend y backend, que corresponden a los componentes ejecutados en el cliente y en el servidor. El frontend es el encargado de interpretar y visualizar el contenido para el usuario. El backend, ejecutado en el servidor, implementa los diferentes procesos de negocios, que usualmente involucran interactuar con alguna base de datos o con otros servicios, y enviarle toda la información necesaria al frontend para que pueda mostrarla. Recibe consultas o *requests HTTP* de múltiples clientes al mismo tiempo, las encola y eventualmente envía las respuestas correspondientes.

En este laboratorio implementaremos un backend, es decir, un servidor que recibirá distintas consultas y devolverá las respuestas correspondientes. Los frontend y backend no son siempre tan independientes, pero cada vez es más común el desarrollo de aplicaciones web sin un frontend, y que proveen acceso a sus funcionalidades a través de un conjunto bien definido de interfaces que se denomina *API*. Este tipo de aplicaciones con sólo el backend, denominadas servicios web, son muy comunes en la actualidad y permite la generación de nuevos productos mediante la integración de diferentes servicios web.

### Las API REST

Un buen ejemplo para entender cómo funcionan los servicios web es la [API de MercadoLibre](https://developers.mercadolibre.com.ar/es_ar/api-docs-es) que permite a desarrolladores externos a la empresa interactuar con su base de datos directamente, sin tener que utilizar la interfaz gráfica de la web. Por ejemplo, podemos consultar todas las categorías de productos si hacemos un GET a la URL https://api.mercadolibre.com/sites/MLA/categories

Como ya dijimos, para “llamar” a una API, lo hacemos a través de *requests*, que pueden ser de varios tipos. Cuando accedemos a una URL a través del navegador, como en el ejemplo de la API de Mercado Libre, estamos realizando un request GET. Estos normalmente se utilizan para consultar información de la base de datos. Para alterar la base de datos, se utilizan los PUT, POST y DELETE, pero nosotros por simplicidad sólo usaremos requests de tipo POST.

En las filminas hay más detalles sobre las APIs y cómo las vamos a implementar.
## Requerimientos funcionales
Vamos a implementar un backend para un directorio de freelancers. Una simplificación muy burda de sitios como [freelancer.com](https://www.freelancer.com/), [TopTal](https://www.toptal.com/) o [Upwork](upwork.com).

Todo proyecto siempre comienza con la necesidad de un usuario (que pueden ser ustedes mismos). En este caso, desde la cátedra hicimos un estudio de mercado de miles de pesos y les damos una especificación de las operaciones que su backend tiene que soportar para poder satisfacer las necesidades de los usuarios. O dicho de otra forma, estos son los *requerimientos funcionales* para poder aprobar el lab.

La plataforma está construida para dos tipos de usuarios: *clientes* y *freelancers*. Los clientes pueden crear *trabajos* (o *job* en inglés), buscar freelancers para contratar y pagarles. Los freelancers pueden buscar trabajos y recibir pagos.

Para mantener la información organizada, los freelancers y los trabajos van a tener categorías asignadas. Por ejemplo: *Web development*, *Mobile*, *Translation*, etc. Esto va a facilitar la búsqueda tanto de freelancers (por parte de clientes), como de trabajos (por parte de freelancers).

Ambos tipos de usuario van a tener un perfil donde pueden otros usuarios pueden ver su información antes de aceptar un contrato. Por ejemplo, los clientes muestran su nombre de usuario, su país, los trabajos que crearon y cuánto gastaron en la plataforma, mientras que los freelancers muestran su nombre de usuario, su país, lo que cobran por hora, las categorías en las que trabajan, su nivel de experiencia (experto, avanzado o principiante) y cuánto ganaron en la plataforma.

Los trabajos también tienen que tener cierta información para ser atractivos a los freelancers. Como ya dijimos, tendrán una categoría, pero además tendrán un título,  un monto por hora que el cliente está dispuesto a pagar por ese trabajo, una preferencia de país (algunos clientes sólo quieren freelancers de su mismo país por cuestiones de idioma y zona horaria), y una preferencia de experiencia (algunos clientes prefieren freelancers principiantes, y baratos, mientras que otros prefieren pagar más por el trabajo de un experto).

Como nuestra aplicación va a ser super exitosa y esperamos miles de trabajos en la primera semana, tenemos que permitirles a los usuarios filtrar lo que les interesa más fácilmente. Un cliente debería poder filtrar los freelancers por categoría, país y/o reputación. Un freelancer debería poder filtrar trabajos por categoría, país y/o reputación.
### Simplificaciones
Como esto es un prototipo, vamos a simplificar algunas cosas:
* Las categorías no pueden crearse, están fijas en la plataforma.
* Los usuarios pueden ser o freelancers o clientes, no los dos al mismo tiempo.
* Todos los números en la plataforma serán enteros. Esto es para evitar los errores de redondeo y formato al leerlos o convertirlos en strings.
* Los atributos de los objetos en Scala se escriben normalmente en camelCase, mientras que las claves de los archivos json se escriben en underscore_case. Para tener que evitarnos tener que convertir de un formato al otro, vamos a romper el estilo de código de Scala y poner todos los atributos que van a terminar en un json en underscore_case.
* Si hay algún error al leer o escribir la base de datos causado por el sistema operativo, lo vamos a ignorar. Es decir, podemos asumir que en la versión terminada del proyecto, las funciones  `DatabaseTable.write` y `DatabaseTable.loadJsonFile` siempre devuelve `Success`. Mientras estén desarrollando, estas funciones pueden fallar si los archivos json no están bien escritos, o su código no hace lo que tiene que hacer. Esos problemas, por supuesto, sí deben solucionarlos.

## Requerimientos de diseño

* El servidor no puede devolver un error 500 *bajo ningún concepto*.
* Si un fragmento de código se repite frecuentemente, abstráiganlo mediante una función, o mejor, un método común a varias clases.
* NO reinventen la rueda. Busquen en internet la forma más fácil de parsear json, hagan pattern matching, usen funciones como filter y map, etc.
* Este lab les vamos a bajar puntos por estilo de código. Usen líneas de menos de 80 caracteres y una indentación de dos espacios *consistente*. Pongan nombres de variables significativos y respeten las convenciones de capitalización.
* Pongan nuevas clases en archivos distintos, dentro del directorio `Models`.
## Informe
**EL INFORME VALE DOS PUNTOS DE LA NOTA FINAL**

Aparte del código, tienen que entregar un archivo `INFORME.md` donde tienen que responder a las preguntas que les planteamos dentro de esta consigna. (Sí, para saber cuáles son, ¡van a tener que leer la consigna completa!)
Además, pueden incluir otras decisiones de diseño que tomen al interpretar la consigna, y los puntos estrella que hagan, si hacen alguno.

## Configuración del entorno

* Instalar java 8
```bash
sudo apt-get update
sudo apt install openjdk-8-jdk openjdk-8-jre
```

* Instalar SBT siguiendo las instrucciones de [esta página](https://www.scala-sbt.org/download.html)

## Implementación

El resultado final de este laboratorio es un sistema bastante complejo, entonces lo vamos a implementar por partes. Siempre que se encuentren ante un proyecto grande, hay que dividirlo en pequeñas partes más fáciles de implementar. Acá les mostramos una forma en que se podría dividir el trabajo.
### Persistencia de datos
La mayoría de las aplicaciones web tienen que guardar sus datos en algún lado, para que si el servidor se apaga, se puedan recuperar y volver a levantar la aplicación. Para eso, normalmente se usan *bases de datos*, que van a estudiar en materias siguientes. Como esto es un ejercicio, nosotros vamos a leer y guardar nuestros datos en archivos con formato json. Una de las motivaciones para hacer esto es que los objetos en nuestro backend ya son convertidos a json para poder enviarlos a través de la API al frontend, así que podemos reutilizar este código para guardarlos en archivos.

**Aclaración** Nunca, pero nunca, una aplicación real va a preferir usar archivos json en lugar de una base de datos. Esto lo hacemos sólo como un ejercicio para ejemplificar los conceptos de POO.
### Estructura de clases
En este proyecto, tenemos que:
Manejar muchísima información de distintos tipos.
La información está estructurada, es decir, ciertas partes del sistema (por ejemplo, los clientes) siguen siempre un mismo esqueleto.
Las operaciones sobre esta información son complejas. Por un lado tenemos el código que lee y escribe a la base de datos, y por el otro tenemos el código que determina cómo la información se relaciona entre sí, como por ejemplo que cada contrato es creado por un cliente.
Hay mucho código que se repite. Por ejemplo, hay muchas cosas que necesitan ser convertidas a json y guardadas. Hay muchas cosas que necesitan ser filtradas.

El paradigmas de orientación a objetos es una *herramienta* para organizar la información estructurada JUNTO CON las operaciones que la modifican en clases. Sirve, justamente, para mantener nuestro código ordenado, en paquetitos separados, y para poder reutilizar código que es común a muchas partes.

Para empezar, vamos a definir una clase para cada entidad de la aplicación. La primera de ellas será `Category`. Segundo, vamos a crear un archivo `.json` por cada clase, que va a contener una lista de los objetos creados que sean de esa clase. Cada instancia va a ser representada como un diccionario que mapea el nombre de los atributos con sus valores. Pueden ver el archivo `database/categories.json` como ejemplo.

Les proveemos una gran parte del código que se encarga de leer y escribir estas archivos, organizado en las clases:
* `Model`: es una abstracción de “cosas” que quiero leer o guardar en la base de datos. Los modelos tienen funciones para rellenar sus atributos a partir de strings con formato json, y funciones para convertirse en diccionarios que luego serán guardados en los archivos. Además, todos los objetos tienen un `id` para poder distinguirlos entre sí.
* `DatabaseTable`: es una abstracción de una tabla. Las tablas tienen una lista de las instancias, y provee funciones para buscar, agregar y borrar instancias. Como leer y escribir desde archivos en el disco duro lleva mucho tiempo, nuestras tablas mantienen una lista de los objetos existentes en memoria en el atributo `_instances` (aunque la solución real a este problema es muchísimo más compleja).
* `Database`: es un conjunto de `DatabaseTable`. Cuando iniciamos la aplicación, “cargamos” las tablas en memoria y mantenemos una referencia a la base de datos mientras que el servidor esté levantado.

### Polimorfismo de datos en Scala.
Van a notar que los tipos `DatabaseTable` y `Model` son polimórficos. Es decir, toman un tipo como argumento, similar al polimorfismo que vimos en el lab 1 con el tipo Command. En el caso de la clase `DatabaseTable`, es polimórfica porque cada tabla se compone solamente de un tipo de modelos, y necesita saber qué modelo es para poder construir nuevas instancias cuando se carga desde la base de datos.

Por otra parte, el polimorfismo de la clase `Model` es más complejo y se llama “polimorfismo recursivo”. La razón por la cual esto fue necesario está relacionada con el sistema de tipos estáticos de Scala y no la vamos a analizar en este lab.

### Framework Web

Finalmente, y como no vamos a reinventar la rueda, vamos a usar Scalatra para que maneje todo el protocolo HTTP de las requests y responses.
### Parte 1: Categorías y Freelancers
Al correr el esqueleto por primera vez, tienen el endpoint `GET api/categories` implementado, pero no devuelve lo que debería. Deben obtener la lista de instancias de `Category`, y completar el código para que el valor de retorno esté en formato JSON. Para eso, pueden extender el `ScalatraServlet` para soportar este tipo de operaciones. Miren los links abajo para más información sobre cómo hacer esto.

Lo segundo que deberán hacer es implementar el endpoint `GET api/freelancers`, que devuelve una lista de freelancers presentes en la plataforma. Como todavía no hemos implementado los endpoints POST que permiten crear freelancers, pueden agregar los freelancers directamente en el/los archivos json. Pista: usen las funciones `super` para poder llamar a los métodos de la superclase Model.

Como tercera parte, tienen que implementar el endpoint  `GET api/freelancers/:id`, que recibe argumentos en la misma URL y devuelve toda la información para construir el perfil del freelancer. Les recomendamos implementar (y utilizar) el método `DatabaseTable.get`.
Este endpoint es el primero que tiene errores: debe devolver un error 400 si el parámetro id no es un entero, o si el freelancer no existe. Para manejar estos casos de error sin que el programa explote, pueden usar por un lado excepciones, y por el otro pattern matching con tipo `Option[_]` (el equivalente al Maybe de Haskell).

Finalmente, tienen que agregar el endpoint `POST api/freelancers` que recibe un diccionario json con valores de los atributos, guarda un nuevo freelancer en la base de datos y devuelve el id asignado por la tabla. Debe devolver un error 400 en los tres casos detallados en la especificación de la API. Pista: usen `flatMap`.

Al final de esta parte, su backend debe cumplir la siguiente especificación:

| Action | Endpoint             | Params                                                                                       | Response                                                                                                                                                                                                  | Notes                                                                                                            |
|--------|----------------------|----------------------------------------------------------------------------------------------|-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|------------------------------------------------------------------------------------------------------------------|
| GET    | /api/categories      |                                                                                              | 200 - [{id: int, name: str}]                                                                                                                                                                              |                                                                                                                  |
| GET    | /api/freelancers     |                                                                                              | 200 - [{id: int, username: str, country_code: str, category_ids: [int], reputation: str, hourly_price: float}]                                                                                            |                                                                                                                  |
| GET    | /api/freelancers/:id |                                                                                              | 200 - {id: int, username: str, country_code: str, category_ids: [int], reputation: str, hourly_price: int, total_earnings: int} \ 400 - Bad Request If Id is not an Int of the freelancer does not exist. |                                                                                                                  |
| POST   | /api/freelancers/    | {username: str, country_code: str, category_ids: [int], reputation?: str, hourly_price: int} | 200 - id \ 400 - Bad Request if: * Can’t parse json * Json fields names or types are incorrect * There are no categories with those ids * The received parameters have a key id."                         | If `reputation` is not present as a parameter, create freelancer with value “junior”. Total earnings start on 0. |

Con esta primera parte, han visto cómo responder a distintos tipos de consultas HTTP, manejo de pattern matching y excepciones en Scala, y algunas aplicaciones de los conceptos de orientación a objetos. Preguntas para responder:
Mencionen y expliquen al menos dos casos en los que utilizaron herencia para evitar tener que duplicar código.
¿En qué clase se encargan de controlar que las categorías asignadas a un freelancer efectivamente existan? ¿Por qué es responsabilidad de esa clase y no de otra?
### Parte 2: Clientes y trabajos
Para la segunda parte tienen que agregar todos los endpoints GET y POST para las entidades clientes y trabajos.


| Method | Endpoint         | Params                                                                                                              | Response                                                                                                                             |
|--------|------------------|---------------------------------------------------------------------------------------------------------------------|--------------------------------------------------------------------------------------------------------------------------------------|
| GET    | /api/clients     |                                                                                                                     | 200 - [{id: int, username: str, country_code: str, total_spend: Int}]                                                                |
| GET    | /api/clients/:id |                                                                                                                     | 200 - {id: int, username: str, country_code: str, total_spend: Int, job_ids: [int]} \ 400 - Bad Request                              |
| POST   | /api/clients     | {username: str, country_code: str}                                                                                  | 200 - id: int                                                                                                                        |
| GET    | /api/jobs        |                                                                                                                     | 200 - [{id: int, title: str, category_id: int, client_id: int, preferred_expertise: str, preferred_country: str, hourly_price: int}] |
| POST   | /api/jobs        | {title: str, category_id: int, client_id: int, preferred_expertise: str, preferred_country: str, hourly_price: int} | 200 - id:int \ 400 - Bad Request if: \* the client does not exists \* any of the categories does not exists                          |

### Parte 3: Filtrado
También tienen que implementar los endpoints `GET api/freelancers` y `GET api/jobs` con parámetros opcionales. En el caso de `GET api/freelancers`, si no tiene ningún parámetro, seguimos devolviendo la lista completa de freelancers. Si tiene parámetros de filtro adicionales (uno o más), por ejemplo `country_code=AR`, sólo se deben devolver los freelancers que tengan el valor `AR` para el atributo `country_code`. En caso de que haya más de un filtro, se deben devolver las instancias que cumplan con todos los filtros. El caso del filtro category es distinto, ya que se deben devolver los freelancers que incluyan el id de la categoría recibido en su lista de categorías.

**AYUDA**: hacer las funciones de filtro es mucho más fácil de lo que parece. Traten de modificar sólo las clases DatabaseTable y Model, ya que esas modificaciones servirán también para los otros modelos. En Scala, es fácil saber [si un diccionario está incluído en otro diccionario](https://stackoverflow.com/questions/25189198/in-scala-how-to-check-if-a-map-contains-all-entries-from-another-map), sin saber de antemano cuáles son las claves de ninguno de los diccionarios.
Para el caso del filtro de category sí van a tener que implementar una parte particular para freelancer, pero traten de sobre-escribir lo que hicieron para el caso general, en lugar de hacer un método con un nombre nuevo.

Al finalizar esta etapa, el backend debe agregar los siguientes endpoints:


| Method | Endpoint  | Params  | Response   |
|--------|-----------|------|---------|
| GET    | api/freelancers/ | {country_code?: str, category_id?: id, reputation?: str}                                    | 200 - [{id: int, username: str, country_code: str, category_ids: [int], reputation: str, hourly_price: float}]                       |
| GET    | api/posts/       | {preferred_country?: str, category_id?: id, preferred_reputation?: str, hourly_price?: int} | 200 - [{id: int, title: str, category_id: int, client_id: int, preferred_expertise: str, preferred_country: str, hourly_price: int}] |

Para responder:
* ¿Qué concepto de la programación orientada a objetos es el que les permite que un mismo endpoint como `api/freelancers` tome distintos parámetros?

### Parte 4: Pagos

El último punto que tienen que implementar es el endpoint de pago. La acción (pagar) puede involucrar muchos pasos. Se tendrían que comprobar si el método de pago del cliente es válido, si la cuenta del freelancer es válida, si existen fondos suficientes. Como estamos haciendo un prototipo, no vamos a implementar todo eso, sólo tienen que cambiar los montos del cliente y el freelancer. Pero lo importante es que piensen en qué parte del código tiene que ir la lógica de cómo pagar.

Al finalizar esta etapa, el backend debe agregar el siguiente endpoint:


| Method | Endpoint      | Params                                         | Response |
|--------|---------------|------------------------------------------------|------------------------------------------------------------------------|
| POST   | api/posts/pay | {freelancert_id: int, job_id:int, amount: int} | 200 or 400 - Bad request if freelancer of job do not exist in database. |

Para responder:
* ¿Dónde colocaron el código con la lógica de la acción pagar? ¿Lo dividieron entre varios objetos, o pusieron todo en un mismo lugar?
* ¿Dónde tendrían que agregar código si se decidiera llamar a una API externa como MercadoPago para realizar la transacción?

## ¿Cómo probar la API?

### cURL
Esta es una herramienta que se encuentra pre-instalada en la mayoría de las distribuciones linux/OSX. Sin embargo también es bastante verbosa:
```sh
$ curl -X GET http://localhost:8080/api/categories
```
El comando anterior devuelve por consola lo que se obtenga de hacer GET sobre la dirección solicitada. Este resultado, si es un JSON se devolverá todo junto, para hacer un “pretty print” se debe hacer pipe a `json_pp` (previo instalarlo).
Para hacer un `POST` pasando un string JSON, se utiliza el siguiente comando:
```sh
$ curl -X POST http://localhost:8080/api/freelancers -H "Content-type: application/json" -d '{"username": "freelancer", "country_code": "AR", "categories": [1, 2], "hourly_price": 40}'
```

### HTTPie
Es una herramienta escrita en Python que simplifica mucho el uso de cURL. Se puede instalar de [varias maneras](https://httpie.org/) (`apt-get`, `pip`, `yum`, etc.). Los comandos anteriores se simplifican de la siguiente forma:
```sh
$ http http://localhost:8080/api/categories  # GET es por defecto
$ http POST http://localhost:8080/api/freelancers username=freelancer country_code=AR categories:='[1, 2]' hourly_price=40  # El operador "=" siempre se asume string, para pasar valores complejos (arrays u objetos de JSON), hay que usar ":="
```
Por otra parte, `httpie` devuelve un “pretty print” de los valores JSON.

### Postman o https://insomnia.rest/
También existen herramientas con interfaz gráfica para mandar requests HTTP y recibir sus respuestas, pero van a tener que investigarlas por su cuenta. Acá tienen un [tutorial de Postman](https://www.google.com/url?q=https://www.youtube.com/watch?v%3DqsejysrhJiU&sa=D&ust=1587083075040000&usg=AFQjCNGakoQ-DgdLWQqTp_e7ZEqXWDFhbw) en español con un simpático acento dominicano. Insomnia se los dejamos a ustedes.
## Entrega
* Fecha de entrega MARTES 12 DE MAYO a las 23:59
* Deberán crear un tag indicando el release para corregir.
```bash
git tag -a 1.0 -m 'entrega lab 2'
```
* Si agregan puntos estrella, háganlo en un branch aparte, y documenten lo que hicieron en el informe.
## Links y documentación
* Para scala en general
  - [From Python to Scala](https://crscardellino.github.io/archive/): Guía   escrita por el profesor Cristian Cardellino. Es una iniciación a Scala desde   Python. No está completa ni es exhaustiva (y puede estar un poco   desactualizada), pero cubre con lo básico para comenzar. En el link está el   archivo del blog de Cristian, y de ahi pueden acceder a todas las entradas   (año 2014). Disclaimer, está en inglés, pero le pueden preguntar a Cristian   ante cualquier duda.
  - [Documentación Oficial de Scala](https://docs.scala-lang.org/): Es muy buena  y es la referencia sobre la que siempre se tienen que basar. Cualquier consulta sobre la [API de Scala](https://docs.scala-lang.org/api/all.html) puede ser resuelta en este lugar. Pero además se ofrecen varios aspectos más básicos como el [Tour of Scala](https://docs.scala-lang.org/tour/tour-of-scala.html): que cubre más que suficiente todos lo necesario que van a tener que utilizar en el  laboratorio.
* [Sobre métodos GET y POST](http://blog.micayael.com/2011/02/09/metodos-get-vs-post-del-http/): Para leer y entender un poco más sobre los conceptos que vienen detrás de las  REST APIs.
* [Tips para el desarrollo utilizando POO](https://scotch.io/bar-talk/s-o-l-i-d-the-first-five-principles-of-object-oriented-design)
* [¿Cómo usar Json en Scalatra?](https://scalatra.org/guides/2.3/formats/json.html)
* [¿Cómo leer los parámetros en un GET?](https://www.oreilly.com/library/view/scala-cookbook/9781449340292/ch15s08.html)
* [Tipos de errores HTTP en Scalatra](https://scalatra.org/guides/2.5/http/actions.html)
* [Functional Error Handling](https://docs.scala-lang.org/overviews/scala-book/functional-error-handling.html)

## Puntos estrella

* Agregar un trait User y utilizar herencia múltiple en todos los tipos de usuario.
Hacer que la reputación de un freelancer sea un Enumeration, y serializarlo/descerializarlo correctamente.
* Agregar filtros de orden. Los clientes pueden filtrar freelancers que cobren menos (o más) de XX cantidad por hora, los freelancers pueden filtrar trabajos que paguen más (o menos) de XX cantidad por hora. Pueden cambiar los parámetros que toma cada endpoint.
* En lugar de guardar en un atributo el id de la categoría correspondiente a un freelancer, guardar una referencia a la instancia de Category correspondiente. Esto va a involucrar modificar la forma en que se crean los modelos, porque ahora además de recibir como parámetro la DatabaseTable correspondiente a esa clase, tendrán que recibir también otras tablas relacionadas (en este caso Category).
* Implementar endpoints para crear contratos con los atributos y métodos que quieran, y modificar el endpoint de pago para que sea efectuado sobre un contrato, en lugar de sobre un job y un freelancer.

