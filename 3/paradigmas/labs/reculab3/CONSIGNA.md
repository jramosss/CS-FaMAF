Laboratorio 3 - Programación Concurrente Usando Actores
=======================================================

> **Advertencia**: Este laboratorio requiere que **lean** (como se explicará
> más adelante con mayor detalle). Empiecen por leer **TODA** la consigna antes
> de preguntar (o empezar a hacer) nada.

Introducción
------------

En este laboratorio veremos una introducción a la programación concurrente
mediante el uso del [modelo de
actores](https://en.wikipedia.org/wiki/Actor_model), la implementación que
usaremos para esto es la librería [Akka](https://akka.io/) en su versión para
[Scala](https://doc.akka.io/docs/akka/current/typed/guide/introduction.html?language=scala).

Para que puedan comprender un poco mejor el modelo de actores, a continuación
una breve introducción al modelo de actores que tomé de la estupenda guía de
Daniel Westheide: [The Neophyte's Guide to
Scala](https://danielwestheide.com/books/the-neophytes-guide-to-scala/).

### Los problemas con el estado mutable compartido

El enfoque predominante de la concurrencia hoy en día es el del estado mutable
compartido - un gran número de objetos con estado, el cuál puede ser cambiado
por múltiples partes de la aplicación, cada una corriendo en su propio hilo.
Típicamente, el código se intercala con bloqueos de lectura y escritura, para
asegurarse de que el estado sólo pueda cambiarse de forma controlada y evitar
que múltiples hilos lo muten simultáneamente. Al mismo tiempo, nos esforzamos
por no bloquear un fragmento de código demasiado grande, ya que esto puede
ralentizar drásticamente la aplicación.

La mayoría de las veces, código como este ha sido escrito originalmente sin
tener en cuenta la concurrencia en absoluto - sólo para ser adecuado para un
mundo de hilos múltiples una vez que surgió la necesidad. Mientras que escribir
software sin la necesidad de concurrencia como este lleva a un código muy
sencillo, adaptarlo a las necesidades de un mundo concurrente lleva a un código
que muchas veces se vuelve muy difícil de leer y entender.

El principal problema con la sincronización a bajo nivel, es que es muy difícil
abstraerse sobre el código que se está escribiendo y, en consecuencia es
difícil hacerlo bien, luego ocurren errores como condiciones de carrera o
*deadlocks*, o sencillamente comportamientos extraños que son difíciles de
replicar.

### El modelo de concurrencia por actores

El modelo de concurrencia por actores tiene como objetivo evitar todos los
problemas descriptos anteriormente, permitiendo escribir código concurrente de
alto rendimiento sobre el que es más sencillo abstraerse. A diferencia del
ampliamente utilizado enfoque de estado mutable compartido, en este modelo la
concurrencia es pensada desde el diseño mismo inicial del código, i.e. no es
posible añadir soporte concurrente mediante actores para más adelante, salvo
refactorización del código.

La idea es que la aplicación consiste en un montón de entidades ligeras
llamadas actores. Cada uno de estos actores es responsable de una tarea muy
pequeña, y por lo tanto es fácil de razonar. Sobre estas tareas pequeñas se
pueden construir sistemas más complejos mediante la interacción de actores,
delegando tareas en una [jerarquía definida
previamente](https://doc.akka.io/docs/akka/current/typed/guide/tutorial_2.html)
a la escritura de código.

La aplicación: Un RSS Feed Aggregator concurrente
-------------------------------------------------

Si bien el modelo de actores está pensado para [grandes
aplicaciones](https://spark.apache.org/) que manejen concurrencia y trabajen en
computación distribuida, la verdad es que no vamos a pedir que implementen algo
tan grande, simplemente nos conformaremos con que puedan obtener una idea
[general de los
conceptos](https://danielwestheide.com/blog/the-neophytes-guide-to-scala-part-14-the-actor-approach-to-concurrency/)
de programación concurrente con Actores y puedan armar un [protocolo
sencillo](https://developer.lightbend.com/guides/akka-quickstart-scala/define-actors.html)
de pasaje de mensajes entre actores.

Sin embargo, para que no quede solo en una ida y vuelta de [impresión de
mensajes por
pantalla](https://developer.lightbend.com/guides/akka-quickstart-scala/full-example.html),
vamos a darle forma de REST API (como la vista en el Laboratorio 2, pero en
este caso será mucho más sencilla).

Para ello vamos a implementar una aplicación muy sencilla que será un agregador
de [RSS Feeds](https://en.wikipedia.org/wiki/RSS) que se resolverá de manera
concurrente. Para los que no lo sepan, RSS es un protocolo de XML que se
utiliza mucho para hacer resúmenes de páginas web que produzcan contenido
regularmente (e.g. blogs o páginas de noticias). La idea para este laboratorio
consiste en crear una API que, a pedido, pueda traer información de RSS Feeds
de varios sitios al mismo tiempo para más de un usuario. Para ello se ofrecerán
algunos endpoints que se encarguen de consumir y combinar la información de los
XML de los Feeds RSS.

### El esqueleto

Verán que el esqueleto inicial del laboratorio tiene los elementos básicos para
iniciar una REST API mediante [Akka
HTTP](https://doc.akka.io/docs/akka-http/current/introduction.html#routing-dsl-for-http-servers),
simplemente se paran en el root de su repositorio y ejecutan:

```bash
$ sbt compile
$ sbt run
Server online at http://localhost:8080/
Press RETURN to stop...
```

Luego, con el servidor corriendo, tienen habilitadas, por ahora, dos opciones:

```bash
$ http GET http://localhost:8080/ # This should return "Hello, World!" message
$ http GET http://localhost:8080/feed url=="https://www.theguardian.com/world/rss" # This will return latest items on "The Guardian" rss
```

El esqueleto de la aplicación implementa sólo dos *API Endpoints* y los hace de
manera síncrona sin utilizar actores. Su trabajo será expandir la funcionalidad
de esta aplicación sencilla mediante el uso de actores.

La implementación
-----------------

Para facilitar el trabajo y seguimiento del laboratorio, van a implementar esto
por partes, cada parte tendrá una contrapartida en el informe que se entrega
con el laboratorio, así que presten atención y no resuelvan todo mecánicamente.

> **Nota Importante**: Este laboratorio no se diseñó para ser muy largo, y
> probablemente una vez que logren entender lo que tienen que hacer lo puedan
> hacer rápido (incluso tengan tiempo suficiente para poder asegurarse algunos
> ejercicios estrella y poder sumar algunos puntos extras). 
> 
> Sin embargo, está pensado para que hagan un poco de **investigación** y
> aprendan a buscar en documentación, tutoriales,
> [stackoverflow](https://stackoverflow.com/) (donde también pueden hacer
> preguntas, aunque tengan en cuenta que la comunidad es medio hostil si le
> hacen una pregunta que ya fue hecha, y son muchas las preguntas hechas) y la
> vastedad de Internet en general. 
> 
> Van a tener que leer mucho, y vamos a esperar que lean mucho. Se les
> brindarán todos los links necesarios (muchos de ellos están en inglés, así
> que háganse a la idea de buscar documentación en inglés o bien ayúdense de
> [DeepL](https://www.deepl.com/en/translator) o [Google
> Translate](https://translate.google.com/)) y será su tarea leerlos.
> 
> Como regla general, pueden preguntar lo que quieran, pero si la respuesta es
> el primer resultado de Google, serán redirigidos a ella.

### El diseño de los actores

Lo primero que deberán hacer es aprender sobre Akka y como se construyen los
actores. Esto es transversal a todas las partes de la implementación y tiene
que ver con el diseño que se elije para llevar a cabo la implementación de los
actores. 

Es muy importante que tengan idea del esquema que van a utilizar, pueden pensar
a los Actores como su estructura de objetos (en realidad se implementan como
objetos, simplemente cambia un poco la manera en que se comunican ya que lo
hacen de manera asíncrona). Por otro lado, los mensajes que los Actores se
pasaran forman un protocolo que debe estar claramente definido.

Verán que a medida que avancen con el laboratorio, el diseño de sus modelo de
actores puede sufrir variaciones, para minimizar esto se recomienda primero
leer toda la consigna intentando establecer cuales son todos los actores de su
modelo y la jerarquía que tienen, y luego comenzar a implementar desde los
actores mas chicos (los que están más abajo de la jerarquía) y así irse
elevando a actores más globales. De todas maneras, si lo hacen bien, verán que
un diseño que puede aplicar para resolver esto no debería ser terriblemente
complejo puesto que el problema no lo es.

### Actores tipados vs no tipados

La implementación original de Akka eran los [actores no
tipados](https://doc.akka.io//docs/akka/current/actors.html#introduction). Es
una decisión que, si bien cuestionable, tenía como objetivo la simplificación
de algunos de los conceptos. Sin embargo, desde la versión 2.6 de Akka, la
implementación sugerida es con el [modelo de actores
tipados](https://doc.akka.io//docs/akka/current/typed/actors.html).

En el laboratorio no van a tener obligatoriedad por ninguno de los dos
sistemas, pueden utilizar el que más les guste. En particular, una
implementación sencilla por parte de la cátedra se hizo basándose en el modelo
no tipado que se introduce muy bien en la [guía de Daniel
Westheide](https://danielwestheide.com/blog/the-neophytes-guide-to-scala-part-14-the-actor-approach-to-concurrency/).
Ahora bien, en la [guía oficial de
Akka](https://doc.akka.io//docs/akka/current/typed/guide/introduction.html#how-to-get-started),
se usan actores tipados para introducir los conceptos. La recomendación es que
lean y vayan por la opción que más les convenza. En lo que respecta a lecturas,
se les recomienda mucho que primero lean la guía de Westheide porque es mucho
más fácil de seguir para una introducción y luego vayan por el lado de la guía
oficial de Akka que es bastante más detallada, pero también más compleja.

### Parte 1: Feeds à la carte

En esta primera parte vamos a tomar lo que ya está implementado en el esqueleto
y vamos a llevarlo a un modelo de actores, con un poco más de información. El
*endpoint* de la API que van a tener que implementar es el siguiente:

| Action | Endpoint | Params                            | Response |
| ------ | -------- | --------------------------------- | -------- |
| GET    | `/feed`  | `{url: str, since?: datetimestr}` | 200 - `{ title: str, description?: str, items: [{title: str, link: str, description?: str, pubDate: datetimestr}]` \ 400 - Bad request: can't parse `url`/`since` \ 404 - Not found: `url` doesn't exist |

#### Requerimientos funcionales

La idea es que la API toma una URL de un feed RSS y, opcionalmente, un *string*
convertible a fecha y hora con el formato `yyyy-MM-ddThh:mm:ss` (e.g.
`2020-05-10T12:30:45`). En base a esto, deberá devolver el título del RSS, su
descripción (si la tiene) y una lista de `items`, donde en cada uno haya
título, opcionalmente una descripción, un `link` y un `pubDate` que viene a ser
el día de la publicación (nuevamente en el formato convertible a fecha y hora).

El parámetro opcional `since` deberá utilizarse para filtrar todos aquellos
items cuya fecha de publicación sea anterior a el valor de la fecha `since`. Si
el parámetro no está se pasarán todos los items.

La implementación de base tiene la funcionalidad para realizar el *endpoint*
requerido, sin embargo, todo se ejecuta localmente en el mismo módulo y de
manera síncrona. 

Su trabajo es tomarlo y crear el primer actor al que le pedirán información
sobre un feed RSS específico. Para ello deberán lidiar con el [*ask
pattern*](https://doc.akka.io/docs/akka/current/actors.html#ask-send-and-receive-future)
y con
[*Futures*](https://danielwestheide.com/blog/the-neophytes-guide-to-scala-part-8-welcome-to-the-future/).

#### URLs de Feeds para probar

Si bien pueden utilizar el feed que quieran, se les deja algunos de los feeds
que la cátedra estará probando a la hora de corregir. Tengan en cuenta que
estaremos evaluando feeds [RSS](https://en.wikipedia.org/wiki/RSS), hay otro
protocolo llamado [Atom](https://en.wikipedia.org/wiki/Atom_(Web_standard)) que
no será necesario que utilicen.

Los feeds que deberán poder parsear son:

- The Guardian: https://www.theguardian.com/world/rss
- Le Monde: https://www.lemonde.fr/international/rss_full.xml
- New York Times: https://rss.nytimes.com/services/xml/rss/nyt/World.xml
- El País: https://feeds.elpais.com/mrss-s/pages/ep/site/elpais.com/section/internacional/portada
- Al Jazeera: https://www.aljazeera.com/xml/rss/all.xml

#### Links útiles

Algunos de los links que van a tener que leer antes de poder comenzar a
trabajar esta parte:

- Neophyte's Guide to Scala: Sobre
  [Futures](https://danielwestheide.com/blog/the-neophytes-guide-to-scala-part-8-welcome-to-the-future/)
  y
  [Actores](https://danielwestheide.com/blog/the-neophytes-guide-to-scala-part-14-the-actor-approach-to-concurrency/).
  Quizás les convenga darle un vistazo también a
  [Option](https://danielwestheide.com/blog/the-neophytes-guide-to-scala-part-5-the-option-type/)
  donde se hace una explicación sencilla y efectiva de como utilizar `flatMap`.
- Documentación oficial de Akka: [Classic
  Actors](https://doc.akka.io/docs/akka/current/actors.html) (si van por la
  versión no tipada) o [Getting Started
  Guide](https://doc.akka.io/docs/akka/current/typed/guide/index.html) si van
  por la versión tipada. También pueden revisar [Gettings Started
  Guide](https://doc.akka.io/docs/akka/2.5/guide/tutorial.html) de Akka 2.5 que
  fue la última versión donde los actores no tipados eran los actores por
  defecto.
- Ask pattern: Se encuentra en la [documentación oficial de
  Akka](https://doc.akka.io/docs/akka), pueden
  [buscarlo](https://doc.akka.io/docs/akka/current/actors.html#ask-send-and-receive-future),
  en la guía de Westheide también lo introduce sobre el final.
- Documentación de Akka HTTP: El servidor web para la RESTful API estará
  implementado en [Akka
  HTTP](https://doc.akka.io/docs/akka-http/current/introduction.html), la idea
  no es que lo manejen completamente, basta con que sepan lo suficiente para
  hacer un par de *endpoints* `GET` y un par de *endpoints* `POST`. En
  particular van a tener que hacer algo de uso de
  [*marshalling*](https://doc.akka.io/docs/akka-http/current/introduction.html#marshalling)
  para poder pasar `case class` a JSON y devolver. Tienen lo esencial
  implementado en el esqueleto y un ejemplo muy sencillo de un sistema de
  actores en la [documentación de Akka
  HTTP](https://doc.akka.io/docs/akka-http/current/introduction.html#streaming).
- Para poder obtener los feeds RSS necesitan alguna librería para hacer
  *requests HTTP*. La librería que recomendamos es
  [*Dispatch*](https://dispatchhttp.org/Dispatch.html) que sirve para hacer
  requests asíncronos.
- A la hora de lidiar con devolución de mensajes futuros, en Akka clásico hay
  distintas opciones ya sea haciendo uso del [*pipe
  pattern*](https://doc.akka.io/docs/akka/2.5/futures.html#use-the-pipe-pattern)
  como de [otras opciones más
  sofisticadas](https://stackoverflow.com/questions/42583322/akka-futures-pipe-different-messages-depending-on-success-or-failure),
  en Akka tipado existe un [patrón más
  estándar](https://doc.akka.io/docs/akka/current/typed/interaction-patterns.html#send-future-result-to-self)
  que es posible gracias al uso de tipos.
- Librería para XML: Si bien son libres de utilizar la librería XML que deseen
  (Scala o Java), en principio la librería de
  [Scala](https://www.scala-lang.org/api/2.9.1/scala/xml/Elem.html) cubre lo
  necesesario para este laboratorio.
- Manejo de fechas: Para parsear fechas en Scala pueden hacer uso de
  [SimpleDateFormat](https://docs.oracle.com/javase/7/docs/api/java/text/SimpleDateFormat.html)
  de Java. Aunque también pueden explorar [otras
  opciones](https://stackoverflow.com/questions/16996549/how-to-convert-string-to-date-time-in-scala).

### Parte 2: Un agregador de feeds

En la segunda parte deberán extender su aplicación anterior para incluir su
agregador de feeds. Los *endpoints* a completar son los siguientes:

| Action | Endpoint      | Params                  | Response |
| ------ | ------------- | ----------------------- | -------- |
| POST   | `/subscribe`  | `{url: str}`            | 200 - Ok \ 400 - Bad request: can't parse `url` \ 404 - Not found: `url` doesn't exist |
| GET    | `/feeds`      | `{since?: datetimestr}` | 200 - `[{ title: str, description?: str, items: [{title: str, link: str, description?: str, pubDate: datetimestr}]` \ 400 - Bad request: can't parse `since` \ 404 - Not found: there are no feeds in subscription |

#### Requerimientos funcionales

Los *endpoints* son dos, uno para subscribir nuevos canales (que el agregador
de feeds mantendrá en memoria, no implementaremos ningún tipo de persistencia),
y uno para obtener información de todos los feeds, eventualmente filtrados por
un campo `since` de fecha.

En esta parte ya deberán implementar su [jerarquía de
actores](https://doc.akka.io//docs/akka/2.5/guide/tutorial_1.html#the-akka-actor-hierarchy)
y entender quién será quién en dicha estructura. La comunicación deberá ser a
través del protocolo y deberán hacer [manejo de errores
correcto](https://danielwestheide.com/blog/the-neophytes-guide-to-scala-part-15-dealing-with-failure-in-actor-systems/)
y asegurarse de que no haya fallas o, si las hay, sean correctamente tratadas.

#### Links útiles

- Neophyte's Guide to Scala: Sobre [manejo de errores en sistemas de
  actores](https://danielwestheide.com/blog/the-neophytes-guide-to-scala-part-15-dealing-with-failure-in-actor-systems/).
- Documentación oficial de Akka: Extiende lo explicado en el blog de Westheide,
  con mayor cobertura sobre
  [supervisión](https://doc.akka.io/docs/akka/current/supervision-classic.html)
  y [tolerancia a
  fallas](https://danielwestheide.com/blog/the-neophytes-guide-to-scala-part-15-dealing-with-failure-in-actor-systems/)
  (también para la versión
  [tipada](https://doc.akka.io/docs/akka/current/typed/fault-tolerance.html)).

### Parte 3: Múltiples usuarios [Punto Estrella]

Bueno, hasta acá llega la parte que necesitan completar para aprobar el
laboratorio (casi, falta el informe que se detalla más adelante). Les dijimos
que era un laboratorio corto. Para el primer punto estrella deberán modificar
su sistema para que acepte varios usuarios que se podrán subscribir a varios
feeds cada uno (y su lista de feeds será única para su cuenta). Los *endpoints*
que tendrán que implementar son los siguientes:

| Action | Endpoint      | Params                                 | Response |
| ------ | ------------- | -------------------------------------- | -------- |
| POST   | `/user`       | `{username: str}`                      | 200 - Ok \ 409 - Conflict: `username` already registered |
| POST   | `/subscribe`  | `{url: str, username: str}`            | 200 - Ok \ 400 - Bad request: can't parse `url` \ 404 - Not found: `url`/`username` doesn't exist |
| GET    | `/feeds`      | `{username: str, since?: datetimestr}` | 200 - `[{ title: str, description?: str, items: [{title: str, link: str, description?: str, pubDate: datetimestr}]` \ 400 - Bad request: can't parse `since` \ 404 - Not found: there are no feeds in subscription for `username` or `username` doesn't exist |

#### Requerimientos funcionales

Los *endpoints* a implementar son 3, en particular deberán extender dos de los
que ya implementaron (`/subscribe` y `/feeds`) para que tomen un argumento no
opcional `username` y subscriba al usuario a un nuevo feed o devuelva los feeds
del usuario requerido respectivamente. Además tendrán que implementar un nuevo
*endpoint* que se encargará de crear un nuevo usuario en el agregador de feeds.
Tengan en cuenta que nada de esto debe ser persistente, basta con que quede
vivo en memoria mientras el servidor está en funcionamiento.

Una simplificación muy sencilla que haremos aquí es que el usuario en sí no
tendrá ningún tipo de seguridad asociada (e.g. mediante una contraseña), no es
el objetivo de este laboratorio. Sin embargo en un caso real efectivamente
necesitarían implementar algún tipo de contraseña.

Para este punto estrella deberán extender la jerarquía de actores para soportar
la noción de usuario. Cualquiera sea la jerarquía deberán respetar el protocolo
de pasaje de mensajes y asegurarse del correcto manejo de errores como lo
hicieron con los casos anteriores.

### Parte 4: Múltiples feed parsers [Punto Estrella]

#### Requerimientos funcionales

En este punto estrella deberán extender su jerarquía de actores para que su
sitema pueda parsear automáticamente feeds RSS o feeds ATOM. Es importante
denotar que el parsing no deberá ser bloqueante y resolverse de manera
asíncrona, para ello deberán extender su jerarquía de actores con herencia y
ver como tratar los distintos tipos de feeds.

### Algunas recomendaciones a la hora de probar el código

Es importante poder probar las cosas a medida que se van creando, para ello se
recomienda hacer uso del Scala REPL haciendo `sbt console` desde el directorio
raíz de su repositorio. De esa manera podrán probar las cosas que van
implementando y tendrán acceso a todas las librerías que hayan incluído en el
archivo `build.sbt`.

#### Comandos de SBT

- `compile`: Compila el código, necesario si quieren correr el código con los
  últimos cambios reflejados.
- `run`: Corre el código (servidor), debería poder detenerse con un simple
  *enter*, en caso de que no lo haga pueden lanzar *Ctrl+C*.
- `clean`: Limpia los archivos compilados (útil para asegurarse de no estar
  trabajando con alguna versión vieja de algo).
- `cleanFiles`: Similar al anterior perio limpia todos los archivos generados
  en el proceso de compilación (muy útil para asegurarse de que no haya nada
  viejo dando vueltas en el código).
- `reload`: Deberán ejecutarlo cada vez que cambiar el archivo `build.sbt`
  (e.g. agregando una librería).

#### Probar la API

##### cURL

Esta es una herramienta que se encuentra pre-instalada en la mayoría de las
distribuciones linux/OSX. Sin embargo también es bastante verbosa:

```bash
$ curl -X GET http://localhost:8080/feed\?url=https://www.theguardian.com/world/rss
```

El comando anterior devuelve por consola lo que se obtenga de hacer GET sobre
la dirección solicitada. Este resultado, si es un JSON se devolverá todo junto,
para hacer un “pretty print” se debe hacer pipe a `json_pp` (previo
instalarlo).

Para hacer un `POST` pasando un string JSON, se utiliza el siguiente comando:

```bash
$ curl -X POST http://localhost:8080/subscribe -H "Content-type: application/json" -d '{"url": "https://www.theguardian.com/world/rss"}'
```

##### HTTPie

Es una herramienta escrita en Python que simplifica mucho el uso de cURL. Se
puede instalar de [varias maneras](https://httpie.org/) (`apt-get`, `pip`,
`yum`, etc.). Los comandos anteriores se simplifican de la siguiente forma:

```bash
$ http GET http://localhost:8080/feed url==https://www.theguardian.com/world/rss # Para pasar "query parameters" en httpie se usa el "==" en lugar del "="
$ http POST http://localhost:8080/subscribe url=https://www.theguardian.com/world/rss # El operador "=" siempre se asume string, para pasar valores complejos (arrays u objetos de JSON), hay que usar ":="
```

Por otra parte, `httpie` devuelve un “pretty print” de los valores JSON.

##### Postman o https://insomnia.rest/

También existen herramientas con interfaz gráfica para mandar requests HTTP y
recibir sus respuestas, pero van a tener que investigarlas por su cuenta. Acá
tienen un [tutorial de
Postman](https://www.google.com/url?q=https://www.youtube.com/watch?v%3DqsejysrhJiU&sa=D&ust=1587083075040000&usg=AFQjCNGakoQ-DgdLWQqTp_e7ZEqXWDFhbw)
en español con un simpático acento dominicano. Insomnia se los dejamos a
ustedes.

## La entrega

Los entregables se detallan a continuación. Es importante detallar que **sólo
con la implementación no basta para aprobar el laboratorio** aún si funciona al
100% y tienen los puntos estrella hechos. Por la naturaleza de este
laboratorio, que requerirá de tiempo de lectura e investigación, habrá otros
puntos a tener en cuenta a la hora de entregar.

### Uso del repositorio

Es importante hacer uso correcto del repositorio de BitBucket, no llernarlo de
commits insignificantes (y con mensajes vagos) y hacer commits entre todos los
miembros. Recuerden que también es importante hacer commits frecuentes y no
subir todo en 1 o 2 commits.

La entrega será por medio del repositorio, con fecha límite el **Martes 9 de
Junio de 2020 a las 23:59**, deberán hacerlo por medio de un tag:

```bash
$ git tag -a 1.0 -m "Entrega Laboratorio 3"
```

Para el caso de los puntos estrella, se deberán hacer en un branch aparte y
entregarse mediante tags también (con alguna de las siguientes opciones de
acuerdo a que puntos hagan:

```bash
$ git tag -a punto-estrella-3 -m "Punto estrella 3"
$ git tag -a punto-estrella-4 -m "Punto estrella 4"
$ git tag -a puntos-estrella -m "Puntos estrella"
```

### La implementación

Para la entrega deberán completar los puntos 1 y 2 para aprobar, con la
funcionalidad requerida en cada uno de ellos, eventualmente agregando los
puntos 3 y/o 4 para obtener mejor puntuación. Se detalló previamente.

### Informe

El informe es esencial en la entrega (cubre 1/3 de la nota final), deberá
detallar claramente todos los desafíos encontrados y como los solucionaron y
además deberán contemplar los siguientes aspectos:

- Diagrama (un png/jpg que deberán subir al respositorio o bien pueden hacerlo
  en ASCII), que detallará:
    - La jerarquía de actores utilizada
    - Una justificación sobre la decisión de haberla hecha así
    - Detallar cómo se ajustarían los puntos estrella a su jerarquía (creando
      otros diagramas si fuese necesario). Esto deberán hacerlo
      independientemente de si implementan o no los puntos estrella.
- Protocolo de mensajes entre actores:
    - Describan el protocolo de pasaje de mensajes haciendo referencia entre
      qué actores se pasarían que mensajes.
    - Deben considerar también el caso extendido con los puntos estrella 3 y 4.
- ¿Cómo hicieron para comunicarse con el "mundo exterior"? (i.e. el servidor de
  la API REST).
- ¿Qué són los *Futures*?, ¿Para qué fueron utilizados en su implementación?
- ¿Qué problemas traería el implementar este sistema de manera síncrona?
- ¿Qué les asegura el sistema de pasaje de mensajes y cómo se diferencia con un
  semáforo/mutex?

### Requerimientos de diseño

- Este es un laboratorio de programación concurrente, por lo tanto deberán
  implementar todo de manera asíncrona mediante el uso de actores y futuros. 
    - El uso de `Await` sólo es válido si está bien justificado (en el informe)
      y en general sólo se usaría en caso de querer probar cosas y no para la
      versión final del código (e.g. hay una versión de como utilizar `Await`
      con la librería `Dispatch` en el código fuente de base que pueden
      utilizar como referencia para probar cosas pero no deberán utilizar en
      otra parte del código).
- Scala es un lenguaje funcional con orientación a objetos, hagan uso de ello. 
    - El uso de métodos como `map`, `filter` o `fold` es esencial. 
    - También deberán utilizar `flatMap` si la situación lo amerita (limiten el
      uso de `flatten` cuando algo se puede resolver mediante `flatMap`).
    - Hagan uso de *pattern matching*: El uso de `case class` y `case object`
      es obligatorio en los casos que lo amerite y utilizar construcciones que
      pueden ser reemplazadas por estos casos será penalizado.
    - El uso de construcciones de lenguaje imperativo (e.g. `for`, salvo para
      el caso de mónadas mediante [*for
      comprehension*](https://danielwestheide.com/blog/the-neophytes-guide-to-scala-part-5-the-option-type/))
      será penalizado.
    - Si algo puede ser inmutable siempre prefieran esa opción, limiten el uso
      de `var` y el uso de colecciones mutables. En especial en el pasaje de
      mensajes entre actores.
    - El uso de recursión a la cola, de ser necesario, no es obligatorio pero
      está recomendado.
    - Hagan uso de clases y herencia cada vez que puedan.
    - Resuelvan las excepciones mediante mónadas (i.e `scala.util.Try` en lugar
      de `try { ... } catch { ... }`).
- Agreguen a `build.sbt` cualquier librería extra que decidan utilizar y
  asegúrense de que todo sea compatible: 
    - Vean de tener correctamente configuradas las versiones de Scala y SBT en
      los archivos `build.sbt` y `project/build.properties`. 
    - Si utilizarían algún plugin que el archivo `project/plugins.sbt` esté
      agregado también.
- Modularizar es esencial, no pongan todo en el mismo `package` si las
  construcciones son claramente distintas. Nunca hagan todo en un
  archivo.

### Estilo de código

- Scala es un lenguaje que, a diferencia de Python, da lugar a hacer código
  ilegible, es importante que esto no suceda.
- El estilo de código es válido si el código es legible y está prolijo. Traten
  de no pasar de las 80 columnas, y jamás sobrepasen las 100.
- Hagan buen uso de espacios e indentaciones. Nunca utilicen tabs, siempre
  prefieran espacios. Scala suele indentarse con un espacio de `2` como base.
- Todos los archivos deben tener estilo consistente.
- El objetivo de clases, atributos y el output de métodos deben estar
  documentados en inglés. No exageren tampoco, **good code is the best
  documentation**.
- Por sobre todas las cosas, siempre recuerden
  [KISS](https://en.wikipedia.org/wiki/KISS_principle)
