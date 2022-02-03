> Bernardi Paludi Ramos

> Universidad Nacional de Córdoba  Facultad de Matemática, Astronomía, Física y Computación  Paradigmas de Programación 2020

# Informe Laboratorio 3

El **objetivo de este laboratorio** es entender el concepto de programacion concurrente con actores, usando la libreria `akka`.

**Objetivo del programa:** Hacer un RSS Feed Agregator, que obtenga los feeds de un url dado segun las condiciones dadas.

---

## Decisiones de diseño:

* Seguimos al pie de la letra lo que nos dijeron de "Todo es un actor", asi que para cada tarea que sea medianamente secundaria o no este directamente relacionada con nuestra accion principal, la delegamos a otro actor.

* Para parsear las fechas de cada noticia utilizamos la libreria `SimpleDateFormat` la cual es un submodulo de una libreria de Java.

* Cada actor tiene definido un objeto, el cual contiene las variables y definiciones que van a ser usadas por otros actores.

* Tenemos un severo control de errores, controlando que los tipos de los parametros sean los pedidos, y que las url sean correctas y existentes.

* Intentamos darle la maxima cantidad de trabajo a nuestros actores, con tal de que quede poco codigo a nivel de servidor, nuestro `Handler` solo se encarga de pasar las requests y controlar que lo que nos retornaron sea del tipo correcto, mientras que los actores hacen el resto del trabajo.

* Creamos un amplio sistema de mensajes para comunicarse entre actores, estos mensajes extienden de nuestros traits especificados en el archivo `CBA` (Communication Between Actors).

* Para las respuestas de actores decidimos usar `Success` y `Failure` de la libreria `akka.actor.status` mientras que para el resto de situaciones usabamos tambien `Success` y `Failure` pero de la libreria `scala.util`.

* En el caso de nuestro actor `Subscriptions`, cuando le piden que nuestro usuario se suscriba a un feed al cual ya esta suscrito, decidimos dar como mensaje `200 - OK`, dado que por consigna no se especifica que hacer en este caso, pero de todas formas el feed no se agrega si ya esta asociado a nuestro usuario.

---

## Actores

### SyncA:  
Se encarga de retornar el elemento xml que contiene la url pasada por parametro.

Lanza una excepcion en caso que no pueda ser parseada la url (No existe o no es un feed RSS).

```scala
def receive = {
  case Sync(url) => {
    SyncA.syncRequest(url) match {
      case Right(value) => sender() ! value
      case Left(value) => sender() ! Status.Failure
    } 
  }
}
```

### FeedA:
Se encarga de parsear el elemento xml dado.

Contemplamos el caso en el que tenga un parametro `since`, en ese caso acudimos al actor `FilterA` para que se encargue de filtrar todos los feeds anteriores a la fecha dada.

```scala
(filterA ? FilterA.Getfilter(feedInfo, since)).onComplete {
  case Success(response: ResponseMsg) => response match {
    case FilterA.FilterResponse(value) =>            
      handler ! Status.Success(FeedResponse(value))
    case _ => 
  } 
  case Failure(exception) => 
    handler ! Status.Failure(exception)  
  }            
}
```

Si no existe el parametro since, no se llama al actor encargado de filtrar y se retorna el feed normal.

#### Respuestas Posibles
* `FeedResponse` en caso de que todo haya salido bien
* `Failure` en caso que algo haya fallado (se levanto alguna excepcion)


### FilterA:
Encargado de filtrar los feeds anteriores a la fecha dada.  
Para hacer el parseo de la fecha utilizamos la libreria `SimpleDateFormat` y parseamos el since al formato que se pide en la consigna y el horario de Inglaterra, para luego compararlo con las fechas de publicacion de las distintas noticias.

```scala
val date: SimpleDateFormat = 
  new SimpleDateFormat("EEE, dd MMM yyyy HH:mm:ss z", ju.Locale.ENGLISH)

val parm: SimpleDateFormat = 
  new SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ss", ju.Locale.ENGLISH)

// Parse the parameter to a SimpleDateFormat
val sc = parm.parse(since)

val result: Handler.FeedInfo =
  Handler.FeedInfo(
    feedInfo.title,
    feedInfo.description,
    feedInfo.items.filter(item => date.parse(item.pubDate) //Compare Param with PubDate 
    .compareTo(sc) > 0)
  )
```

#### Respuestas posibles:
* `FilterResponse` si todo salio bien
* `Failure` si se levanto alguna excepcion


### AggregatorA:
Encargado de realizar la suscripcion al feed dado.  

Toma la base de datos como parametro para lograr persistencia, ya que si usasemos un nuevo actor `Subscriptions` cada vez que nos queremos suscribir a un feed, perderiamos las suscripciones anteriores.

```scala
class Aggregator(sb: ActorRef) extends Actor
```

```scala
def receive = {
    case Aggregator.Add(feed) => {
      sb forward Subscriptions.Insert(feed)
    }
  }
```

### SubscriptionsA:  
Este actor esta hecho para recibir 3 posibles mensajes:

* `Insert(feed,username)` asocia el feed dado con el usuario dado.

```scala
 case Subscriptions.Insert(feed, username) => {
   /*si el nombre de usuario ya esta en la base
   * de datos entonces devolvemos NotUser
   */
  db.contains(username) match{
    case true => {
      /*
      * si el usuario ya esta suscrito al feed dado, retornamos
      * FeedAdded a pesar de que no lo estamos agregando
      */
      db.get(username).get.contains(feed) match {
        case true => sender ! Status.Success(FeedAdded)
        case false => {
          //Si no esta ni el usuario ni el feed, lo agregamos
          db.put(username, db.get(username).get.:::(List(feed)))
          sender ! Status.Success(FeedAdded)
        }
      }
    }
    case false => sender ! Status.Success(NotUser)
  }
}
```

* `Register(username)` Registra un usuario en la base de datos

```scala
case Subscriptions.Register(username) => {
  /*Si el nombre de usuario ya esta en la base de datos
  * le decimos a quien nos llamo que ya estaba registrado.
  */
  db.contains(username) match {
    case true => sender ! Status.Success(Subscriptions.AlreadyRegistered)
    case false => {
      //Si no existe, lo agregamos
      db.addOne((username, List()))
      sender ! Status.Success(Subscriptions.Registered)
    }
  }
} 
```

* `Feeds(since,username)` retorna todos los feeds a los que esta suscrito un usuario (Si el parametro since existe, retorna todos los feeds posteriores a la fecha dada, a los que el usuario esta suscrito).

---
## Endpoints

Todos los endpoints estan en el objeto `FeedAggregatorServer`, este simplemente da los parametros necesarios para que el `Handler` se haga cargo de las request.

La clase `Handler` tiene una funcion determinada para cada endpoint y algunas funciones auxiliares que son necesarias para manejar mejor estos ultimos.

### GET	/feed
* `Parametros:` 
  * url: str
  * since?: datetimestr   
Respuestas posibles: 
* 200 - `{ title: str, description?: str, items: [{title: str, link: str, description?: str, pubDate: datetimestr}]`
* 400 - Bad request: can't parse url/since 
* 404 - Not found: url doesn't exist

Este endpoint toma una `url` y opcionalmente un `since`, toma la url y devuelve todos los feeds existentes en este.
En caso de que haya un parametro since, devuelve todos los feeds posteriores a la fecha dada.

Puede fallar si:
* La url es invalida (404 - Not Found)
* El parametro since es invalido (400 - Bad Request)
* La url es valida pero no es un feed RSS (400 - Bad Request)

Usa la funcion `EndpointFeed`, la cual llama a los actores `FeedA` y `SyncA`

```scala
path("feed") {
  get {
    parameters('url, 'since.?) { (url, mSince) => 
      {
        val since = mSince.getOrElse("0000-01-01T00:00:00")
        Handler.EndPointFeed(url, since)
      }
    }
  }
}
```

`getOrElse` es una funcion propia de scala que hace lo siguiente:
Toma un `Option[String]`, en caso de que este sea `None`, retorna el parametro dado, Caso contrario (`Some(value)`), retorna el value.

### POST	/subscribe

* `Parametro:` 
  * url: string
  * username : string

* `Respuestas Posibles:`
  * `200 - [{ title: str, description?: str, items: [{title: str, link: str, description?: str, pubDate: datetimestr}]` 
  * 400 - Bad request: can't parse url 
  * 404 - Not found: there are no feeds in subscription

Para este endpoint usamos la funcion de nuestra clase `Handler`, `EndpointSubscribe`, la cual llama al actor `Aggregator`

```scala
path("subscribe") {
  post {
    parameters('url,'username) { (url,username) => 
      {
        Handler.EndPointSubscribe(url,username)
      }
    }
  }
}
```

Este endpoint toma un `url`(parsea su feed con nuestros actores `SyncA` y `FeedA`) y un `username` los cuales son guardados en un diccionario, el cual se guarda en la base de datos, este diccionario simboliza todos los feeds a los que cada cliente esta suscrito.

En caso que nuestro usuario no este registrado en la base de datos, retornamos nuestro mensaje `NotUser`, para que nuestra clase `Handler` sepa de que excepcion estamos hablando especificamente.


```scala
case Subscriptions.Insert(feed, username) => {
  db.contains(username) match{
    case true => {
      .
      .
      .
    }
    case false => sender ! Status.Success(NotUser)
  }
}
```
En caso que nuestro usuario ya este suscrito a ese feed, simplemente retornamos nuestro mensaje `FeedAdded`, y si no esta suscrito, lo agregamos a nuestra base de datos y respondemos `FeedAdded`.

```scala
db.get(username).get.contains(feed) match {
  case true => sender ! Status.Success(FeedAdded)
  case false => {
    db.put(username, db.get(username).get.:::(List(feed)))
    sender ! Status.Success(FeedAdded)
  }
}
```

### GET	/feeds

* `Parametro:`
  * since?: datetimestr
  * username : str

* `Respuestas Posibles:`
  * 200 - `[{ title: str, description?: str, items: [{title: str, link: str, description?: str, pubDate: datetimestr}]`
  * 400 - Bad request: can't parse since 
  * 404 - Not found: there are no feeds in subscription

Este endpoint toma un `username` y **opcionalmente** un parametro since, por lo que nuevamente usamos la funcion `getOrElse` en nuestro `FeedAggregatorServer`

```scala
path("feeds") {
  get {
    parameters('since.?,'username) { (mSince,username) => 
      {
        val since = mSince.getOrElse("0000-01-01T00:00:00")
        Handler.EndPointFeeds(since,username)
      }
    }
  }
}
```

Nuevamente corroboramos que la url dada sea valida, en ese caso llamamos a nuestro actor `Subscriptions` para que se encargue de realizar la suscripcion, asociando en nuestro diccionario(base de datos) la clave `username` con el valor `feed`, en caso contrario, lanzamos un error `400`.

Tambien puede lanzar un error `404` en caso que no se haya podido parsear el feed.

```scala
case Success(response: ResponseMsg) => response match {
  case Subscriptions.SubsResponse(feeds) => complete(feeds)
  case Subscriptions.NotUser => complete (
    "404 - Not found: username doesn't exist"
  )
  case Subscriptions.NotFeed => complete(
    "404 - Not found: there are no feeds in subscription for " +
      s"${username}")
```

* `SubsResponse` es retornado en el caso que todo salio bien
* `NotUser` es retornado en caso que el usuario solicitado no se encuentre en la base de datos.
* `NotFeed` es retornado en el caso que el usuario dado se encuentra en la base de datos pero no se encuentra suscrito a ningun feed.

### POST /user:

* `Parametro:`
  * username: str
* `Respuestas Posibles:` 
  * 200 - Ok 
  * 409 - Conflict: username already registered

Este endpoint toma un parametro `username` y lo registra en la base de datos.
Para eso precisa de nuestro actor `Subscriptions`.

```scala
val register = (subscriptions ? Subscriptions.Register(username))
```
En caso que el nombre de usuario ya figure registrado en la base de datos, lanza un error 409.

---
## Preguntas

- Diagrama (un png/jpg que deberán subir al respositorio o bien pueden hacerlo
  en ASCII), que detallará:
    - La jerarquía de actores utilizada

      * Esquema de actores:  
        ![Imgur](https://i.imgur.com/yJg5rv0.png)
      
      #### Las flechas azules significan respuesta mientras que las negras son solicitudes.

    - Una justificación sobre la decisión de haberla hecha así
      * Con esta jerarquia de actores nos aseguramos que no haya comunicaciones innecesarias, cada actor se comunica solo con el actor o modulo necesario, dejando el resto de trabajo para nuestro `Handler`.
      
    - Detallar cómo se ajustarían los puntos estrella a su jerarquía (creando
      otros diagramas si fuese necesario). Esto deberán hacerlo
      independientemente de si implementan o no los puntos estrella.  

      * En el punto 3 no tuvimos que hacer modificaciones a nuestra jerarquia, ya que pudimos incorporar de forma prolija las funcionalidades para servir a los respectivos endpoints.

      * En el caso del punto 4 pensamos en ampliar nuestra jerarquia con un nuevo actor que se comunique tanto con el `Handler` como con el `FeedA` que determine si el url dado es de protocolo `Atom` o `RSS` y en base a eso determinar como parsear el feed.

- Protocolo de mensajes entre actores:
    - Describan el protocolo de pasaje de mensajes haciendo referencia entre
      qué actores se pasarían que mensajes.
    - Deben considerar también el caso extendido con los puntos estrella 3 y 4.  
      * Definimos 2 "tipos" de mensajes, `RequestMsg` y `ResponseMsg`, para diferenciar entre si los mensajes que debia recibir un actor y los que debia enviar, Por ejemplo `Subscriptions` recibia el mensaje `Register` pero podia responder con `NotFeed`,`NotUser` o `Registered`
      
- ¿Cómo hicieron para comunicarse con el "mundo exterior"? (i.e. el servidor de
  la API REST).

  * Nuestro servidor usaba una funcion `bindAndHandle` la cual seteaba un puerto y una direccion y le permite escuchar solicitudes entrantes.
  En una segunda instancia, nos conectamos con el mundo exterior a la hora de parsear los feeds de una cierta url, para esto usamos la funcion `SyncRequest` que fue dada por la catedra.

- ¿Qué són los *Futures*?, ¿Para qué fueron utilizados en su implementación?

  * `Future` es un tipo el cual nos permite realizar tareas en paralelo de una forma no bloqueante.
  Podriamos considerar que un Future es un espacio donde va un valor que todavia no existe, Por lo que se pueden hacer otras tareas mientras este es calculado.

  * Nosotros decidimos usarlos en varios casos, ya que hacer una pregunta a un actor, su respuesta es un valor future al fin y al cabo.

  ```scala
  //Valor a futuro
  val xmlelem : Future[Any] = sync ? SyncA.Sync(url)
  //que voy a hacer una vez que tenga ese valor
  onComplete(xmlelem) {
  ```

- ¿Qué problemas traería el implementar este sistema de manera síncrona?

  * Implementar este sistema de manera sincrona nos traeria consigo un mayor tiempo de ejecucion, ya que de manera asincrona, nuestro programa sigue corriendo mientras nuestros actores realizan su tarea, y de manera sincrona deberian esperar a que cada tarea finalize para realizar la siguiente.

- ¿Qué les asegura el sistema de pasaje de mensajes y cómo se diferencia con un
  semáforo/mutex?

  * El sistema de actores nos asegura que nuestras acciones y peticiones son no bloqueantes, por lo que no es necesario esperar a que una accion termine para realizar otra, esa es nuestra diferencia con un semaforo, ya que si el semaforo esta en rojo, se esta bloqueando una salida hasta que este se ponga en verde.