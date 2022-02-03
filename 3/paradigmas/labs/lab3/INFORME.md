> Bernardi Paludi Ramos

> Universidad Nacional de Córdoba  Facultad de Matemática, Astronomía, Física y Computación  Paradigmas de Programación 2020

# Informe Laboratorio 3

El **objetivo de este laboratorio** es entender el concepto de programacion concurrente con actores, usando la libreria `akka`.

**Objetivo del programa:** Hacer un RSS Feed Agregator, que obtenga los feeds de un url dado segun las condiciones dadas.

**Observacion muy importante**: Para que se ejecuten correctamente los endpoints, hay que enviarles la request 2 veces. Esto se debe a que se ejecuta el complete antes de que el actor reciba la respuesta.
Intentamos solucionarlo usando locks, pero no funcionaba, tanto ciclos como banderas. 
Tambien intentamos correrlo dentro de bloques `onComplete` pero la libreria no nos dejaba ya que tomaba como que podia no retornar un future por mas que hicieramos 
```scala
case Success(value) => complete(value)
case Failure(value) => complete("400 - Bad request\n")
```

**Tambien cabe aclarar**: No llegamos a terminar del todo el proyecto por falta de tiempo, no por falta de conocimiento, por lo que posterior a la fecha de entrega van a seguir llegando commits por que estamos interesados en cumplir todos los objetivos.

## Decisiones de diseño:

* Seguimos al pie de la letra lo que nos dijeron de "Todo es un actor", asi que para cada tarea que sea medianamente secundaria o no este directamente relacionada con nuestra accion principal, la delegamos a otro actor.

* Para parsear las fechas de cada noticia utilizamos la libreria `SimpleDateFormat` la cual es un submodulo de una libreria de Java.

* Cada actor tiene definido un objeto, el cual contiene las variables y definiciones que van a ser usadas por otros actores.

* Tenemos un severo control de errores, controlando que los tipos de los parametros sean los pedidos, y que las url sean correctas y existentes.

* Intentamos abstraernos lo maximo posible, dejando las capas de nivel alto con apenas unas lineas de codigo, al punto de que el objeto `FeedAgregatorServer` Solo declara unas variables y llama a un actor: 

```scala
def main(args: Array[String]) {
  implicit val system = ActorSystem()
  implicit val executionContext = system.dispatcher

  val handler = system.actorOf(Props[Handler], "Handler")

  handler ! Handler.Handle
}
```

* Tuvimos que acudir al uso de locks ya que en algunos casos la concurrencia nos jugaba en contra y los actores retornaban un mensaje antes de obtenerlo a pesar de que usabamos `Future.onComplete`.

* Creamos un sistema propio de mensajes `OK` y `BAD` para representar si una request fue concluyo exitosamente o si fallo en su tarea.

---

## Actores

### HandlerA:  
Este actor es el "cabeza" o jefe, es el encargado de llamar a todos los otros actores dependiendo la request que hagamos.
* Es llamado por `FeedAggregatorServer`  
* En caso de que nos hagan una request `GET /feed`, Este llama al actor `FeedA`
* Llama a `AggregatorA` en el endpoint `suscribe`
* Llama a `SubscriptionsA` en el endpoint `feeds`


### FeedA:
Se encarga de parsear la url dada y devolver el feed que esta contiene.

```scala
case FeedA.Parsear(url,since) => {

      FeedA.syncRequest(url) match{
          .
          .
          .
```
y contemplamos el caso en el que tenga un parametro `since`, en ese caso acudimos al actor `FilterA` para que se encargue de filtrar todos los feeds anteriores a la fecha dada.

```scala
since match{
  case "0" => handlerA ! feedInfo 
  case _ => (filterA ? FilterA.Getfilter(feedInfo,since)).onComplete{
      .
      .
      .
```

* Es llamado por `HandlerA`
* Es llamado por `AggregatorA`  

### FilterA:
Encargado de filtrar los feeds anteriores a la fecha dada.  
Para hacer el parseo de la fecha utilizamos la libreria `SimpleDateFormat` y parseamos el since al formato que se pide en la consigna y el horario de Inglaterra, para luego compararlo con las fechas de publicacion de las distintas noticias.

```scala
val date = new SimpleDateFormat("EEE, dd MMM yyyy HH:mm:ss z", ju.Locale.ENGLISH)
val result=
  FeedAggregatorServer.FeedInfo(
    feedInfo.title,
    feedInfo.description,
    feedInfo.items.filter(item => date.parse(item.pubDate)
      .compareTo(sc) > 0)
  )
```

* Es llamado por `FeedA`

### AggregatorA:
Encargado de realizar la suscripcion al url dado.
Primero toma la url, llama a `FeedA` para que le de el feed correspondiente:
```scala
val feedres = (feedA ? FeedA.Parsear(url)).flatMap(res =>
//Parsear toma since = "0" por defecto
```
Luego de checkear que su tipo sea correcto (`FeedInfo`), le pedimos a nuestro actor `SubscriptionsA` que lo agregue a la base de datos:
```scala
res match {
  case res : FeedInfo => {
    (subsciptionsA ? Aggregator.Insert(res))
  }
})
```

* Es llamado por `HandlerA`
* Llama a `SubscriptionsA`

### SubscriptionsA:  
Se encarga de guardar las suscripciones en la base de datos.
```scala
var db : List[FeedInfo] = List()
case Aggregator.Insert(feed) => {
  db = db.::: (List(feed))
  sender() ! Done
}
```

Tambien a su vez es el encargado de mostrar los feeds que existen en la base de datos:
```scala
case Handler.Feeds => {
      sender() ! db
}
```

* Lo llama `AggregatorA`
* Tambien lo llama el endpoint `/feeds`

---
## Endpoints

### GET	/feed
Parametros: `{url: str, since?: datetimestr}`  
Respuestas posibles: 
* 200 - `{ title: str, description?: str, items: [{title: str, link: str, description?: str, pubDate: datetimestr}]`
* 400 - Bad request: can't parse url/since 
* 404 - Not found: url doesn't exist

Este endpoint toma una `url` y opcionalmente un `since`, toma la url y devuelve todos los feeds existentes en este.
En caso de que haya un parametro since, devuelve todos los feeds posteriores a la fecha dada.

Puede fallar si:
* La url es invalida (404 - Not Found)
* El parametro since es invalido (400 - Bad Request)

Llama al actor `FeedA`

```scala
path("feed") {
  get {
    parameter("url".as[String]) { url => 
      parameter("since" ? "0") {since =>
      /*Esto significa que si since no existe en la url, se le asigna 0, ya que es un parametro opcional*/
        try {
          (feedA ? FeedA.Parsear(url,since))
```

### POST	/subscribe

Parametro: `{url: str}`
Respuestas Posibles: 
* `200 - [{ title: str, description?: str, items: [{title: str, link: str, description?: str, pubDate: datetimestr}]` 
* 400 - Bad request: can't parse url 
* 404 - Not found: there are no feeds in subscription

Este endpoint toma un `url` y lo guarda en una lista, la cual se guarda en la base de datos, esta Lista simboliza todos los feeds a los que un cliente esta suscrito.

Puede fallar si:
* La url es invalida (400 - Bad Request)
* La url no existe (404 - Not Found)

Llama al actor `Aggregator`

```scala
path("subscribe") {
  post {
    parameter("url".as[String]) { url =>
      try {
        (aggregator ? Handler.Add(url, subscriptions))
```

### GET	/feeds

Parametro: `{since?: datetimestr}`
Respuestas Posibles:
* 200 - `[{ title: str, description?: str, items: [{title: str, link: str, description?: str, pubDate: datetimestr}]`
* 400 - Bad request: can't parse since 
* 404 - Not found: there are no feeds in subscription

Este endpoint toma **opcionalmente** un parametro since:

* Si since es 0(No existe):  
Devuelvo todos los feeds a los que el cliente este suscrito:
```scala
case Handler.Feeds(since) => since match {
  case "0" => sender() ! db
```

* Si since es distinto de 0 (es decir que existe el parametro):  
Devuelvo todos los feeds posteriores a la fecha dada a los que el cliente este suscrito:
```scala
var newdb = List[FeedInfo] = List()
case _ => {
  var original_sender = sender()
  /*Guardamos quien nos envio el mensaje, ya que al llamar a FilterA, nuestro nuevo sender() va a ser SubscriptionsA en vez de Handler*/
  db.foreach(fi => (filterA ? FilterA.Getfilter(fi,since)).onComplete{
    case Success(value) => newdb ::: List(fi)
    case Failure(e) => 
  })
  original_sender ! newdb
}
```

Llama al actor `SubscriptionsA`  
Llama al actor `FilterA`