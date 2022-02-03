Paradigmas de Programación @ FAMAFyC - 2020
===========================================

Laboratorio de Programación Concurrente
---------------------------------------

### Correcciones en base a los comentarios dados en la correcion y los puntos destacados en cada seccion (Funcionalidad, Modularizacion y diseño, Calidad de codigo)

#### 1er commit:

*	Modularizacion: separe a los actores del servidor.
*	Delegacion: como teniamos el servidor sobrecargado con tareas, el actor Handler, paso a ser un Objeto al cual el servidor le delega tareas. En el puse los 3 distintos metodos que puede realizar; uno para cada endpoint. El Handler se encarga de dividir las tareas a el resto de los actores, dependiendo el endpoint.

Anotaciones: Ya que nos dividimos las tareas, les deje las "carcazas" de sus actores para que lo completen con el codigo anterior. [Jero completo]

#### 2do commit [Jero]:

* Se adapto el actor `FeedA` y sus subordinados para que funcionen con el nuevo manejo del Handler. Ademas delegamos algunas pequeñas tareas para que sean realizados por dos nuevos actores `DataChecker` y `SyncA`.
* Se acomodaron los `var/val` mal usados en el codigo anterior.

#### 3er commit [Juli]:
* Se adapto el actor `Subscriptions` al nuevo modelo de Handler.
* Cambie de lugar el objeto `Insert` ya que parecia ser propio de `Subscriptions` mas que de `Aggregator`.
* Elimine librerias que no hacian falta.
* Cambie los parametros de `Aggregator.Add` y `Subscriptions.Feeds` para que tomen solamente url y no al actor, ya que este fue cambiado de lugar y ahora se encuentra en `Handler`.


### 8vo commit [Mariano]:
* Arregle el `case Subscriptions.Feeds(since, username)` printeaba lista vacia cuando el usuario tenia feeds.
* En el `case Subscriptions.Insert(feed, username)` resumi el codigo de 5/6 lineas que habia a uno de 2 lineas un poco mas funcional.
* Agregue el `endpoint feeds` con el campo de username.
* Termine de conectar lo que habian pusheado, dejando todo funcionando. 

### 9no commit [Jeronimo]:
* Testee todos los endpoints con punto estrella 3.
* Solucione error donde usuario podia subscribirse 2 veces al mismo feed y este se duplicaba en la base de datos, ahora no se duplica, no tira error.
* Cambie caso en endpoint user, donde tiraba `"400 - Conflict: username already registered"` cuando el problema no era ese, simplemente es un caso para rellenar, ahora tira 
`"400 - Conflict: username already registered"`.
* Codigo comentado en partes principales.
* Corregi lineas que superaban los 80 caracteres

### 10mo commit [Jeronimo]:

* Implementacion de los traits RequestMsg y ResponseMsg. Falta testear.

### 11vo commit [Julian]:
* Arregle el error que tenia el endpoint feeds (Estaba mal el matching de mensajes)
* Testee todos los endpoints con la nueva implementacion de sistema de mensajes [Todos pasados](https://paste.mod.gg/ratasasiya.md)
* Cambie los nombres de variables a nombres mas significantes e ilustrativos
* Corregi un poco el estilo de codigo para hacerlo mas funcional, borrando recursos imperativos
* Elimine librerias que sobraban

### 12vo commit [Julian]:
* Cambios en el FeedA, ahora si el parametro since no existe, directamente no se filtra.
* Informe terminado