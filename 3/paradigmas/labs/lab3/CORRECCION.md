# Grupo 01		
## Corrección		
	Tag o commit corregido:	"1.0"
		
### Entrega y git		25.00%
	Commits de cada integrante	100.00%
	Commits frecuentes y con nombres significativos	0.00%
	En tiempo y con tag correcto	0.00%
### Informe		32.50%
	Informe escrito en Markdown, correctamente formateado, aprovechando las facilidades del lenguaje (numeración, itemización, código, títulos).	100.00%
	Diagrama de jerarquía básico completo (jpeg, ascii, etc.),	0.00%
	Justificación de la jerarquía utilizada (redacción coherente).	100.00%
	Extensión sobre la jerarquía básica detallando como agregar los puntos estrella (indistinto a si se realizaron los puntos estrella o no).	0.00%
	Protocolo de mensajes detallado correctamente, y denotando que mensajes ocurren entre que actores.	50.00%
	Protocolo de mensajes extendido para el caso de puntos estrella (independientemente de si lo realizaron o no).	0.00%
	Pregunta: ¿Cómo hicieron para comunicarse con el mundo exterior? (i.e. el servidor REST-API)	0.00%
	Pregunta: ¿Qué son los Futures?, ¿Para qué fueron utilizados en su implementación?	0.00%
	Pregunta: ¿Qué problemas traería el implementar este sistema de manera síncrona?	0.00%
	Pregunta: ¿Qué les asegura el sistema de pasaje de mensajes y cómo se diferencia con un semáforo/mutex?	0.00%
### Funcionalidad		26.25%
	Parte 1: `GET /feed` devuelve 200 y el feed con todo lo indicado en caso de url correcta.	50.00%
	Parte 1: `GET /feed` filtra los items de acuerdo al campo opcional `since`.	50.00%
	Parte 1: `GET /feed` devuelve 400 si la url es incorrecta (no se puede parsear la url o el recurso no es XML) o si el `since` no es correcto.	50.00%
	Parte 1: `GET /feed` devuelve 404 si la url es correcta pero no existe el recurso.	50.00%
	Parte 2: `POST /subscribe` devuelve 200 y OK si se le pasa una URL correcta, el RSS que se pasa es guardado en el servidor de alguna manera.	25.00%
	Parte 2: `POST /subscribe` devuelve 400 si hay algún error con la URL (no se puede parsear o el recurso no es XML)	0.00%
	Parte 2: `POST /subscribe` devuelve 404 si la url es correcta pero el recurso no existe.	50.00%
	Parte 2: `GET /feeds` devuelve 200 y la lista de feeds subscriptos con todo lo indicado por la tabla API.	50.00%
	Parte 2: `GET /feeds` filtra los items (en cada feed) de acuerdo al campo opcional `since`.	0.00%
	Parte 2: `GET /feeds` devuelve 400 si no se puede parsear `since`.	0.00%
	Parte 2: `GET /feeds` devuelve 404 si no hay feeds disponibles.	0.00%
### Modularización y diseño		57.50%
	Los actores se trabajan en archivos separados.	100.00%
	Actores viven en un `package` distinto al del servidor Akka HTTP.	0.00%
	Los protocolos se escriben en el Object Companion de cada actor. Están trabajado utilizando `case class` o `case object` para facilitar el *pattern matching*.	100.00%
	El protocolo de mensajes se hace con herencia entre mensajes de la misma índole (i.e. mensajes del tipo `Request` heredan de un mismo `trait` y lo mismo con mensajes del tipo `Response`).	0.00%
	Hacen *pattern matching* para lidiar con pasaje de mensajes entre actores.	100.00%
	Respetan la jerarquía y el protocolo dado en el informe, no comunican cosas que no tiene sentido que se comuniquen.	100.00%
	No se utiliza `Await` ni ningún otro tipo de instrucción bloqueante salvo justificación expresa en el Informe.	0.00%
	No hacen uso de contrucciones de lenguaje imperativo (e.g. `for`, `forEach`).	100.00%
	Los `var` son usados sólo en el caso de que no hay otra opción (i.e. en caso de colecciones mutable, utilizan la estructura de datos adecuada pero sobre un `val`, no están constatemente sobreescribiendo).	0.00%
	Hacen uso de construcciones de lenguaje funcional (`map`, `filter` y eventualmente `fold`/`reduce` y `flatMap`). Evitan el uso de `flatten`.	100.00%
	No se utiliza la sentencia `try { ... } catch { ... }` y se resuelven las excepciones mediante el uso de la mónada `scala.util.Try` y *pattern matching*.	0.00%
	Las excepciones capturadas mediante *pattern matching* en un `receive` son avisadas a los actores padres de la jerarquía mediante el uso del protocolo de mensajes y no devueltas simplemente como un error.	100.00%
	Cualquier librería "extra" fue correctamente agregada a `build.sbt`/ `project/build.properties` y los plugins están en `project/plugins.sbt`.	100.00%
### Calidad de código		60.00%
	Ninguna línea sobrepasa los 100 caracteres, y sólo unas pocas los 80.	100.00%
	El código es legible, está correctamente indentado (con espacios, no tabs), y con espacios uniformes (i.e. no hay más de dos saltos de línea en ningún lado).	100.00%
	Hacen uso comprensivo de comentarios y documentan partes que sean complejas.	0.00%
	No hay código spaghetti, no utilizan variables *flags* (i.e. variables que guarden valores temporales para ser utilizadas más adelante como un booleano).	0.00%
	Evitan el uso de `return` en las funciones.	100.00%
### Opcionales		0.00%
	Punto estrella 1 (Parte 3): Está en el tag correcto, no en master.	0.00%
	Punto estrella 1 (Parte 3): Implementan la API correctamente, con los códigos como se establecen en la tabla.	0.00%
		
	Punto estrella 2 (Parte 4): Está en el tag correcto, no en master.	0.00%
	Punto estrella 2 (Parte 4): Los actores para parsing se desprende del actor encargado de hacer *request* a la url. Heredan de la misma clase base.	0.00%
		
# Nota Final		2.773333333
		
		
# Comentarios		
- 18 minutos de retraso en la entrega.		
- a los commits les faltan mensajes relevantes. "error", "bug fixed"(which one?), "Parte 2 95%".		
- no debieran haber tenido problemas con el `onComplete`, tal vez hay otro problema?		
- Meter las rutas dentro de un actor es invertir el diseño: el server es el FeedAgregatorServlet, que utiliza los actores. No al revés.		
- Por momentos usan `var` en lugares que `val` tendría mas sentido.		
- Usar un flag como un entero es feo y no describe nada. Mejor usar un booleano y un nombre mas adecuado (igual, no es nada comparado con su mera existencia).		
- No termino de entender por qué es que hay que ejecutarlo dos veces, si igual están esperando la respuesta.		
- Los case object y classes no heredan de un trait o algo que los distinga.		
- Hay un print de debugging que quedó.		
- Falta descripción mas clara de los mensajes.		
- El since con "0" como default no es ideal, debería ser un Option		
- El código no tiene casi comentarios.		
- El post debería recibir la info como dato, no por url		
- subscribe retorna 404 con una url incorrecta (debía ser 400).		
- feeds con since retorne siempre vacío.		
- feeds no retorna error nunca.		
