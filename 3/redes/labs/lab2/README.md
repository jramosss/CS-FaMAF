
> Aimar, Osiecki, Ramos

> Redes y sistemas distribuidos 2021, FaMAF, Universidad Nacional de Córdoba

# Informe Laboratorio 2

## Aplicación del servidor

### Objetivos :

   1. Aplicar la comunicación cliente/servidor por medio de la programación de sockets,desde la perspectiva del servidor.

   2. Familiarizarse con un protocolo de aplicación diseñado en casa.

   3. Comprender, diseñar e implementar un programa servidor de archivos en Python.


En este laboratorio trabajamos con HFTP, que es un protocolo de capa de aplicación que usa TCP como protocolo de transporte. TCP garantiza una entrega segura, libre de errores y en orden de todas las transacciones hechas con HFTP. Un servidor de HFTP escucha pedidos en el puerto TCP 19500.

## Server

Para iniciar el servidor, lo primero que hicimos fue crear un socket, al que le asignamos el puerto y el directorio. Luego en *serve* se acepta la conexión con el cliente: creamos una nueva *connection* y derivamos a *handle*, que se encarga de manejar la conexión.

## Connection

Ésta clase es la encargada de manejar la conexión con el cliente. Aqui, *Handle*  recibe e interpreta los datos que manda el cliente, y llama a las funciones correspondientes para que realicen el pedido. Estas cuatro funciones son de carácter privado dentro de la clase, y corresponden a cada uno de los cuatro comandos disponibles para este servidor:

  1. __list_files: Se llama cuando el cliente envía el comando "get_file_listing". Envía una secuencia de lineas terminadas en \r\n, cada una con uno de los archivos que se encuentran actualmente disponibles en el directorio del servidor.

  2. __send_metadata: Este comando recibe un argumento que especifica el nombre del archivo del cual se pretende averiguar el tamaño. El servidor responde con una cadena indicando su valor en bytes.

  3. __send_slice: Este comando recibe como argumento el nombre del archivo del que se pretende obtener un slice. Este slice se especifica con un offset (byte de inicio) y un size (tamaño de la parte esperada, en bytes), ambos no negativos. El servidor responde con el fragmento del archivo pedido codificado en base64 y terminado en \r\n.

  4. __quit_conn:  Este comando no recibe argumentos y busca terminar la conexión. El servidor responde con un resultado exitoso (0 OK) y luego cierra la conexión.


## Handle:
Se encarga de manejar la conexión hasta que ésta termina. Primero, recibe la petición del cliente. De no haber un EOL en este, no hace nada y vuelve a seguir recibiendo pedidos, hasta que encuentre un EOL. Luego, obtiene el primer comando de la cadena y lo decola. Finalmente, se llama a la funcion *__exec_comm*, que recibe el comando y llama a una de las cuatro funciones declaradas previamente.

## ¿Cómo atender múltiples clientes simultáneamente?

Mediante el uso de threads es posible atender a multiples cliente, creando un hilo por cada conexión al servidor, de esta forma el procesamiento de pedidos de clientes es independiente de los demás.

## Diferencia entre IP "127.0.0.1" e IP "0.0.0.0"

127.0.0.1 es la dirección del protocolo de Internet de bucle (IP) también conocida como localhost . La dirección se usa para establecer una conexión IP a la misma máquina o computadora que usa el usuario final; es una dirección designada para proveer una interfaz IP funcional y completa dentro de tu misma computadora, sin importar cual es la configuración de la red exterior. Todo el tráfico que se envía a 127.0.0.1 es inmediatamente recibido.
La dirección "0.0.0.0" es una sintaxis de dirección válida. Por lo tanto, debería analizarse como válido donde se espera una dirección IP en la notación decimal con puntos tradicionales. Una vez analizado y convertido a una forma numérica viable, su valor determina lo que sucede a continuación.
En el contexto del enrutamiento, 0.0.0.0 generalmente significa la ruta predeterminada, es decir, la ruta que conduce al "resto de" Internet en lugar de a alguna parte en la red local.
En el contexto de una entrada de ruta, generalmente significa la ruta predeterminada. Eso sucede como resultado más de la máscara de dirección, que selecciona los bits para comparar. Una máscara de 0.0.0.0 no selecciona ningún bit, por lo que la comparación siempre tendrá éxito. Entonces, cuando se configura una ruta así, siempre hay un lugar para que los paquetes se lleven (si está configurado con un destino válido).

## Para correr:
* $ python3 server.py  
* (en otra consola) $ telnet 0.0.0.0 19500  