> Aimar, Osiecki, Ramos  
> Redes y sistemas distribuidos 2021, FaMAF UNC

# <center>Informe Laboratorio 3 - Transporte<center>

## Abstract 
En este trabajo se proponen soluciones a problemas que afectan a la capa de transporte, sobre simulaciones de modelos de red. Para esto, se debieron generar dichos modelos en la plataforma Omnet++, sobre una maquina virtual Lubuntu. El trabajo consiste en una primera parte de análisis, donde estudiamos el trafico de red bajo tasas de datos acotadas y tamaño de buffers limitados; y una segunda en la que diseñamos e implementamos soluciones para las problemáticas antes mencionadas.

Para estudiar los comportamientos de la red, fue necesario tomar metricas sobre los buffers y su estado a lo largo del tiempo, para poder ver si hay congestion y como se comporta cada componente.
Los resultados obtenidos fueron los esperados.

## Introducción

Estudiaremos los problemas de control de congestión y control de flujo. 

El primero se encarga del control de tráfico de paquetes en la red. Su objetivo es evitar que los nodos intermedios entre hosts colapsen y se pierdan paquetes de datos.
El segundo se encarga de la tasa de transferencia de datos entre dos hosts, con el objetivo de prevenir que uno de transmisión rápida exceda a otro de transmisión lenta, produciendo así perdida de paquetes.

El objetivo de este trabajo es desarrollar una solución a la perdida de paquetes que se da debido a estos dos problemas. Abordamos este problema mediante la programación dirigida por eventos, utilizando el simulador Omnet++.

Las simulaciones se configuran mediante distintos parametros, como tiempo de simulación, tasa de generación de paquetes, capacidad de los buffers, etc. El sistema tiene estados, que cambia segun los eventos que sucedan durante la simulación.

Como primer tarea analizamos un modelo de red simple, que cuenta con un nodo emisor, una buffer intermedio y un nodo receptor. Cada nodo tiene su forma de manejar un evento, el cual será un envío o recibida de paquetes.
Como segunda tarea, y en base a la primera, planteamos una solución para evitar la perdida de paquetes que se origina debido a la diferencia entre la carga ofrecida (paquetes enviados por el nodo generador) y la carga util (paquetes que el nodo receptor puede procesar).

## *Tarea Analisis:*

Analizamos el impacto de la tasa de transmisión y tamaño de buffers en el tráfico de red, bajo dos casos de estudio. Ambos se aplican a la misma red, pero con distintos parámetros. 

El modelo de red cuenta de 3 partes: un nodo transmisor nodeTx, que cuenta con un modulo generador de tipo Generator y un buffer de tipo Queue; un buffer intermedio de tipo Queue, y un nodo receptor nodeRx, que cuenta con un buffer de tipo Queue y un modulo "deposito", de tipo Sink. 

**Caso 1**

 - NodeTx​ a ​Queue​: ​datarate = 1 Mbps y delay = 100us
 -  Queue​ a ​NodeRx​: ​datarate = 1 Mbps y delay = 100us
 -  Queue​ a ​Sink​: ​datarate = 0.5 Mbps

**Caso 2**

- NodeTx​ a ​Queue​: ​datarate = 1 Mbps y delay = 100us
-  Queue​ a ​NodeRx​: ​datarate = 0.5 Mbps y delay = 100us
-  Queue​ a ​Sink​: ​datarate = 1 Mbps

### *Expectativas*:

 En el Caso 1, no deberia haber problemas desde NodeTx a Queue, ni de Queue a NodeRx, dado que su tasa de transmisión y delay son los mismos. El problema se encuentra entre el buffer de NodeRx y el modulo Sink, ya que la tasa de trasmisión entre estos es la mitad de la que hay entre Queue y NodeRx. Luego, esperamos que el buffer de NodeRx colapse.
 
 En el Caso 2, la tasa de transmisión de Queue a NodeRx es menor a la tasa de transmision de NodeTx a Queue, por lo tanto esperamos que el buffer de Queue colapse y pierda paquetes ya que tarda mas en enviarlos al NodeRx que lo que esta recibiendo de NodeTx.

### *Resultados*:

Los resultados muestran que nuestras expectativas fueron ciertas para el intervalo de generación  del modulo Generator *genInterval=exponential(0.1)*: para el Caso 1, el buffer de NodeRx colapsó tras poco menos de _t=50s_; y en el Caso 2, el buffer de Queue colapsó tras haber pasado el mismo tiempo aproximadamente.
Sin embargo, para intervalos de generación más grandes (i.e *genInterval= exponential(0.3), exponential(0.5), etc*), los buffers afectados en cada uno de los casos de estudio se ven menos congestionados a medida que aumentamos dichos intervalos. 


## <center> Caso 1 


Aqui el buffer de NodeRx crece linealmente hasta llegar a su maxima capacidad y se estanca. 

![alt](plots/buffers/v1-genint1.png "Caso 1 con genInterval = exponential(0.1)") 


Pero al aumentar el intervalo de generacion levemente, la congestion desaparece:
Notar que aunque el NodeRx sea el que alcanza valores de ocupación más altos, el maximo es de 30 paquetes en buffer (bajo, considerando que el tamaño del buffer es de 200 paquetes).

![alt](plots/buffers/v1-genint2.png "Caso 1 con genInterval exponential(0.2)")

Este efecto crece abruptamente, logrando que con un intervalo de generacion apenas mayor el buffer nunca contenga mas de 5 paquetes.

![alt](plots/buffers/v1-genint3.png "Caso 1 con genInterval exponential(0.3)")

Finalmente, para *genInterval=exponential(1.0)*, los tres buffers permanecen constantes en 1, con un pico ocasional del buffer de NodeTx de 2 paquetes.

![alt](plots/buffers/v1-genint10.png "Caso 1 con genInterval exponential(1.0)")


## <center> Caso 2


En el Caso 2 vemos como sucede lo mismo, pero esta vez con el buffer de Network (Queue), aquí llamado Nx:

![alt](plots/buffers/v2-genint1.png "Caso 2 con genInterval exponential(0.1)")

![alt](plots/buffers/v2-genint2.png "Caso 2 con genInterval exponential(0.2)")

![alt](plots/buffers/v2-genint3.png "Caso 2 con genInterval exponential(0.3)")

![alt](plots/buffers/v2-genint10.png "Caso 2 con genInterval exponential(1.0)")


### *Conclusiones*

Para ambos casos de estudio, un intervalo de generación pequeño menor a exponential(0.2) es el factor determinante para que el buffer en cuestión de cada caso se colapse, pudiendo observar como se forman "cuellos de botella" debido a las diferencias de tasas de transmision entre la entrada y la salida de dicho buffer.

La fuente limitante en el Caso 1 es el NodeRx debido a que la tasa de transferencia desde el buffer Queue de la red al buffer de NodeRx es de 1 Mbps, el doble que la tasa de transferencia de NodeRx al Sink.
Aquí observamos un cuello de botella, y este genera un problema de control de flujo: una red rapida alimenta a un receptor con capacidad pequeña.

En el Caso 2, la fuente limitante es el buffer del Network por razones similares: el buffer recibe de NodeTx a 1Mbps, y le envía al buffer de NodeRx a 0,5Mbps. Aqui esta el cuello de botella , que genera un problema de congestión: se transmiten paquetes a una tasa que la red no puede manejar, y por ende se pierden paquetes que nunca llegarán al receptor (cuya capacidad seria suficiente para recibir todos los paquetes).

 - **Diferencia entre Control de Flujo y Control de Congestión**


 Control de Congestión se encarga del control de tráfico en la red. Su objetivo es evitar que nodos intermedios entre hosts colapsen. Esto sucede en el Caso 2 con el buffer del Network.

 Control de Flujo se encarga de la tasa de transferencia de datos entre dos hosts con el objetivo de prevenir que un host de transmisión rápida exceda a un host de transmisión lenta, produciendoperdida de paquetes. Esto sucede en el Caso 1, en NodeRx.
 

## *Tarea Diseño:*

Tras haber analizado la influencia de los parametros sobre la red, debemos diseñar un sistema de control de flujo y congestión para evitar la pérdida de datos por saturación de buffers

 **Algoritmo de control de flujo y congestión**


Implementamos el algoritmo de parada y espera, donde NodeTx manda un paquete y no manda otro hasta que recibe un feedback de parte de NodeRx, confirmando la llegada del último paquete enviado. 

Para esto, añadimos los siguientes campos a las clases *TransportTx*:

### *TransportTx*
```cpp
bool       shouldSend;
int        sinkRemainingBuffer;
simtime_t  lastSentMsgTs;
```

El algoritmo funciona de la siguiente manera:

Cada vez que TransportTx envia un paquete, fija el valor de *shouldSend* en false, indicando que el paquete de feedback de TransportRx todavia no llego. Cuando el paquete llega al receptor, éste crea un paquete feedback y le guarda información que el emisor necesita saber: el tamaño de ocupación del buffer y el tipo de paquete (uno de tipo 2 corresponde a un feedback, uno de tipo 0 corresponde a un paquete de datos),  y luego lo envia a TransportTx.

Cuando un paquete llega a TransportTx, este se fija de que tipo es: si es de tipo 0, el paquete viene del generador, y es encolado al buffer para ser enviado a TransportRx. Si es de tipo 2, actualiza *sinkRemainingBuffer* al valor que indica el paquete, y solo actualiza *shouldSend* a true si *sinkRemainingBuffer* es mayor que cero (es decir, hay espacio disponible en el buffer receptor) y si la identificación del paquete feedback coincide con el último paquete enviado (*lastSentMsgTs*). Así, si todo está en orden para enviar un nuevo paquete, se quita del buffer, se le da un nombre y se envía al receptor. Ademas, implementanos un TimeOut de la siguiente manera: si el buffer de TransportTx llega a encolar 10 paquetes sin recibir un Feedback de confirmacion de recepcion, automaticamente intenta enviar un paquete para asi evitar el estancamiento. 


### *Expectativas de los casos de estudio*:

Al ser parada y espera, supusimos que el buffer en TransportRx no tendria problemas puesto que el algoritmo asegura que un paquete nunca se envia antes de haber recibido la confirmación del anterior. De igual manera no se veria afectado el buffer del Network. Esto resolvería el problema de control de flujo, pues las perdidas originadas por las diferencias entre tasas de transmisión en las entradas y salidas de un nodo ya no serian problema gracias a que solo se estaría transmitiendo de a un paquete, desde TransportTx a TransportRx y viceversa.

Asimismo observamos que esto tambien resolvería el problema de control de congestión: los buffers de Network y de TransportRx nunca colapsarian debido a que nunca se almacenaría mas de un paquete. El buffer de TransportTx tiene un tamaño mayor al total de paquetes generados durante las simulaciones (con distintos *genInterval*), por lo que asumimos que tampoco colapsaria. 


### *Resultados*

Sin embargo, la implementacion no es funcional, por eso sigue sucediendo la perdida de paquetes.

Para este estudio tomamos como medida El tiempo en el que se envian los paquetes desde TransportTx y el tiempo en el que se reciben los mensajes en TransportRx.
## <center> Caso 1 

Cuando el intervalo de generacion es muy pequeno, los paquetes se envian y se reciben casi al mismo tiempo, 

![alt](plots/buffersParte2/v12-genint01.png "Caso 1 con genInterval exponential(0.1)")

Pero a medida que va creciendo el intervalo de generacion, el tiempo que pasa desde que un paquete se envia hasta que se recibe, aumenta.

![alt](plots/buffersParte2/v12-genint03.png "Caso 1 con genInterval exponential(0.3)")

Esto puede observarse en la creciente rugosidad de la recta a medida que aumenta el intervalo de generacion.

![alt](plots/buffersParte2/v12-genint05.png "Caso 1 con genInterval exponential(0.5)")

![alt](plots/buffersParte2/v12-genint10.png "Caso 1 con genInterval exponential(1.0)")

## <center> Caso 2
No se notan diferencias con el caso anterior

![alt](plots/buffersParte2/v22-genint01.png "Caso 2 con genInterval exponential(0.1)")


![alt](plots/buffersParte2/v22-genint02.png "Caso 2 con genInterval exponential(0.2)")

![alt](plots/buffersParte2/v22-genint03.png "Caso 2 con genInterval exponential(0.3)")

![alt](plots/buffersParte2/v22-genint10.png "Caso 2 con genInterval exponential(1.0)")