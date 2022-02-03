# Grupo 01		
## Corrección		
	Tag o commit corregido:	"1.0"
		
### Entrega y git		100.00%
	Informe	100.00%
	Commits de cada integrante	100.00%
	En tiempo y con tag correcto	100.00%
	Commits frecuentes y con nombres significativos	100.00%
### Funcionalidad		81.00%
	unfold computa correctamente	30.00%
	unfold maneja correctamente los casos de error	30.00%
	compute computa correctamente	50.00%
	compute maneja correctamente los casos de error	100.00%
	time computa correctamente	100.00%
	time maneja correctamente los casos de error	100.00%
	run computa correctamente	100.00%
	run maneja correctamente los casos de error	100.00%
### Modularización y diseño		80.00%
	El tipo Song está bien definido	100.00%
	No simplificaron Repeat y Transpose_by con funciones de Euterpea	100.00%
	El tipo Command está bien definido	100.00%
	El código está bien estructurado	50.00%
	Modularizan funciones aprovechando el alto-orden	0.00%
	Reutilizan funciones de librería	100.00%
### Calidad de código		40.10%
	Estilo de línea	0.00%
	Estructuras de código simples	70.00%
	Estilo de código	50.00%
### Opcionales		
	Puntos estrella	0.00%
		
# Nota Final		6.60475
		
		
# Comentarios		
Las instrucciones que dejaron en el INSTALL no funcionaban, igual lo pude compilar		
		
En el unfold, sí se pueden transponer cosas con un argumento negativo. Se llama "bajar de tono" a la canción.		
		
Falla el siguiente caso:		
```unfold $ Repeat 2 $ Transpose_by 1 $ Fragment [Note (1 % 4) (C,2),Rest (1 % 4)]		
Obtained: Just (Concat (Transpose_by 1 (Fragment [Note (1 % 4) (C,2),Rest (1 % 4)])) (Transpose_by 1 (Fragment [Note (1 % 4) (C,2),Rest (1 % 4)])))		
Expected: Just (Concat (Fragment [Note (1 % 4) (Cs,2),Rest (1 % 4)]) (Fragment [Note (1 % 4) (Cs,2),Rest (1 % 4)]))```		
		
El error (no sé si el único) está en la línea 64, donde ponen `unfold s = Just s` y no contemplan el caso en el que primero haya cualquier contructor y después, adentro de s, haya un Transpose_by o un Repeat. Eso también hace que no manejen bien los casos de error.		
		
Cuando traté de ejecutar los test de compute, tiró este error: `Non-exhaustive patterns in function pitcheopp` porque faltan los casos en que la lista tiene un sólo elemento o está vacía. En general, toda la lógica de las funciones pitchear y pitcheopp no es la recursión adecuada con caso base y caso recursivo. Por ejemplo `pitchear 0 (Fragment s) = s`, no hace falta ver la estructura interna de s si no lo vamos a transponer en nada. Otra cosa es que sacan la lista de Primitive Pitch de adentro del constructor Fragment en disarm, pero después lo vuelven a agregar en pitcheopp para pasarlo a pitchear. 		
		
En ningún lado el concepto de programación de alto orden del paradigma funcional (map, filter, etc.)		
		
Estilo: traten de siempre mantener el idioma del código consistente. Y como los operadores del lenguaje el 99% del tiempo están en inglés, se considera mejor si los nombres de las funciones y variables también están en inglés. Lo digo por el método *pitchear*.		
		
Estilo: mantengan líneas de 80 caracteres. Para ustedes capaz parece algo pavo, pero para personas (como yo) que no ven bien la pantalla y tienen que aumentar el tamaño del texto, cuando se cruzan las líneas se complica la legibilidad.
