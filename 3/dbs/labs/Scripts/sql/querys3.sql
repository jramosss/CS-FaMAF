USE world;

#1- Lista el nombre de la ciudad, nombre del país, región y forma de gobierno de las 10 ciudades más pobladas del mundo.
SELECT city.Name , country.Name , country.Region , country.GovernementForm FROM country
INNER JOIN city ON country.Code=city.CountryCode ORDER BY city.Population DESC LIMIT 10;

#2- Listar los 10 países con menor población del mundo, junto a sus ciudades capitales (Hint: puede que uno de estos países no tenga ciudad capital asignada, en este caso deberá mostrar "NULL").
# I think i dint understood because its easier than the others
SELECT Name, Capital FROM country ORDER BY Population ASC LIMIT 10;

#3- Listar el nombre, continente y todos los lenguajes oficiales de cada país. (Hint: habrá más de una fila por país si tiene varios idiomas oficiales).
SELECT country.Name, country.Continent , countrylanguage.`Language` FROM country 
INNER JOIN countrylanguage ON country.Code=countrylanguage.CountryCode 
WHERE countrylanguage.IsOfficial='T';

#4- Listar el nombre del país y nombre de capital, de los 20 países con mayor superficie del mundo.
SELECT Name,Capital FROM country ORDER BY SurfaceArea LIMIT 20;

#5- Listar las ciudades junto a sus idiomas oficiales (ordenado por la población de la ciudad) y el porcentaje de hablantes del idioma.
# Dont really know where the error is
SELECT city.Name , countrylanguage.`Language` , countrylanguage.Percentage FROM countrylanguage
INNER JOIN city USING (CountryCode)
WHERE countrylanguage.IsOfficial='T' ORDER BY city.Population DESC ;

#6- Listar los 10 países con mayor población y los 10 países con menor población (que tengan al menos 100 habitantes) en la misma consulta.
(SELECT Name FROM country ORDER BY Population DESC LIMIT 10)
UNION 
(SELECT Name FROM country ORDER BY Population ASC LIMIT 10);

#7- Listar aquellos países cuyos lenguajes oficiales son el Inglés y el Francés (hint: no debería haber filas duplicadas).
SELECT Name FROM country AS c 
WHERE c.Code IN (SELECT CountryCode FROM countrylanguage WHERE `Language`='English' AND IsOfficial='T') AND 
c.Code IN (SELECT CountryCode FROM countrylanguage WHERE `Language`='French' AND IsOfficial='T');

#8- Listar aquellos países que tengan hablantes del Inglés pero no del Español en su población.
SELECT Name FROM country WHERE Code NOT IN  
(SELECT CountryCode FROM countrylanguage WHERE Language='Spanish')
AND Code IN (SELECT CountryCode FROM countrylanguage WHERE Language='English');


## Preguntas

#¿Devuelven los mismos valores las siguientes consultas? ¿Por qué? 
SELECT city.Name, country.Name
FROM city
INNER JOIN country ON city.CountryCode = country.Code AND country.Name = 'Argentina';

SELECT city.Name, country.Name
FROM city
INNER JOIN country ON city.CountryCode = country.Code
WHERE country.Name = 'Argentina';

#Si por que ambas son condiciones que debe cumplir el join

#¿Y si en vez de INNER JOIN fuera un LEFT JOIN?
SELECT city.Name, country.Name
FROM city
LEFT JOIN country ON city.CountryCode = country.Code AND country.Name = 'Argentina';

SELECT city.Name, country.Name
FROM city
LEFT JOIN country ON city.CountryCode = country.Code
WHERE country.Name = 'Argentina';

# No sabria decir por que ¿?¿?¿?¿?¿?











