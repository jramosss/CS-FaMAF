USE world;

#1 - Listar el nombre de la ciudad y el nombre del país de todas las ciudades que
#pertenezcan a países con una población menor a 10000 habitantes.

# Con INNER JOIN
#SELECT city.Name, country.Name FROM city
#INNER JOIN country ON city.CountryCode=country.Code
#WHERE country.Population < 10000;

# Con INNER JOIN pero mas complicado usando IN
SELECT city.Name, country.Name FROM city
INNER JOIN country ON city.CountryCode=country.Code 
WHERE country.Code IN 
(SELECT Code FROM country WHERE Population < 10000);


#2 - Listar todas aquellas ciudades cuya población sea mayor que la población promedio entre todas las ciudades.
SELECT Name FROM city WHERE CountryCode IN (
SELECT CountryCode FROM city
WHERE AVG(Population) > ALL (SELECT AVG(Population) FROM city)
);

#3 - Listar todas aquellas ciudades no asiáticas cuya población sea igual o mayor a la población total de algún país de Asia.
SELECT Name FROM city WHERE CountryCode NOT IN 
(SELECT Code FROM country WHERE Continent != 'Asia') 
AND Population >= SOME 
(SELECT Population FROM country WHERE Code IN 
(SELECT Code FROM country WHERE Continent = 'Asia')
);

#4 - Listar aquellos países junto a sus idiomas no oficiales, que superen en porcentaje de hablantes a cada uno de los idiomas oficiales del país.
SELECT country.Name , countrylanguage.`Language` FROM country 
INNER JOIN countrylanguage ON country.Code = countrylanguage.CountryCode
WHERE countrylanguage.IsOfficial = 'F' AND countrylanguage.Percentage  > 
ALL (SELECT Percentage FROM countrylanguage WHERE countrylanguage.IsOfficial = 'T');
# Esto funciona en mi cabeza pero me da 0 filas asi que supongo que esta mal


#5. Listar (sin duplicados) aquellas regiones que tengan países con una superficie menor
#a 1000 km 2 y exista (en el país) al menos una ciudad con más de 100000 habitantes.
#(Hint: Esto puede resolverse con o sin una subquery, intenten encontrar ambas respuestas).
SELECT Region FROM country WHERE SurfaceArea > 1000 
AND Code IN 
(SELECT Code FROM city WHERE Population > 10000);


#6. Listar el nombre de cada país con la cantidad de habitantes de su ciudad más
#poblada. (Hint: Hay dos maneras de llegar al mismo resultado. Usando consultas
#escalares o usando agrupaciones, encontrar ambas).
SELECT Name , (SELECT max(Population) FROM city WHERE CountryCode=Code) AS max_popul FROM country;


#7. Listar aquellos países y sus lenguajes no oficiales cuyo porcentaje de hablantes sea
#mayor al promedio de hablantes de los lenguajes oficiales.
#! No es la misma que la 4?

#8. Listar la cantidad de habitantes por continente ordenado en forma descendiente.
SELECT SUM(Population) FROM country GROUP BY Continent ORDER BY sum(Population) DESC; 

#9. Listar el promedio de esperanza de vida (LifeExpectancy) por continente con una
#esperanza de vida entre 40 y 70 años.
SELECT avg(LifeExpectancy) FROM country AS c
WHERE c.LifeExpectancy > 40 AND c.LifeExpectancy < 70
GROUP BY Continent;


#10. Listar la cantidad máxima, mínima, promedio y suma de habitantes por continente.
SELECT max(Population), min(Population), avg(Population), sum(Population) FROM country c
GROUP BY Continent;

##Preguntas
#1. Si en la consulta 6 se quisiera devolver, además de las columnas ya solicitadas, el
#nombre de la ciudad más poblada. ¿Podría lograrse con agrupaciones? ¿y con una
#subquery escalar?














