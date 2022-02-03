USE sakila;

/*
2 - El top 5 de actrices y actores de la tabla `actors` que tienen la mayor experiencia (i.e.
el mayor número de películas filmadas) son también directores de las películas en
las que participaron. Basados en esta información, inserten, utilizando una subquery
los valores correspondientes en la tabla `directors`.
*/

INSERT INTO directors (
SELECT a.actor_id ,a.first_name , a.last_name ,d.number_of_movies FROM actor a
INNER JOIN directors d ON d.director_id = a.actor_id
ORDER BY 
(SELECT COUNT(film_id) FROM film_actor fa WHERE a.actor_id=fa.actor_id));
    

/*
 * 3 - Agregue una columna `premium_customer` que tendrá un valor 'T' o 'F' de acuerdo a
 * si el cliente es "premium" o no. Por defecto ningún cliente será premium.
*/ 

#4 - Modifique la tabla customer. Marque con 'T' en la columna `premium_customer` de
#los 10 clientes con mayor dinero gastado en la plataforma.

#No puedo usar limit en subqueries pero no se me ocurre otra forma
WITH 
    premiums AS (SELECT customer_id
                FROM payment
                GROUP BY customer_id
                ORDER BY sum(amount) DESC)
UPDATE customer 
SET premium_customer = 'T'
WHERE customer_id IN (SELECT * FROM premiums LIMIT 10);

/*
 * 5. Listar, ordenados por cantidad de películas (de mayor a menor), los distintos ratings
 * de las películas existentes (Hint: rating se refiere en este caso a la clasificación
 * según edad: G, PG, R, etc).
*/

SELECT f.rating, count(*) AS n_of_movies FROM film f GROUP BY f.rating ORDER BY n_of_movies DESC;

#6. ¿Cuáles fueron la primera y última fecha donde hubo pagos? 

SELECT MIN(payment_date) AS opening, MAX(payment_date) AS closing FROM payment;

/*
 * 7. Calcule, por cada mes, el promedio de pagos (Hint: vea la manera de extraer el
 * nombre del mes de una fecha).
*/

# Asumo que se refiere a los ingresos totales, sino no entiendo
select monthname(payment_date) as mes, avg(amount) 
from payment
group by mes
order by mes desc;

/*
 * 8. Listar los 10 distritos que tuvieron mayor cantidad de alquileres (con la cantidad total
de alquileres).
*/

WITH districts AS (
    SELECT rental.rental_id, customer.customer_id, address.district
    FROM rental 
    INNER JOIN customer ON customer.customer_id = rental.customer_id 
    INNER JOIN address ON customer.address_id = address.address_id
)

SELECT district, count(*) AS rentals
FROM districts
GROUP BY district
ORDER BY rentals DESC;


/*
9 - Modifique la table `inventory_id` agregando una columna `stock` que sea un número
entero y representa la cantidad de copias de una misma película que tiene
determinada tienda. El número por defecto debería ser 5 copias.
*/

ALTER TABLE inventory ADD stock int NOT NULL DEFAULT 5;
#INSERT INTO inventory VALUES (
#    SELECT film_id ,COUNT(film_id) FROM inventory GROUP BY film_id);

/*
10 - Cree un trigger `update_stock` que, cada vez que se agregue un nuevo registro a la
tabla rental, haga un update en la tabla `inventory` restando una copia al stock de la
película rentada (Hint: revisar que el rental no tiene información directa sobre la
tienda, sino sobre el cliente, que está asociado a una tienda en particular).
*/

CREATE TRIGGER update_stock AFTER INSERT ON rental 
UPDATE inventory 
SET stock = stock - 1
WHERE inventory_id = NEW.inventory_id;

























