--1
CREATE TABLE directors (
	director_id SMALLINT UNSIGNED NOT NULL AUTO_INCREMENT,
	first_name VARCHAR(45) NOT NULL,
	last_name VARCHAR(45) NOT NULL,
	no_of_films INT NOT NULL,
	PRIMARY KEY  (director_id),
	CONSTRAINT fk_director_actor_id FOREIGN KEY (director_id) REFERENCES actor (actor_id) ON DELETE CASCADE ON UPDATE CASCADE
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4;

--2
INSERT INTO directors (first_name, last_name, no_of_films)
WITH most_experienced_performers AS (
	SELECT actor_id, COUNT(*) AS no_of_films 
	FROM film_actor 
	GROUP BY (actor_id) 
	ORDER BY no_of_films DESC 
	LIMIT 5
), most_experienced_performers_names AS (
	SELECT first_name, last_name, no_of_films
	FROM actor a1 
		INNER JOIN most_experienced_performers a2
		ON (a1.actor_id = a2.actor_id)
) SELECT * FROM most_experienced_performers_names;

--3
ALTER TABLE customer
ADD premium_customer char(1) NOT NULL DEFAULT 'F';

--4
UPDATE customer SET customer.premium_customer = 'T'
WHERE customer.customer_id IN (
    SELECT premium.customer_id 
    FROM (
        SELECT customer_id, sum(amount) AS total_spent 
        FROM payment 
        GROUP BY customer_id 
        ORDER BY total_spent DESC 
        LIMIT 10
    ) AS premium
);

--5
SELECT f.rating, count(*) AS no_of_movies
FROM film f
GROUP BY f.rating
ORDER BY no_of_movies DESC;

--6
SELECT MIN(payment_date) AS opening, MAX(payment_date) AS closing 
FROM payment;

--7
SELECT MONTHNAME(payment_date) AS mes, AVG(amount) AS promedio
FROM payment
GROUP BY mes;

--8
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

--9
ALTER TABLE inventory
ADD stock int NOT NULL DEFAULT 5;

--10
delimiter //
CREATE TRIGGER update_stock AFTER INSERT ON rental 
FOR EACH ROW
BEGIN
	UPDATE inventory
	SET stock = stock - 1 
	WHERE inventory_id = NEW.inventory_id;
END//
delimiter ;

--11
CREATE TABLE fines(
	  rental_id INT NOT NULL AUTO_INCREMENT,
	  amount DECIMAL(5,2) NOT NULL,
	  FOREIGN KEY (rental_id) REFERENCES rental(rental_id)
	  )
	  
--12
delimiter ;;
CREATE PROCEDURE check_date_and_fine()
BEGIN	
INSERT INTO fines
WITH overdues AS (SELECT rental_id, (1.5 * datediff(return_date, rental_date))
				  FROM rental
				  WHERE datediff(return_date, rental_date) > 3)
				  
SELECT * FROM overdues;
END
;;
delimiter;

--13
CREATE ROLE employee;
GRANT ALL PRIVILEGES ON rental TO employee;

--14
REVOKE DELETE ON rental FROM employee;
CREATE ROLE administrator;
GRANT ALL PRIVILEGES TO administrator;

--15
CREATE USER empleado1 DEFAULT ROLE employee;
CREATE USER empleado2 DEFAULT ROLE employee;
CREATE USER eladmin DEFAULT ROLE administrator;



