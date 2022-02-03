USE classicmodels;

#1 - Devuelva la oficina con mayor número de empleados.

WITH max_office AS (
    SELECT officeCode, COUNT(*) AS c FROM employees e 
    GROUP BY officeCode 
    ORDER BY c DESC LIMIT 1
)

SELECT * FROM offices o 
WHERE officeCode = (SELECT officeCode FROM max_office ORDER BY officeCode LIMIT 1);

/*
2 - ¿Cuál es el promedio de órdenes hechas por oficina?, ¿Qué oficina vendió la mayor
cantidad de productos?
*/

WITH sales_per_off AS
(
    SELECT o.officeCode, count(*) AS num
    FROM offices o
    INNER JOIN employees e ON o.officeCode=e.officeCode
    INNER JOIN customers c ON e.employeeNumber=c.salesRepEmployeeNumber
    INNER JOIN orders op ON c.customerNumber=op.customerNumber
    GROUP BY o.officeCode
)

SELECT avg(s.num) AS Average, MAX(s.num) AS Max 
FROM sales_per_off AS s;

/*3 - Devolver el valor promedio, máximo y mínimo de pagos que se hacen por mes.*/

WITH payments_per_month AS (
    SELECT COUNT(*) AS num FROM offices o 
    INNER JOIN employees e ON e.officeCode = o.officeCode 
    INNER JOIN customers c ON c.salesRepEmployeeNumber = e.employeeNumber 
    INNER JOIN payments p ON p.customerNumber = c.customerNumber
    GROUP BY MONTH(p.paymentDate);
) 

SELECT AVG(ppm.num), MAX(ppm.num) , MIN(ppm.num) 
FROM payments_per_month AS ppm;

#La otra alternativa, no entendi bien
SELECT AVG(amount), MAX(amount),MIN(amount) FROM payments p GROUP BY MONTH (p.paymentDate) ORDER BY MONTH (p.paymentDate );

/*
4 - Crear un procedimiento "Update Credit" en donde se modifique el límite de crédito de
un cliente con un valor pasado por parámetro.
*/

CREATE PROCEDURE update_credit (IN customer_n int , IN new_credit int) BEGIN 
    UPDATE customers c 
    SET c.creditLimit = new_credit
    WHERE c.customerNumber = customer_n;
END

CALL update_credit(103,500);

/*
5 - Cree una vista "Premium Customers" que devuelva el top 10 de clientes que más
dinero han gastado en la plataforma. La vista deberá devolver el nombre del cliente,
la ciudad y el total gastado por ese cliente en la plataforma.
*/

WITH top_customers AS (
    SELECT customerNumber AS cn ,SUM(amount) AS amount FROM payments p 
    GROUP BY customerNumber 
    ORDER BY n DESC LIMIT 10
)

CREATE VIEW premium_customers AS
SELECT c.customerName , c.city , tc.amount  FROM customers c
INNER JOIN top_customers tc ON c.customerNumber = tc.cn;


/*6 - Cree un procedimiento "employee of the month" que tome un mes y un año y
devuelve el empleado (nombre y apellido) cuyos clientes hayan efectuado la mayor
cantidad de órdenes en ese mes. */

CREATE FUNCTION get_employee_of_the_month (m int,y int) 
RETURNS int
DETERMINISTIC 
BEGIN
    DECLARE res int;
    WITH payments_from_month AS (
        SELECT p.customerNumber AS cn ,COUNT(*) AS c FROM payments p 
        WHERE YEAR (p.paymentDate) = y AND MONTH(p.paymentDate) = m
        GROUP BY p.customerNumber
    ) 
    
    SELECT e.employeeNumber AS en FROM employees e
    WHERE e.employeeNumber = (
        SELECT c.salesRepEmployeeNumber FROM customers c
        WHERE c.customerNumber = (
            SELECT cn FROM payments_from_month pfm 
            WHERE cn = (SELECT MAX(pfmm.cn) FROM payments_from_month pfmm)
        )
    ) INTO res;
    RETURN res;
END 


/*
7. Crear una nueva tabla "Product Refillment". Deberá tener una relación varios a uno
con "products" y los campos: `refillmentID`, `productCode`, `orderDate`, `quantity`.
*/

CREATE TABLE productRefillment (
    refillmentID int NOT NULL,
    productCode varchar(15) NOT NULL,
    orderDate date NOT NULL,
    quantity int NOT NULL,
    PRIMARY KEY (refillmentID),
    KEY productCode (productCode),
    CONSTRAINT `fk_product_code` FOREIGN KEY (productCode) REFERENCES products (productCode) ON DELETE RESTRICT ON UPDATE CASCADE
)ENGINE=InnoDB DEFAULT CHARSET=latin1;

/*8. Definir un trigger "Restock Product" que esté pendiente de los cambios efectuados
en `orderdetails` y cada vez que se agregue una nueva orden revise la cantidad de
productos pedidos (`quantityOrdered`) y compare con la cantidad en stock
(`quantityInStock`) y si es menor a 10 genere un pedido en la tabla "Product
Refillment" por 10 nuevos productos.
*/

delimiter //
CREATE TRIGGER restock_product AFTER INSERT ON orderdetails
FOR EACH ROW
BEGIN
  IF NEW.quantityOrdered - (SELECT quantityInStock AS q FROM products WHERE productCode=NEW.productCode) < 10
      THEN INSERT INTO prod_refill(quantity, productCode) VALUES (10,NEW.productCode);
  END IF;
END//
delimiter ;

/*9. Crear un rol "Empleado" en la BD que establezca accesos de lectura a todas las
tablas y accesos de creación de vistas.*/

CREATE ROLE employee_role;
GRANT SELECT 
ON *
TO employee_role;

GRANT CREATE VIEW 
ON *
TO employee_role;






























