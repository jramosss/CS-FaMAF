USE `university`;

DROP TABLE thesis;

#Ej 1
#Modifique la tabla `advisor` de manera que cada par sea único.
ALTER TABLE `advisor` ADD UNIQUE (s_ID);
ALTER TABLE `advisor` ADD UNIQUE (i_ID);

#Ej 2
CREATE TABLE `thesis`(
    `student_id` varchar(5) NOT NULL,
    `director_id` varchar(5) NOT NULL,
    `codirector_id` varchar(5),
    `title` varchar(100) NOT NULL,
    `pages` int NOT NULL,
    PRIMARY KEY (`student_id`,`title`),
    FOREIGN KEY (student_id) REFERENCES student (ID),
    FOREIGN KEY (director_id) REFERENCES instructor (ID),
    FOREIGN KEY (codirector_id) REFERENCES instructor (ID)
);

#Ej 3
#Listar el top-10 de departamentos por número de tesistas.

WITH `thesists` AS (
    SELECT s.id , t.director_id, i.dept_name FROM `student` s
    INNER JOIN `thesis` t ON s.ID = t.student_id
    INNER JOIN `instructor` i ON t.director_id = i.ID
)

SELECT `dept_name`,COUNT(*) AS n_thesists FROM `thesists`
GROUP BY `dept_name` ORDER BY n_thesists DESC  LIMIT 10 ;

#Ej 4
/*4. Listar el nombre del estudiante, nombre del director y título de la tesis de aquellos
directores que tienen más de 45 estudiantes a cargo.*/

WITH `popular_instructors` AS (
    SELECT `director_id` FROM (
        SELECT `director_id`,COUNT(*) AS n_students FROM `thesis` t
        GROUP BY `director_id` 
    ) AS `directors_student_count`
    WHERE n_students > 45
)

SELECT s.name , d.name , t.title FROM `student` s 
INNER JOIN thesis t ON s.ID = t.student_id 
INNER JOIN `instructor` d ON t.director_id = d.ID
WHERE d.name IN (
    SELECT `name` FROM `instructor` i 
    WHERE ID IN (SELECT director_id FROM popular_instructors)
);


/*5. Crear 2 triggers, `add_advisor` y `remove_advisor`, estos se ejecutarán cuando se
cree o elimine un registro en la tabla `thesis` y deberán crear o eliminar un registro
en la tabla `advisor` que refleje al estudiante y a sus directores (i.e. si el codirector
no es nulo, deberá crear/eliminar 2 entradas, una del estudiante y el director y otra
del estudiante y el codirector).*/

delimiter //
CREATE TRIGGER `add_advisor` AFTER INSERT ON `thesis`
FOR EACH ROW 
BEGIN 
    INSERT INTO `advisor`(a_ID,s_ID,i_ID) VALUES (NEW.student_id,NEW.director_id);
    IF NEW.codirector_id IS NOT NULL THEN
        INSERT INTO advisor VALUES (NEW.student_id,NEW.codirector_id);
    END IF;
END//
delimiter ;

delimiter //
CREATE TRIGGER `remove_advisor` AFTER DELETE ON `thesis`
FOR EACH ROW 
BEGIN 
    DELETE FROM `advisor` a
    WHERE a.s_ID = OLD.student_id
    AND 
    a.i_ID = OLD.director_id;
END//
delimiter ;


/*
6. Eliminar aquellas tesis de los departamentos que en total tengan menos de 50 tesis
(el departamento está dado por el director).
a. Hint: Esto se resuelve con consultas anidadas y sin utilizar CTE (no hay
soporte para CTE en un DELETE en MySQL).
b. Hint: Tengan en cuenta que a la hora de eliminar elementos de una tabla,
cuando se hace necesario utilizar joins, hay que declarar de qué tabla se está
eliminando (en esta caso, sólo eliminar de la tabla `thesis`).
*/

DELETE FROM `thesis`
WHERE `director_id` IN (
    SELECT `ID` FROM `instructor` i2
    WHERE i2.dept_name IN (
        SELECT `dept` FROM (
            SELECT i.dept_name AS dept, COUNT(*) AS cou FROM `thesis` t 
            INNER JOIN instructor i ON t.director_id = i.ID
            GROUP BY i.dept_name
        ) AS `depts_thesis` WHERE cou < 50
    )
);

/*
7. Crear un procedimiento `update_student_dept_name` que tome el nombre de un
estudiante como dato de entrada, verifique si el nombre del departamento al que
pertenece dicho estudiante es el mismo que el de su director de tesis, y sólo en caso
de que no lo sea, lo actualice poniendo el nombre del departamento de su director
de tesis.
a. Hint: Pueden ver de llamar el procedimiento sólo sobre nombres que sean
únicos, aquí una lista: 'Abraham', 'Achilles', 'Adam', 'Adda', 'Baroni', 'Cole',
'Colin'.
*/
 
delimiter //
CREATE PROCEDURE `update_student_dept_name` (IN student_name varchar(20)) BEGIN
    WITH rebel_students AS (
        SELECT s.name AS sname,i.dept_name AS dname FROM `thesis` t 
        INNER JOIN instructor i ON t.director_id = i.ID 
        INNER JOIN student s ON t.student_id = s.ID 
        WHERE i.dept_name <> s.dept_name
    )
    
    UPDATE `student` s
    SET dept_name = (SELECT dname FROM rebel_students rs WHERE rs.sname = student_name)
    WHERE s.name IN (SELECT sname FROM rebel_students);
END //
delimiter ; 

/*
8. Crear un rol `administrative` que tenga permisos de lectura y eliminación sobre la
tabla `instructor` y permisos de actualización sobre la columna `salary` de la tabla
`instructor`
*/

CREATE ROLE administrative;

GRANT SELECT , DELETE 
ON instructor
TO administrative;

GRANT UPDATE 
ON instructor.salary 
TO administrative;








































