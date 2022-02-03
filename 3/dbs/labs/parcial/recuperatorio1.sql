Ejercicio 1

CREATE TABLE `purchase` (
    `user` INT NOT NULL,
    `game` INT NOT NULL,
    `hours_played` INT,
    CONSTRAINT purchasePK PRIMARY KEY (`user`, `game`),
    CONSTRAINT userPurchaseFK FOREIGN KEY (`user`) REFERENCES `user` (`id`),
    CONSTRAINT gamePurchaseFK FOREIGN KEY (`game`) REFERENCES `game` (`id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4;
Ejercicio 2:

ALTER TABLE `game`
ADD CONSTRAINT `uniqueTitleRelease` UNIQUE (`title`, `release_date`);
Ejercicio 3:

/* a */

ALTER TABLE `game`
ADD COLUMN `discount` DECIMAL(3,0) NOT NULL DEFAULT 0;

/* b */

DELIMITER //

CREATE PROCEDURE `set_game_discount` (IN `company_name` VARCHAR(200),
                                      IN `new_discount` DECIMAL(3,0)
                                     )
BEGIN
    UPDATE `game` AS `g`
    INNER JOIN `publishers` AS `p` ON `g`.`id` = `p`.`game`
    INNER JOIN `company` AS `c` ON `p`.`publisher` = `c`.`id`
    SET `g`.`discount` = `new_discount`
    WHERE `c`.`name` = `company_name`;
END//

DELIMITER ;

/* c */

DELIMITER //

CREATE FUNCTION `get_game_final_price`(`game_id` INT)
RETURNS DECIMAL(5,2)
READS SQL DATA
BEGIN
    DECLARE `game_price`, `game_final_price` DECIMAL(5,2);
    DECLARE `game_discount` DECIMAL(3,0);

    SELECT `price`, `discount` INTO `game_price`, `game_discount`
    FROM `game`
    WHERE `id` = `game_id`;

    IF `game_discount` = 0
    THEN
        SET `game_final_price` = `game_price`;
    ELSE
        SET `game_final_price` = `game_price` * (100 - `game_discount`)/100;
    END IF;

    RETURN `game_final_price`;
END//

DELIMITER ;
Ejercicio 4

/* a */

ALTER TABLE `genre`
ADD COLUMN `profit` DOUBLE NOT NULL DEFAULT 0;

/* b */

WITH
    `game_profit` AS (
        SELECT `g`.`id` AS `id`, COUNT(*) * `price` AS `profit`
        FROM `game` AS `g`
        INNER JOIN `purchase` AS `p` ON `g`.`id` = `p`.`game`
        GROUP BY `g`.`id`
    ),
    `genre_profit` AS (
        SELECT `gg`.`genre` AS `id`, SUM(`gp`.`profit`) AS `profit`
        FROM `game_genres` AS `gg`
        INNER JOIN `game_profit` AS `gp` ON `gg`.`game` = `gp`.`id`
        GROUP BY `gg`.`genre`
    )
UPDATE `genre` AS `g`
SET `profit` = (
    SELECT `gp`.`profit`
    FROM `genre_profit` AS `gp`
    WHERE `gp`.`id` = `g`.`id`
);

/* c */

DELIMITER //

CREATE TRIGGER `update_genre_profit`
AFTER INSERT ON `purchase` FOR EACH ROW
BEGIN
    UPDATE `genre` AS `gnr`
    INNER JOIN `game_genres` AS `gg` ON `gnr`.`id` = `gg`.`genre`
    INNER JOIN `game` AS `g` ON `gg`.`game` = `g`.`id`
    SET `gnr`.`profit` = `gnr`.`profit` + `g`.`price`
    WHERE `p`.`game` = NEW.`game`;
END//

DELIMITER ;
Ejercicio 5

CREATE VIEW `super_gamer` AS
    WITH
        `most_played_games_by_user` AS (
            SELECT DISTINCT `user`,
                   NTH_VALUE(`game`, 1) OVER `W` AS `first_game`,
                   NTH_VALUE(`hours_played`, 1) OVER `W` AS `first_game_hours`,
                   NTH_VALUE(`game`, 2) OVER `W` AS `second_game`,
                   NTH_VALUE(`hours_played`, 2) OVER `W` AS `second_game_hours`
            FROM `purchase`
            WINDOW `W` AS (PARTITION BY `user` ORDER BY `hours_played` DESC)
        )
    SELECT `u`.`username` AS `username`,
           `g1`.`title` AS `most_played_game`,
           `g2`.`title` AS `second_most_played_game`
    FROM `most_played_games_by_user` AS `mpgbu`
    INNER JOIN `user` AS `u` ON `mpgbu`.`user` = `u`.`id`
    INNER JOIN `game` AS `g1` ON `mpgbu`.`first_game` = `g1`.`id`
    INNER JOIN `game` AS `g2` ON `mpgbu`.`second_game` = `g2`.`id`
    WHERE `mpgbu`.`first_game_hours` >= 10 * `mpgbu`.`second_game_hours`
    ORDER BY `first_game_hours` DESC
    LIMIT 10;
Ejercicio 6

CREATE ROLE `client_support`;

GRANT SELECT, INSERT ON `user` TO `client_support`;

GRANT SELECT, UPDATE (`hours_played`) ON `purchase` TO `client_support`;
