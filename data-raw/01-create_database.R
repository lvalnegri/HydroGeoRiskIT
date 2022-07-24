#########################################################################
# ISPRA RISCHIO IDROGEO - Creazione databases e tabelle in MySQL server #
#########################################################################

library(Rfuns)

dbn <- 'ispra'
dd_create_db(dbn)

## TABLE <dataset> ----------------
x <- "
    `CMN` MEDIUMINT UNSIGNED NOT NULL,
    `var_id` CHAR(14) NOT NULL,
    `value` DECIMAL(10, 3) NOT NULL,
    PRIMARY KEY (`CMN`, `var_id`)
"
dd_create_dbtable('dataset', dbn, x)

## TABLE <metadata> ----------------
x <- "
    `var_id` CHAR(14) NOT NULL,
    `id_orig` CHAR(12) NOT NULL,
    `description` VARCHAR(120) NOT NULL,
    `label` VARCHAR(50) NULL DEFAULT NULL,
    `source` CHAR(5) NULL DEFAULT NULL,
    `year` SMALLINT UNSIGNED NULL DEFAULT NULL,
    `risk` CHAR(10) NULL DEFAULT NULL,
    `level` CHAR(15) NULL DEFAULT NULL,
    `clevel` CHAR(5) NULL DEFAULT NULL,
    `measure` CHAR(15) NULL DEFAULT NULL,
    `metric` CHAR(11) NULL DEFAULT NULL,
    PRIMARY KEY (`var_id`),
    KEY `level` (`level`),
    KEY `clevel` (`clevel`),
    KEY `measure` (`measure`),
    KEY `metric` (`metric`)
"
dd_create_dbtable('metadata', dbn, x)

## TABLE <cmn_lid> ----------------
x <- "
    `risk` CHAR(1) NOT NULL,
    `lid` CHAR(6) NOT NULL,
    `level` CHAR(3) NOT NULL,
    `id` SMALLINT UNSIGNED NOT NULL,
    `CMN` MEDIUMINT UNSIGNED NOT NULL,
    PRIMARY KEY (`risk`, `lid`, `CMN`),
    KEY `livello` (`level`),
    KEY `id` (`id`)
"
dd_create_dbtable('cmn_lid', dbn, x)

## TABLE <geoCMN> ----------------
x <- "
    `CMN` MEDIUMINT(6) UNSIGNED NOT NULL,
    `CMNd` CHAR(35) NOT NULL,
    `PRV` CHAR(3) NOT NULL,
    `PRVd` CHAR(30) NOT NULL,
    `PRVs` CHAR(2) NOT NULL,
    PRIMARY KEY (`CMN`),
    KEY `PRV` (`PRV`),
    KEY `PRVs` (`PRVs`)
"
dd_create_dbtable('geoCMN', dbn, x)

## FINE -------------------------------
rm(list = ls())
gc()
