

start_time <- Sys.time()


library(httr)
library(jsonlite)
library(tidyverse)
library(data.table)
library(parallel)

#--------------------------------------------------------------

## Ukcl prva stran
source("./skripte_uvoz_podatkov/uvoz_podatkov_ukcl.R")

## Raziskovalne skupine
source("./skripte_uvoz_podatkov/uvoz_podatkov_raziskovalne_skupine.R")

## Raziskovalci
source("./skripte_uvoz_podatkov/uvoz_podatkov_raziskovalci.R")

## ARIS projekti
source("./skripte_uvoz_podatkov/uvoz_podatkov_proj_aris.R")

## Programi ARIS
source("./skripte_uvoz_podatkov/uvoz_podatkov_programi.R")

#---------------------------------------------------------------

datum_posodobitve <- Sys.Date()

saveRDS(datum_posodobitve, "./data/datum_posodobitve.RDS")

#----------------------------------------------------------------

end_time <- Sys.time()

end_time - start_time


### Skripta za posodobitev za vseh 50k raziskovalcev - samo občasno (cca 10 min na več jedrih)

# source("./skripte_uvoz_podatkov/VSI_raziskovalci_delez_zaposlitve.R")
