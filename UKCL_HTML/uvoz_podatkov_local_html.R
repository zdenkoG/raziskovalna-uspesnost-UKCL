

library(XML)
library(xml2)
library(tidyverse)


#-------------------------------


##################################################################################################
## Uvoz podatkov iz sicrisa po letih - od 2016 dalje
## - izpis se prirpavi za celotno leto z vključenim seznamom avtorjev in brez seznama del
## - prirpavi se html datoteka kot je ta v mailu
## - predstavljene točje so dosožene v enem letu
## - Citati so že izračunani za pretekih 10 let in so prikazani skupni izračuni
###################################################################################################


## Določimo leta, za katera želimo, da se uvozijo podatki

leta <- 2011:format(Sys.Date(), "%Y")


#--------------------------------

############################
## Raziskovalci - po letih -> 'raz_tocke_leto'
############################




#### 
## FUNKCIJA ZA UVOZ PODATKOV IZ HTML DATOTEK
####

preberi_html_raz <- function(leto, base_path = "./podatki/sicris_raziskovalci_ukcl_leto", which = 1) {  ## which = 1 -> prebere 4. tabelo iz htmla-ja
  pot <- file.path(base_path, sprintf("%s.html", leto))
  # readHTMLTable vrne data.frame za izbrano tabelo (ali list ob izpustu 'which')
  df <- readHTMLTable(pot, which = which, stringsAsFactors = FALSE)
  df %>%
    mutate(leto = as.integer(leto))

}

## Prirpava tabele

raz_tocke_leto <- map_dfr(leta, preberi_html_raz) %>% 
  select(16, 1:6, 8:10) 


names(raz_tocke_leto) <- c("leto", "sifra", "raz", "up_tocke", 'A"', "A'", "A1/2", "CI10", "CImax", "h_index")


## Shranimo za nadaljne obdelave
saveRDS(raz_tocke_leto, "./podatki/raz_tocke_leto.RDS")

#-------------------------------------------------------------------------------------------------------------------------------


############################
## UKCL - po letih         ->    'ukc_tocke_leto'
############################



#### 
## FUNKCIJA ZA UVOZ PODATKOV IZ HTML DATOTEK
####

preberi_html_ukc <- function(leto, base_path = "./podatki/sicris_raziskovalci_ukcl_leto", which = 4) {  ## which = 4 -> prebere 4. tabelo iz htmla-ja
  pot <- file.path(base_path, sprintf("%s.html", leto))
  # readHTMLTable vrne data.frame za izbrano tabelo (ali list ob izpustu 'which')
  df <- readHTMLTable(pot, which = which, stringsAsFactors = FALSE)
  df %>%
    mutate(leto = as.character(leto))
}



### Prirpava tabele


ukc_tocke_leto <- map_dfr(leta, preberi_html_ukc) %>% 
  filter(!grepl("Točke|Ocena|Podatki", V2),
         !grepl("A31|A32|A33|A34|A35|A3|Ocena A1", V1)) %>% 
  as_tibble() %>% 
  mutate(ocene = rep(c("Upoštevane tč.", "A''", "A'", "A1/2", "CI10", "CImax", "h_index"),
                     length(leta))) %>%                                                      ## Ponovimo toliko krar kot je let
  select(leto, ocene, tocke = V2, opis = V1) 


  


## Shranimo za nadaljne obdelave
saveRDS(ukc_tocke_leto, "./podatki/ukc_tocke_leto.RDS")
