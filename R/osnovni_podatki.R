

# =================================
# Osnovni podatki - osnovne matrike
# =================================


# Zadnje zaključeno leto
zadnje_leto <- max(publ$leto)


# Upoštevanje točke - celoten UKCL
upostevane_tocke <- ukc$a11

# Raziskovalci

st_raz_aris <- nrow(raz)                                       ## Št. registriranih pri ARIS

st_raz <- sum(raz$vloga == "Raziskovalec")                     ## Št. s statustom Raziskovalca

st_st_sod <- sum(raz$vloga != "Raziskovalec")                  ## Strokovni sodelavci ARIS

fte <-  raz[vloga == "Raziskovalec", sum(researchload)/100]    ## Število FTE (delež zaposlitve)


## Datum posodobljenih podatkov

datum_posodobitve <- readRDS(here("data/datum_posodobitve.RDS"))


# Raziskovalne skupine - število raziskovalnih skupin

st_raz_sk <- nrow(skup)


  
# Programske skupine - aktivni programi

akt_prog <- prog[grepl("^618", org_sifra) & konec > Sys.Date()] ## Aktivnih ARIS PROG

st_prog_sk <- nrow(akt_prog)                                    ## šT. Aktivnih prog


# Aktivni ARIS projekti

akt_aris <- aris[grepl("^618", org_sifra) & konec > Sys.Date()] ## Aktivni ARIS proj

st_aris_proj <- nrow(akt_aris)                                  ## Št. aktivnih ARIS





