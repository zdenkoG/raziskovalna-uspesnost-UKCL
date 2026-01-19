

# ==============
# Uvoz podatkov
# ==============

raz <- readRDS("./data/raziskovalci.RDS")          # Raziskovalci
setDT(raz)

ukc <- readRDS("./data/ukcl_podatki.RDS")          # Podatki iz prve strani UKCL (sicris)
setDT(ukc)

aris <- readRDS("./data/aris_projekti.RDS")        # ARIS projekti
setDT(aris)

prog <- readRDS("./data/programi.RDS")             # Programi
setDT(prog)

skup <- readRDS("./data/raziskovalne_skupine.RDS") # Raziskovalne skupine
setDT(skup)

publ <- read_excel("./data/publikacije.xlsx")      # Publikacije

#-------------------------------------------------


# ========================
# Ossnovne številke
# ========================

# Zadnje zaključeno leto
zadnje_leto <- max(publ$leto)

# Upoštevanje točke
upostevane_tocke <- ukc$a11

# Raziskovalci

st_raz_aris <- nrow(raz) 
st_raz <- sum(raz$vloga == "Raziskovalec")
st_st_sod <- sum(raz$vloga != "Raziskovalec")
fte <-  raz[vloga == "Raziskovalec", sum(researchload)/100] 


# Raziskovalne skupine

st_raz_sk <- nrow(skup)
  
# Programske skupine - aktivni programi

akt_prog <- prog[grepl("^618", org_sifra) & konec > Sys.Date()] ## Aktivnih ARIS PROG
st_prog_sk <- nrow(akt_prog)                                    ## šT. Aktivnih prog


# Aktivni ARIS projekti

akt_aris <- aris[grepl("^618", org_sifra) & konec > Sys.Date()] ## Aktivni ARIS proj
st_aris_proj <- nrow(akt_aris)                                  ## Št. aktivnih ARIS

# -----------------------------------
# Tabela osnovni podatki UKCL
# -----------------------------------

tabela_osn_podatki <- 
  data.frame(
    org = "UKC Ljubljana",
    st_raz = st_raz_aris,
    raz = st_raz,
    fte = fte,
    strok_sod = st_st_sod,
    prog = st_prog_sk,
    raz_sk = st_raz_sk,
    aris = st_aris_proj
)



