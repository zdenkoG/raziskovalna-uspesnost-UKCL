library(data.table)

# ==============
# Uvoz podatkov
# ==============

# --------------------------------------------------------
# Predhodno prirpavljeni podatki iz sicrisa - RAZISKOVALCI
# --------------------------------------------------------


raz <- readRDS("./data/raziskovalci.RDS")             # Raziskovalci
setDT(raz)

raz23 <- readRDS("./data/raz_fte_leta/raz2023.RDS")   # Število raziskovalcev in FTE po letih

raz24 <- readRDS("./data/raz_fte_leta/raz2024.RDS")   

raz25 <- readRDS("./data/raz_fte_leta/raz25.RDS")

# -----------------------------------------------------------------------
# Predhodno prirpavljeni podatki iz sicrisa - RAZISKOVALCI - po programu
# -----------------------------------------------------------------------


raz_prog <- readRDS("./data/prog_raziskovalci.RDS") %>% 
  rename(sicris_id_prog = project_id)
setDT(raz_prog)


# ------------------------------------------------------------
# Predhodno prirpavljeni podatki iz sicrisa - PROJEKTI - TOČKE
# ------------------------------------------------------------

ukc <- readRDS("./data/ukcl_podatki.RDS")          # Podatki iz prve strani UKCL (sicris)
setDT(ukc)

aris <- readRDS("./data/aris_projekti.RDS")        # ARIS projekti
setDT(aris)

##############

prog <- readRDS("./data/programi.RDS")             # Programi
setDT(prog)

## Dopolnimo s podatki o članih in raziskovalcih - PROGRAMI

# Izračuno članov po posameznem programu
clani_po_prog <- raz_prog %>% 
  mutate(researchload = if_else(researchload == 99, NA, researchload)) %>% 
  group_by(sicris_id_prog) %>% 
  summarise(st_clan = n(),
            raz = sum(vloga == "RSR", na.rm = TRUE),
            fte = round(sum(researchload, na.rm = TRUE)/100, 1)) %>% 
  ungroup()

# Podatkom o programih dodamo še podatke o številu članov
prog <- prog %>% 
  left_join(clani_po_prog,          
            by = "sicris_id_prog")
###################


skup <- readRDS("./data/raziskovalne_skupine.RDS") # Raziskovalne skupine
setDT(skup)

publ <- read_excel("./data/publikacije.xlsx")      # Publikacije
setDT(publ)

#---------------------------------------------------------





# ------------------------------
# REDCap - evidence CKR
#-------------------------------

source("./skripte_uvoz_podatkov/REDCap_evidence_CKR.R")


#---------------------------------------------------------
