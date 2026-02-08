


# =============================================================
# PRIRPAVA PODATKOV O PROGRAMIH, KI SO V TEKU - TOČKE & CITATI
# =============================================================



# -------------------------
# Programi, ki so v teku
# --------------------------

prog %>% 
  filter(konec > Sys.Date()) %>% 
  select(sicris_id_prog, code, name,  st_raz, vodja = raz.x, st_clan, raz = raz.y, fte, 
         org, zacetek:a12, aI:hIndex) %>% 
  as_tibble() %>% 
  mutate(ukc_vod = if_else(grepl("^UKC", org), 1, 0)) -> prog_v_teku


#----------------------------------------------------------------

# ------------------------------------
# FUNKCIJA: IZRAČUN KAZALNIKOV
# -----------------------------------

# Funkcija za izračun kazalnikov na raziskovalca in FTE
# 
# Package dependencies: dplyr (mora biti naložen pred uporabo funkcije)
#
# @param kazalnik Character string - ime stolpca s kazalnikom (npr. "a11", "a12", itd.)
# @param podatki Data frame - vedno prog_v_teku
# @return Data frame z izbranimi stolpci in izračunanimi kazalniki

izracunaj_kazalnike <- function(kazalnik, podatki = prog_v_teku) {
  
  podatki %>% 
    select(code, vodja, name, raz, fte, all_of(kazalnik), ukc_vod) %>% 
    mutate(
      kaz_fte = round(.data[[kazalnik]] / fte, digits = 1),
      kaz_raz = round(.data[[kazalnik]] / raz)
    ) %>% 
    relocate(ukc_vod, .after = last_col())
}

# Primer uporabe:
# rezultat <- izracunaj_kazalnike("a11")
# rezultat <- izracunaj_kazalnike("a12")




# --------------------------
# Upoštevane točke
# ---------------------------

up_tocke <- izracunaj_kazalnike("a11")


# ---------------------
# Izjemni dosežki - A''
# ---------------------

aII_prog <- izracunaj_kazalnike("aII")


# -----------------------------
# Zelo kvalitetni dosežki - A'
# -----------------------------

aI_prog <- izracunaj_kazalnike("aI")


# -------------------------
# Pomembni dosežki - A1/2
# -------------------------

a12_prog <- izracunaj_kazalnike("a12")



# ------------------------------
# CI10 - število čistih citatov
# ------------------------------

ci10_prog <- izracunaj_kazalnike("ci10")



# -----------------------------
# CImax - najodmevnejše delo in
# h-indeks 
# -----------------------------

prog_v_teku %>% 
  select(code, vodja, name, st_clan:fte, cimax:ukc_vod) -> cit_prog