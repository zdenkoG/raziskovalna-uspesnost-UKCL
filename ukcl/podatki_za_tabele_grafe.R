

# =============================================
# Prirpava podatkov in tabel za izpise in grafe
# =============================================


# -----------------------------------
# Tabela osnovni podatki UKCL
# -----------------------------------

tabela_osn_podatki <- 
  data.frame(
    org = "UKC Ljubljana",
    st_raz = st_raz_aris,
    raz = st_raz,
    fte = round(fte,1),
    strok_sod = st_st_sod,
    prog = st_prog_sk,
    raz_sk = st_raz_sk,
    aris = st_aris_proj
  ) 


# ---------------------------------
# Tabela vrednotenje in citiranost
# ---------------------------------

tabela_vrednot_ukc <-
  ukc %>% 
    select(org, a11:hIndex) %>% 
    mutate(org = "UKC Ljubljana") %>% 
    select(org, a11, aII, aI, a12, ci10, cimax, hIndex) 



# ---------------------------------
# Tabela osnovni kazalniki 
# ---------------------------------

tabela_osn_kaz <- 
  ukc %>% 
  select(ci10, a11, aII, a12) %>% 
  pivot_longer(ci10:a12, names_to = "kaj", values_to = "tocke") %>% 
  mutate(raz = round(tocke/st_raz, 1),
         fte = round(tocke/fte, 1)) %>% 
  mutate(kaj = case_match(
    kaj, 
    "ci10" ~ "CI10 - število čistih citatov",
    "a11" ~ "Upoštevane točke",
    "aII" ~ "A'' - izjemni dosežki",
    "a12" ~ "A1/2 - Odlični in izjemni dosežki"
  )) 


# ----------------------------------
# Tabela finančna sredstva UKCL
# ----------------------------------


tabela_sredstva <- 
  podatki_ckr %>%
    filter(leto < format(Sys.Date(), "%Y")) %>%     ## Odstranimo tekoče leto 
    group_by(leto, vrsta_projekta) %>% 
    summarise(sred = sum(fin_sredstva)) %>% 
    ungroup() %>% 
    bind_rows(
      podatki_ckr %>%
        filter(leto < format(Sys.Date(), "%Y")) %>%  ## Odstranimo tekoče leto
        group_by(leto, vrsta_projekta = "UKCL") %>% 
        summarise(sred = sum(fin_sredstva)) %>% 
        ungroup()
    ) %>% 
    pivot_wider(names_from = vrsta_projekta, values_from = sred) %>% 
    arrange(desc(leto)) %>% 
    filter(!is.na(UKCL))  



# ----------------------------
# Raziskovalne skupine v UKCL
# -----------------------------

# Raziskovalne skupine UKCL - izberemo samo nekaj spremenljivk za prikaz
tabela_raz_sk <- skup %>% 
  select(st_raz_sk, naziv, vodja, skupaj_clanov, st_raziskov, fte, v_teku_prog) 


tabela_raz_sk$v_teku_prog[is.na(tabela_raz_sk$v_teku_prog)] <- 0
tabela_raz_sk$vodja[tabela_raz_sk$naziv == "SPS Kirurška klinika, Klinični oddelek za otroško kirurgijo in intenzivno terapijo"] <-"/"


# --------------------------
# Programske skupine v UKCL
# --------------------------

## UKCL vodilna ustanova
tabela_programi_ukcl <- 
  prog %>% 
  filter(grepl("^618", org_sifra),
         konec > Sys.Date()) %>% 
  select(code, name, raz.x, zacetek, konec, st_clan:fte, org) %>% 
  mutate(zacetek = as.Date(zacetek),
         konec = as.Date(konec))

## Preštejemo koliko organizacij sodeluje v programu

tabela_programi_ukcl$org <- sapply(tabela_programi_ukcl$org, function(x) {
  if (is.na(x) || x == "") {
    return(0)
  }
  length(strsplit(x, ",\\s*")[[1]])
})


## UKCL sodeluje

tabela_programi_zunanji <- prog %>% 
  filter(!grepl("^618", org_sifra),
         konec > Sys.Date()) %>% 
  select(code, name, raz.x, org, zacetek, konec) %>% 
  mutate(org = case_when(grepl("^FMF", org) ~ "Fakulteta za matematiko in fiziko",
                         grepl("^MF", org) ~ "Medicinska fakulteta",
                         grepl("^NUTRIS", org) ~ "Inštitut za nutricionistioko",
                         grepl("^FF", org) ~ "Filozofska fakulteta",
                         grepl("^OIL", org) ~ "Onkološki inštitut Ljubljana"),
         zacetek = as.Date(zacetek),
         konec = as.Date(konec))
