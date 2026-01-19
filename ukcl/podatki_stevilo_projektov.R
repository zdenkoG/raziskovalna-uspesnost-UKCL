

# ====================================================
# PRIRPAVA PODATKOV O ŠTEVILU PROJEKTOV PO LETU
# Vir podatkov je REDCap - register raziskav & sicris
# ====================================================


# ------------------------------------------------------
## 0. Uvoz podatkov iz REDCap-a - Register raziskav UKCL
# ------------------------------------------------------


formData <- list("token"=Sys.getenv("API_TOKEN_REGISTER"),
                 content='record',
                 action='export',
                 format='csv',
                 type='flat',
                 csvDelimiter='',
                 'fields[0]'='record_id',
                 'fields[1]'='sklop_raziskave1',
                 'fields[2]'='zacetek_raziskave',
                 'fields[3]'='konec_raziskave',
                 'fields[4]'='sklop_raziskave1_ter',
                 'fields[5]'='zacetek_raziskave_ter',
                 'fields[6]'='konec_raziskave_ter',
                 'fields[7]'='sklop_raziskave1_arrs',
                 'fields[8]'='zacetek_raziskave_arrs',
                 'fields[9]'='konec_raziskave_arrs',
                 'fields[10]'='sklop_raziskave_prog',
                 'fields[11]'='zacetek_programa',
                 'fields[12]'='konec_programa',
                 'fields[13]'='sklop',
                 'fields[14]'='zacetek_eu',
                 'fields[15]'='konec_eu',
                 'fields[16]'='vloga_ukcl_v_raziskavi_arrs',
                 'fields[17]'='datum_podpisa_pogodbe',
                 rawOrLabel='label',
                 rawOrLabelHeaders='raw',
                 exportCheckboxLabel='false',
                 exportSurveyFields='false',
                 exportDataAccessGroups='false',
                 returnFormat='json'
)

response <- httr::POST(url, body = formData, encode = "form")

result <- httr::content(response) %>% 
  filter(grepl("Vpis", redcap_event_name))


# -------------------------------------------------------------
## 1. PRIPRAVA PODATKOV - združitev vseh projektov v eno tabelo
# -------------------------------------------------------------

# KP projekti - Register
kp_projekti <- result %>% 
  filter(grepl("KP", redcap_event_name)) %>% 
  select(record_id, 
         sklop = sklop_raziskave1, 
         zacetek = datum_podpisa_pogodbe, 
         konec = konec_raziskave)

# Terciarni projekti - Register
terciarni_projekti <- result %>% 
  filter(grepl("Terciar", redcap_event_name)) %>% 
  select(record_id, 
         sklop = sklop_raziskave1_ter, 
         zacetek = zacetek_raziskave_ter,
         konec = konec_raziskave_ter)

# ARRS programi - sicris
aris_programi <- prog %>% 
  filter(grepl("^618", org_sifra)) %>%          ## Samo kjer je UKCL vodilna ustanova
  select(zacetek, konec) %>% 
  mutate(record_id = NA,
         sklop = "ARIS programi",
         .before = zacetek) %>% 
  mutate(zacetek = as.Date(zacetek),
         konec = as.Date(konec))



# ARRS projekti - sicris
aris_projekti <- aris %>% 
  filter(grepl("^618", org_sifra)) %>%         ## Samo kjer je UKCL vodilna ustanova
  mutate(zacetek = as.Date(zacetek),
         konec = as.Date(konec),
         record_id = NA,
         sklop = "ARIS projekti") %>% 
  filter(zacetek >= '2018-01-01') %>% 
  select(record_id, sklop, zacetek, konec) 



# EU projekti - Register
eu_projekti <- result %>% 
  filter(grepl("EU", redcap_event_name)) %>% 
  select(record_id, 
         sklop, 
         zacetek = zacetek_eu,
         konec = konec_eu)


# Združi vse projekte
projekti_cas <- bind_rows(
  kp_projekti,
  terciarni_projekti,
  aris_programi,
  aris_projekti,
  eu_projekti
) %>% 
  mutate(
    # Če projekt nima konca, nastavi na današnji datum
    konec = if_else(is.na(konec), today(), konec),
    # Poenotenje imen sklopov
    sklop = if_else(sklop == "Terciarni projekt", "Terciarni projekti", sklop)
  ) %>% 
  # Odstrani projekte brez začetnega datuma
  filter(!is.na(zacetek), !is.na(sklop))



# -----------------------------------------------
## 2. NOVI PROJEKTI PO LETIH (zacetek >= 2018)
# ----------------------------------------------

novi_projekti <- projekti_cas %>% 
  # Ekstrakcija leta iz datuma začetka
  mutate(leto_zacetka = year(zacetek)) %>% 
  # Filtriranje od 2018 dalje
  filter(leto_zacetka >= 2018) %>% 
  # Štetje po sklop in leto
  group_by(sklop, leto_zacetka) %>% 
  summarise(n_projektov = n(), .groups = "drop") %>% 
  # Preoblikovanje v široko obliko (leta kot stolpci)
  arrange(leto_zacetka) %>% 
  pivot_wider(
    names_from = leto_zacetka,
    values_from = n_projektov,
    names_prefix = "l",
    values_fill = 0
  )


# -----------------------------------------------
## 3. TEKOČI PROJEKTI NA 31.12. POSAMEZNEGA LETA
# -----------------------------------------------

podatki <- projekti_cas

# Pretvori v Date format
podatki$zacetek <- as.Date(podatki$zacetek)
podatki$konec <- as.Date(podatki$konec)

# Kjer manjka konec, nastavi na daleč v prihodnost (študija še v teku)
podatki$konec[is.na(podatki$konec)] <- as.Date("2099-12-31")

# Leta za preverjanje
leta <- 2018:zadnje_leto

# Unikatne skupine
skupine <- unique(podatki$sklop)


# Izračun po skupinah
rezultat <- do.call(rbind, lapply(skupine, function(sk) {
  podmnozica <- podatki[podatki$sklop == sk, ]
  stevila <- sapply(leta, function(leto) {
    datum <- as.Date(paste0(leto, "-12-31"))
    sum(podmnozica$zacetek <= datum & podmnozica$konec >= datum)
  })
  data.frame(sklop = sk, t(stevila))
}))

# Poimenuj stolpce z leti
colnames(rezultat) <- c("sklop", leta)

rezultat[rezultat$sklop == "Klinicna preskusanja zdravil in med. prip.", 
         c("2018", "2019", "2020", "2021")] <- NA
# Dodaj * pri kliničnih preskušanjih
rezultat$sklop[rezultat$sklop == "Klinicna preskusanja zdravil in med. prip."] <- 
  "Klinična preskušanja*"

# Dodaj ** pri EU in mednarodnih projektih
rezultat$sklop[rezultat$sklop == "EU in mednarodni projekti"] <- 
  "EU projekti**"




