


library(parallel)

# ==============================
## POVEZAVA NA SICRIS PREKO API
# ==============================

## JWT žeton

# URL za pridobitev JWT žetona
token_url <- "https://cris.cobiss.net/ecris/si/sl/service/getjwt"

# Podatki za avtentikacijo
credentials <- list(
  username = Sys.getenv("API_USERNAME"),
  password = Sys.getenv("API_PASSWORD")
)

# Pošlji POST zahtevo
response <- POST(
  url = token_url,
  body = credentials,
  encode = "json"
)

# Pridobi JWT token - PRAVILNO IME!
jwt_token <- content(response)$jwt

#-----------------------------------------------------------
#-----------------------------------------------------------

# ==============================
# PODATKI IZ STRANI OD UKCL
# =============================

## izberemo vse raziskovalne skupine

url <- "https://cris.cobiss.net/ecris/si/sl/service/organization/618"

a <- GET(url, add_headers(Authorization = jwt_token))


## dobimo seznam vseh projektov + programov

x <-  fromJSON(content(a, "text")) 

###############################################################
############
### raziskovalci - raziskovalci sicris ID
############

## Tabela z RAZISKOVALCI

raz <- as_tibble(x[["researchers"]]) %>% 
  select(sicris_id_raz = id,
         ime = fullName,
         naziv = title,
         st_raz = mstid,
         vloga = typeDescription) 


# ID raziskovalcev - sicris ID, potreben za združevanje z ostalimi tabelami - KEY

id_raz <- raz %>% 
  filter(vloga == "Raziskovalec") %>% 
  select(sicris_id_raz) %>% 
  unlist()

names(id_raz) <- NULL

# ===================================================
# PAMETNA NASTAVITEV ŠTEVILA JEDER (paket "parallel")
# ===================================================

num_cores <- detectCores()  ## Samodejno zazna število jeder

# Pametna izbira števila jeder za uporabo:
if (num_cores > 16) {
  cores_to_use <- floor(num_cores * 0.8)  # Za 22 jeder = 17-18 jeder
} else if (num_cores >= 8) {
  cores_to_use <- num_cores - 2
} else if (num_cores >= 4) {
  cores_to_use <- num_cores - 1
} else {
  cores_to_use <- max(1, num_cores)
}


# ==========================================
# ZDRUŽENA FUNKCIJA - en GET klic za vse podatke
# ==========================================

pridobi_vse_podatke_raz <- function(id_raz) {
  
  url <- paste0("https://cris.cobiss.net/ecris/si/sl/service/researcher/", id_raz)
  
  tryCatch({
    a <- GET(url, add_headers(Authorization = jwt_token))
    r <- fromJSON(content(a, 'text'))
    
    rezultat <- list(sicris_id_raz = id_raz)
    
    # TOČKE
    if(length(r[["evaluation"]]) > 0) {
      rezultat$tocke <- as_tibble(data.frame(r[["evaluation"]])) %>%
        mutate(sicris_id_raz = id_raz)
    }
    
    # CITATI
    if(length(r[["cit"]]) > 0) {
      rezultat$citati <- as_tibble(data.frame(r[["cit"]])) %>%
        mutate(sicris_id_raz = id_raz) %>%
        slice(1)
    }
    
    # EMAIL
    if(length(r[["contact"]][["email"]]) > 0) {
      rezultat$email <- as_tibble(data.frame(r[["contact"]][["email"]])) %>%
        mutate(sicris_id_raz = id_raz)
    }
    
    # DELEŽ ZAPOSLITVE
    if(length(r[["employs"]]) > 0) {
      rezultat$del_zap <- as_tibble(data.frame(r[["employs"]])) %>%
        filter(orgCode == '0312') %>%
        select(researchload) %>%
        mutate(sicris_id_raz = id_raz)
    }
    
    # PROGRAMI
    if(length(r[["programs"]]) > 0) {
      rezultat$programi <- r[["programs"]] %>%
        mutate(sicris_id_raz = id_raz)
    }
    
    # PROJEKTI
    if(length(r[["projects"]]) > 0) {
      rezultat$projekti <- r[["projects"]] %>%
        mutate(sicris_id_raz = id_raz)
    }
    
    # MLADI RAZISKOVALCI
    if(length(r[["yngResearchers"]]) > 0) {
      rezultat$mladi_raz <- r[["yngResearchers"]] %>%
        mutate(sicris_id_raz = id_raz)
    }
    
    return(rezultat)
    
  }, error = function(e) {
    return(list(sicris_id_raz = id_raz))
  })
}

# ==========================================
# IZVOZ PODATKOV - PARALELNO (Windows)
# ==========================================


# Ustvarimo cluster
cl <- makeCluster(cores_to_use)

# Izvozimo potrebne spremenljivke in pakete v cluster
clusterExport(cl, c("jwt_token", "pridobi_vse_podatke_raz"))
clusterEvalQ(cl, {
  library(httr)
  library(jsonlite)
  library(tidyverse)
})

# Progress bar
total <- length(id_raz)
pb <- txtProgressBar(min = 0, max = total, style = 3)
progress <- function(n) setTxtProgressBar(pb, n)

# Paralelno procesiranje z load balancing
vsi_podatki <- parLapplyLB(cl, id_raz, function(x) {
  result <- pridobi_vse_podatke_raz(x)
  return(result)
})

close(pb)
stopCluster(cl)


# ==========================================
# PRIPRAVA POSAMEZNIH TABEL
# ==========================================


# PROGRAMI
programi_ukcl <- vsi_podatki %>%
  map("programi") %>%
  compact() %>%
  bind_rows() %>%
  mutate(vodja = ifelse(rsrid == sicris_id_raz, 1, 0),
         enddate = as.Date(enddate),
         v_teku = ifelse(enddate > Sys.Date(), 1, 0))

raz_programi <- programi_ukcl %>%
  group_by(sicris_id_raz) %>%
  summarise(prog_st_skupaj = n(),
            prog_vodja_sk = sum(vodja),
            prog_skupaj_a = sum(v_teku),
            prog_sk_akt_v = sum(vodja == 1 & v_teku),
            .groups = "drop")

# PROJEKTI
projekti_ukcl <- vsi_podatki %>%
  map("projekti") %>%
  compact() %>%
  bind_rows() %>%
  mutate(vodja = ifelse(rsrid == sicris_id_raz, 1, 0),
         enddate = as.Date(enddate),
         v_teku = ifelse(enddate > Sys.Date(), 1, 0))

raz_projekti <- projekti_ukcl %>%
  group_by(sicris_id_raz) %>%
  summarise(aris_st_skupaj = n(),
            aris_vodja_sk = sum(vodja),
            aris_skupaj_a = sum(v_teku),
            aris_sk_akt_v = sum(vodja == 1 & v_teku),
            .groups = "drop")

# MLADI RAZISKOVALCI
mr_ukcl <- vsi_podatki %>%
  map("mladi_raz") %>%
  compact() %>%
  bind_rows() %>%
  select(sicris_id_raz, od = startdate, do = enddate, mstid) %>%
  mutate(od = as.Date(od),
         do = as.Date(do),
         ak_mentor = if_else(do > Sys.Date(), 1, 0))

mentorji <- mr_ukcl %>%
  group_by(sicris_id_raz) %>%
  summarise(st_mentor = n(),
            ak_mentor = sum(ak_mentor),
            .groups = "drop") %>%
  mutate(ak_mentor = if_else(is.na(ak_mentor), 1, ak_mentor))


# ==========================================
# TERCIARNI PROJEKTI (REDCap)
# ==========================================


url <- Sys.getenv("API_URL")
  
formData <- list("token"=Sys.getenv("API_TOKEN"),
                 content='record',
                 action='export',
                 format='csv',
                 type='flat',
                 csvDelimiter='',
                 'forms[0]'='register_raziskav_terciar',
                 'events[0]'='vpis_v_register__t_arm_3',
                 rawOrLabel='raw',
                 rawOrLabelHeaders='raw',
                 exportCheckboxLabel='false',
                 exportSurveyFields='false',
                 exportDataAccessGroups='false',
                 returnFormat='json'
)
response <- httr::POST(url, body = formData, encode = "form")
result <- httr::content(response)

terciar <- result %>%
  mutate(terciar_a = if_else(faza_izvedbe_raziskave_ter == 3, 0, 1)) %>%
  group_by(st_raz = sicris_st_raziskovalca_ter) %>%
  summarise(ter = n(),
            ter_a = sum(terciar_a),
            .groups = "drop")


# ==========================================
# KLINIČNA PRESKUŠANJA (REDCap)
# ==========================================


formData <- list("token"=Sys.getenv("API_TOKEN"),
                 content='record',
                 action='export',
                 format='csv',
                 type='flat',
                 csvDelimiter='',
                 'forms[0]'='register_raziskav',
                 'events[0]'='vpis_raziskave_v_r_arm_1',
                 rawOrLabel='label',
                 rawOrLabelHeaders='raw',
                 exportCheckboxLabel='false',
                 exportSurveyFields='false',
                 exportDataAccessGroups='false',
                 returnFormat='json'
)

response <- httr::POST(url, body = formData, encode = "form")
result <- httr::content(response)

kp <- result %>%
  separate(raziskovalec, c("st_raz", "ime"), sep = " - ") %>%
  select(st_raz, tip_raziskave, faza_izvedbe_raziskave) %>%
  mutate(tip_raziskave = if_else(tip_raziskave == "IIS", "IIS", "KP")) %>%
  group_by(st_raz, tip_raziskave) %>%
  count() %>%
  ungroup() %>%
  filter(!is.na(st_raz)) %>%
  pivot_wider(names_from = tip_raziskave, values_from = n)

akt_kp <- result %>%
  separate(raziskovalec, c("st_raz", "ime"), sep = " - ") %>%
  select(st_raz, tip_raziskave, faza_izvedbe_raziskave) %>%
  mutate(tip_raziskave = if_else(tip_raziskave == "IIS", "IIS", "KP")) %>%
  filter(faza_izvedbe_raziskave == "V teku") %>%
  group_by(st_raz, tip_raziskave) %>%
  count() %>%
  ungroup() %>%
  pivot_wider(names_from = tip_raziskave, values_from = n)

kp <- left_join(kp, akt_kp, by = "st_raz", suffix = c("", "_a")) %>%
  select(st_raz, kp = KP, kp_a = KP_a, iis = IIS, iis_a = IIS_a)
kp[is.na(kp)] <- 0


# ==========================================
# EVROPSKI PROJEKTI (REDCap)
# ==========================================

formData <- list("token"=Sys.getenv("API_TOKEN"),
                 content='record',
                 action='export',
                 format='csv',
                 type='flat',
                 csvDelimiter='',
                 'forms[0]'='register_eu_projekti',
                 'events[0]'='vpis_v_register__e_arm_4',
                 rawOrLabel='label',
                 rawOrLabelHeaders='raw',
                 exportCheckboxLabel='false',
                 exportSurveyFields='false',
                 exportDataAccessGroups='false',
                 returnFormat='json'
)

response <- httr::POST(url, body = formData, encode = "form")
result <- httr::content(response)

eu <- result %>%
  mutate(faza = if_else(faza_izvedbe_raziskave_eu == "V teku", 1, 0)) %>%
  select(st_raz = st_raz_eu, vodja = vodja_eu, faza) %>%
  group_by(st_raz) %>%
  summarise(eu = n(),
            eu_a = sum(faza),
            .groups = "drop")


# ==========================================
# ZDRUŽEVANJE VSEH PODATKOV
# ==========================================


raziskovalci <- raz %>%
  select(1, 2, 4, 5) %>%
  left_join(vsi_podatki %>% map("citati") %>% compact() %>% bind_rows(), 
            by = "sicris_id_raz") %>%
  left_join(vsi_podatki %>% map("tocke") %>% compact() %>% bind_rows(), 
            by = "sicris_id_raz") %>%
  left_join(vsi_podatki %>% map("email") %>% compact() %>% bind_rows(), 
            by = "sicris_id_raz") %>%
  left_join(raz_programi, by = "sicris_id_raz") %>%
  left_join(raz_projekti, by = "sicris_id_raz") %>%
  left_join(mentorji, by = "sicris_id_raz") %>%
  left_join(vsi_podatki %>% map("del_zap") %>% compact() %>% bind_rows(), 
            by = "sicris_id_raz") %>%
  select(-any_of(c("id.code", "id.type", "a3update"))) %>%
  rename(e_mail = 'r...contact......email...') %>%
  select(sicris_id_raz, ime, e_mail, st_raz, everything()) %>%
  mutate_if(is.integer, as.numeric) %>%
  mutate(e_mail = if_else(is.na(e_mail), "/", e_mail)) %>%
  left_join(terciar, by = "st_raz") %>%
  left_join(kp, by = "st_raz") %>%
  left_join(eu, by = "st_raz") %>% 
  select(-a3years) %>% 
  filter(ime != "test test")



raziskovalci[is.na(raziskovalci)] <- 0

raziskovalci <- raziskovalci %>%
  mutate(datum = Sys.Date())


#------------------------------------------


# =================
# Shranimo podatke
# =================

## VSE skupaj

saveRDS(raziskovalci, "./data/raziskovalci.RDS")


## Samo deleže zaposlitve (data/raz_fte_leto/)

raziskovalci %>% 
  filter(vloga == "Raziskovalec") %>% 
  select(sicris_id_raz, ime, st_raz, del_zap = researchload) %>% 
  saveRDS(file = paste0("./data/raz_fte_leta/raz", format(Sys.Date(), "%Y"), ".RDS"))   ## Določimo da shrani v pravo mapo v obliki "raz2026"



#------------------------------------------------------------
