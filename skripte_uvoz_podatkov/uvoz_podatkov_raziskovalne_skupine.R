


# ==========================================
# POVEZAVA NA SICRIS PREKO API
# ==========================================

token_url <- "https://cris.cobiss.net/ecris/si/sl/service/getjwt"

credentials <- list(
  username = Sys.getenv("API_USERNAME"),
  password = Sys.getenv("API_PASSWORD")
)

token_response <- POST(
  url = token_url,
  body = credentials,
  encode = "json"
)

token_data <- fromJSON(content(token_response, "text"))
jwt_token <- token_data$jwt


# ==========================================
# RAZISKOVALNE SKUPINE
# ==========================================

url <- "https://cris.cobiss.net/ecris/si/sl/service/group/search?query=*&limit=ALL"
a <- GET(url, add_headers(Authorization = jwt_token))

raz_sk_api <- data.frame(fromJSON(content(a, "text"))) %>% 
  filter(orgid == 618)

raz_sk_api %>%
  as_tibble() %>% 
  select(sicris_id_grp = id,
         id.code = code,
         st_raz_sk = code,
         naziv = name,
         vodja = resaercherFullName,
         st_raz = rsrCode) -> raz_sk

id_group <- raz_sk %>% 
  select(sicris_id_grp) %>% 
  unlist()
names(id_group) <- NULL


# ==========================================
# ZDRUŽENA FUNKCIJA - 1 GET KLIC ZA VSE
# ==========================================

pridobi_vse_podatke_grp <- function(id_group) {
  
  url <- paste0("https://cris.cobiss.net/ecris/si/sl/service/group/", id_group)
  
  tryCatch({
    a <- GET(url, add_headers(Authorization = jwt_token))
    x <- fromJSON(content(a, 'text'))
    
    rezultat <- list(sicris_id_grp = id_group)
    
    # CITATI
    if(length(x[["cit"]]) > 0) {
      rezultat$citati <- as_tibble(data.frame(x[["cit"]])) %>%
        mutate(sicris_id_grp = id_group)
    }
    
    # TOČKE
    if(length(x[["evaluation"]]) > 0) {
      rezultat$tocke <- as_tibble(data.frame(x[["evaluation"]])) %>%
        mutate(sicris_id_grp = id_group)
    }
    
    # RAZISKOVALCI PO SKUPINAH
    if(length(x[["researchers"]]) > 0) {
      rezultat$raziskovalci <- as_tibble(data.frame(x[["researchers"]])) %>%
        select(sicris_id_raz = id,
               raziskovalec = fullName,
               st_raz = mstid,
               naziv = title,
               vloga = typeDescription) %>%
        mutate(sicris_id_grp = id_group)
    }
    
    return(rezultat)
    
  }, error = function(e) {
    message(paste("Napaka pri skupini", id_group, ":", e$message))
    return(list(sicris_id_grp = id_group))
  })
}

# ==========================================
# IZVOZ PODATKOV - OSNOVNA R FOR ZANKA
# ==========================================


# OSNOVNA FOR ZANKA 
vsi_podatki_grp <- vector("list", length(id_group))

for (i in seq_along(id_group)) {
  vsi_podatki_grp[[i]] <- pridobi_vse_podatke_grp(id_group[i])
  cat(".")  # Progress indicator
  
  # Vsake 10 iteracij izpiši status
  if (i %% 10 == 0) {
    cat(sprintf(" [%d/%d]\n", i, length(id_group)))
  }
}


# ==========================================
# PRIPRAVA POSAMEZNIH TABEL
# ==========================================

# RAZISKOVALCI PO SKUPINAH
raz_po_skupinah <- vsi_podatki_grp %>%
  map("raziskovalci") %>%
  compact() %>%
  bind_rows()

raz_grp <- raz_po_skupinah %>% 
  group_by(sicris_id_grp) %>% 
  summarise(skupaj_clanov = n(),
            z_dr = sum(naziv == "dr."),
            st_raziskov = sum(vloga == "Raziskovalec"),
            .groups = "drop")

## Uvozimo podatke o raziskovalcih, da dopolnimo tabelo
raz <- readRDS("./data/raziskovalci.RDS") %>% 
  select(-any_of("sicris_id_grp"))                       ## Odstranimo stare podatke o skupinah, če so shranjeni                           


## tabeli 'raz' dodamo podatek o raziskovalni skupini

raz %>% 
  left_join(raz_po_skupinah %>% 
              select(sicris_id_raz, sicris_id_grp),
            by = "sicris_id_raz") -> raz

# shranimo dopolnjene podatke o raziskovalcih
saveRDS(raz, "./data/raziskovalci.RDS")

# tabeli 'raz_grp' dodamo podatek o fte 
raz_grp %>% 
  left_join(
    raz %>%                                     ## Izračunamo FTE po skupini in dodamo k tabeli
      select(sicris_id_grp, researchload) %>% 
      group_by(sicris_id_grp) %>% 
      summarise(fte = sum(researchload)/100) %>% 
      ungroup()
  ) -> raz_grp

# ==========================================
# PROGRAMI ARIS PO SKUPINAH
# ==========================================

url <- "https://cris.cobiss.net/ecris/si/sl/service/organization/618"
a <- GET(url, add_headers(Authorization = jwt_token))
x <- fromJSON(content(a, "text"))

prog_ukcl <- as_tibble(x[["programs"]]) %>% 
  select(sicris_id_prog = id, st_prog = code, naziv = name,
         od = startdate, do = enddate,
         st_raz = rsrCode, vodja = resaercherFullName,
         projectId)

vodje_skupin <- raz_po_skupinah %>% 
  select(sicris_id_grp, st_raz)

prog_ukcl <- prog_ukcl %>% 
  left_join(vodje_skupin, by = "st_raz") %>% 
  arrange(st_prog, sicris_id_prog) %>%
  group_by(st_prog) %>% 
  fill(sicris_id_grp, .direction = "up") %>%
  ungroup() %>% 
  filter(!is.na(sicris_id_grp)) %>% 
  mutate(v_teku = if_else(do > Sys.Date(), 1, 0))

prog_za_grp <- prog_ukcl %>% 
  group_by(sicris_id_grp) %>% 
  summarise(skupaj_prog = n(),
            v_teku_prog = sum(v_teku == 1),
            .groups = "drop")


# ==========================================
# PROJEKTI ARIS PO SKUPINAH
# ==========================================

proj_aris <- as_tibble(x[["projects"]]) %>%
  select(sicris_id_proj = id,
         st_proj = code, 
         naziv = name,
         od = startdate,
         do = enddate,
         vodja = resaercherFullName,
         st_raz = rsrCode) %>% 
  semi_join(
    raz_po_skupinah %>% 
      select(st_raz, raziskovalec, sicris_id_grp),
    by = "st_raz"
  ) %>% 
  left_join(raz_po_skupinah %>%
              select(st_raz, sicris_id_grp),
            by = "st_raz") %>% 
  mutate(v_teku = if_else(do > Sys.Date(), 1, 0))

proj_za_grp <- proj_aris %>% 
  group_by(sicris_id_grp) %>% 
  summarise(skupaj_projek = n(),
            v_teku_projek = sum(v_teku == 1),
            .groups = "drop")


# ==========================================
# EU PROJEKTI (REDCap)
# ==========================================


url <- Sys.getenv("API_URL")

formData <- list("token"=Sys.getenv("API_TOKEN"),
                 content='record',
                 action='export',
                 format='csv',
                 type='flat',
                 csvDelimiter='',
                 'forms[0]'='register_eu_projekti',
                 'events[0]'='vpis_v_register__e_arm_4',
                 rawOrLabel='raw',
                 rawOrLabelHeaders='raw',
                 exportCheckboxLabel='false',
                 exportSurveyFields='false',
                 exportDataAccessGroups='false',
                 returnFormat='json'
)

response <- httr::POST(url, body = formData, encode = "form")
eu_proj <- httr::content(response)

eu_grp <- eu_proj %>% 
  select(st_raz = st_raz_eu, od = zacetek_eu, do = konec_eu) %>% 
  mutate(v_teku = if_else(do > Sys.Date(), 1, 0)) %>% 
  left_join(
    raz_po_skupinah %>% 
      select(st_raz, sicris_id_grp)
  ) %>% 
  group_by(sicris_id_grp) %>% 
  summarise(st_eu_proj = n(),
            v_teku_eu = sum(v_teku == 1, na.rm = TRUE),
            .groups = "drop") %>% 
  filter(!is.na(sicris_id_grp))


# ==========================================
# KLINIČNA PRESKUŠANJA IN IIS (REDCap)
# ==========================================


formData <- list("token"=Sys.getenv("API_TOKEN"),
                 content='record',
                 action='export',
                 format='csv',
                 type='flat',
                 csvDelimiter='',
                 'fields[0]'='record_id',
                 'fields[1]'='raziskovalec',
                 'fields[2]'='tip_raziskave',
                 'fields[3]'='vloga_ukcl_v_raziskavi',
                 'fields[4]'='datum_podpisa_pogodbe',
                 'fields[5]'='faza_izvedbe_raziskave',
                 'events[0]'='vpis_raziskave_v_r_arm_1',
                 rawOrLabel='label',
                 rawOrLabelHeaders='raw',
                 exportCheckboxLabel='false',
                 exportSurveyFields='false',
                 exportDataAccessGroups='false',
                 returnFormat='json'
)

response <- httr::POST(url, body = formData, encode = "form")
kp <- httr::content(response) %>% 
  select(-c(2:4))

kp1 <- kp %>% 
  mutate(
    st_raz = as.character(gsub("\\D", "", raziskovalec)),
    v_teku = if_else(faza_izvedbe_raziskave == "V teku", 1, 0),
    vodilni = if_else(vloga_ukcl_v_raziskavi == "Vodilna", 1, 0),
    tip_raziskave = if_else(tip_raziskave == "IIS", "IIS", "KP")
  ) %>% 
  filter(
    nchar(st_raz) > 1,
    datum_podpisa_pogodbe >= '2018-01-01'
  ) %>% 
  select(-c(vloga_ukcl_v_raziskavi, faza_izvedbe_raziskave)) %>% 
  left_join(
    raz_po_skupinah %>% 
      select(st_raz, sicris_id_grp)
  ) %>% 
  filter(!is.na(sicris_id_grp))

kp_tbl <- kp1 %>% 
  group_by(sicris_id_grp, tip_raziskave) %>% 
  summarise(st_kp_iis = n(),
            vod_kp_is = sum(vodilni == 1),
            v_teku_kp_iis = sum(v_teku == 1),
            .groups = "drop")

kp_iis_za_grp <- kp_tbl %>% 
  filter(tip_raziskave == "KP") %>% 
  left_join(
    kp_tbl %>% 
      filter(tip_raziskave == "IIS"),
    by = "sicris_id_grp"
  )


# ==========================================
# TERCIARNI PROJEKTI (REDCap)
# ==========================================


formData <- list("token"=Sys.getenv("API_TOKEN"),
                 content='record',
                 action='export',
                 format='csv',
                 type='flat',
                 csvDelimiter='',
                 'fields[0]'='record_id',
                 'fields[1]'='sicris_st_raziskovalca_ter',
                 'fields[2]'='zacetek_raziskave_ter',
                 'fields[3]'='konec_raziskave_ter',
                 'events[0]'='vpis_v_register__t_arm_3',
                 rawOrLabel='label',
                 rawOrLabelHeaders='raw',
                 exportCheckboxLabel='false',
                 exportSurveyFields='false',
                 exportDataAccessGroups='false',
                 returnFormat='json'
)

response <- httr::POST(url, body = formData, encode = "form")

ter <- httr::content(response) %>% 
  select(-c(2:4)) %>% 
  rename(st_raz = sicris_st_raziskovalca_ter) %>%
  mutate(v_teku = if_else(konec_raziskave_ter > Sys.Date(), 1, 0),
         st_raz = if_else(nchar(st_raz) < 5, paste0("0", st_raz), st_raz)) %>% 
  filter(zacetek_raziskave_ter >= Sys.Date() - years(10)) %>%
  left_join(
    raz_po_skupinah %>% 
      select(st_raz, sicris_id_grp),
    by = "st_raz"
  )

ter_za_grp <- ter %>% 
  group_by(sicris_id_grp) %>% 
  summarise(ter = n(),
            v_teku_ter = sum(v_teku == 1),
            .groups = "drop") %>% 
  filter(!is.na(sicris_id_grp))



# ==========================================
# ZDRUŽEVANJE VSEH PODATKOV - KONČNA TABELA
# ==========================================


raziskovalne_skupine <- raz_sk %>% 
  left_join(
    vsi_podatki_grp %>% map("citati") %>% compact() %>% bind_rows(),
    by = "sicris_id_grp"
  ) %>% 
  select(-any_of(c("id.type", "id.code.y"))) %>% 
  left_join(
    vsi_podatki_grp %>% map("tocke") %>% compact() %>% bind_rows(),
    by = "sicris_id_grp"
  ) %>% 
  left_join(raz_grp, by = "sicris_id_grp") %>% 
  left_join(prog_za_grp, by = "sicris_id_grp") %>% 
  left_join(proj_za_grp, by = "sicris_id_grp") %>% 
  left_join(kp_iis_za_grp, by = "sicris_id_grp") %>% 
  left_join(ter_za_grp, by = "sicris_id_grp") %>% 
  left_join(eu_grp, by = "sicris_id_grp") %>% 
  mutate(vodja = paste0("dr. ", vodja)) %>% 
  select(-id.code.x)

#---------------------------------------------------------

# ======================================
# Shranimo podatke za uporabo na strani
# =====================================

saveRDS(raziskovalne_skupine, "./data/raziskovalne_skupine.RDS")


#---------------------------------------------------------------
#---------------------------------------------------------------


