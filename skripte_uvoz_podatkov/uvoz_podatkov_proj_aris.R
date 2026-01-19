


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

# ==========================================
# OSNOVNI PODATKI O PROJEKTIH
# ==========================================

url <- "https://cris.cobiss.net/ecris/si/sl/service/organization/618"
a <- GET(url, add_headers(Authorization = jwt_token))
x <- fromJSON(content(a, "text"))

proj <- as.data.frame(x$projects)

# Vektor vseh ID-jev projektov
proj_ids <- proj %>% select(id) %>% unlist()
names(proj_ids) <- NULL


#-----------------------------------------------------------------------

# ==============================================
# ZDRUŽENA FUNKCIJA - 1 GET KLIC ZA VSE PODATKE
# ==============================================



pridobi_podatke_projekta <- function(project_id) {
  
  url <- paste0("https://cris.cobiss.net/ecris/si/sl/service/project/", project_id)
  
  tryCatch({
    a <- GET(url, add_headers(Authorization = jwt_token))
    x1 <- fromJSON(content(a, "text"))
    
    rezultat <- list(project_id = project_id)
    
    # PODATKI O PROJEKTU - VEDNO ustvari
    rezultat$project_data <- data.frame(
      project_id = project_id,
      id = if(!is.null(x1$organizations) && nrow(x1$organizations) > 0) {
        paste(x1$organizations[, "id"], collapse = ", ")
      } else NA_character_,
      remark = if(!is.null(x1$organizations) && nrow(x1$organizations) > 0) {
        paste(x1$organizations[, "remark"], collapse = ", ")
      } else NA_character_,
      id_proj = if(!is.null(x1$code)) x1$code else NA_character_,
      counter = if(!is.null(x1$counter)) x1$counter else NA_integer_,
      zacetek = if(!is.null(x1$startdate)) x1$startdate else NA_character_,
      konec = if(!is.null(x1$enddate)) x1$enddate else NA_character_,
      stringsAsFactors = FALSE
    )
    
    # RAZISKOVALCI - VEDNO ustvari (lahko prazno)
    if(!is.null(x1$researchers) && nrow(x1$researchers) > 0) {
      rezultat$researchers <- data.frame(
        raz = x1$researchers$fullName,
        vloga = x1$researchers$type,
        org = x1$researchers$orgid,
        sicris_id_raz = x1$researchers$id,
        st_raz = x1$researchers$mstid,
        project_id = project_id,
        stringsAsFactors = FALSE
      )
    } else {
      # Prazna vrstica, če ni raziskovalcev
      rezultat$researchers <- data.frame(
        raz = NA_character_,
        vloga = NA_character_,
        org = NA_integer_,
        id_raz = NA_integer_,
        st_raz = NA_character_,
        project_id = project_id,
        stringsAsFactors = FALSE
      )[0, ]  # Prazna tabela z 0 vrsticami, ampak pravilnimi stolpci
    }
    
    # EVALUATION - VEDNO ustvari
    rezultat$evaluation <- data.frame(
      a11 = if(!is.null(x1$evaluation$a11)) x1$evaluation$a11 else NA_real_,
      a12 = if(!is.null(x1$evaluation$a12)) x1$evaluation$a12 else NA_real_,
      a1score = if(!is.null(x1$evaluation$a1score)) x1$evaluation$a1score else NA_real_,
      a3score = if(!is.null(x1$evaluation$a3score)) x1$evaluation$a3score else NA_real_,
      aI = if(!is.null(x1$evaluation$aI)) x1$evaluation$aI else NA_real_,
      aII = if(!is.null(x1$evaluation$aII)) x1$evaluation$aII else NA_real_,
      ci10 = if(!is.null(x1$evaluation$ci10)) x1$evaluation$ci10 else NA_real_,
      cimax = if(!is.null(x1$evaluation$cimax)) x1$evaluation$cimax else NA_real_,
      hIndex = if(!is.null(x1$evaluation$hIndex)) x1$evaluation$hIndex else NA_real_,
      project_id = project_id,
      stringsAsFactors = FALSE
    )
    
    return(rezultat)
    
  }, error = function(e) {
    message(paste("Napaka pri programu", project_id, ":", e$message))
    return(list(
      project_id = project_id,
      project_data = data.frame(
        project_id = project_id,
        id = NA_character_,
        remark = NA_character_,
        id_prog = NA_character_,
        counter = NA_integer_,
        zacetek = NA_character_,
        konec = NA_character_,
        stringsAsFactors = FALSE
      ),
      researchers = data.frame(
        raz = NA_character_,
        vloga = NA_character_,
        org = NA_integer_,
        id_raz = NA_integer_,
        st_raz = NA_character_,
        project_id = project_id,
        stringsAsFactors = FALSE
      )[0, ],
      evaluation = data.frame(
        a11 = NA_real_,
        a12 = NA_real_,
        a1score = NA_real_,
        a3score = NA_real_,
        aI = NA_real_,
        aII = NA_real_,
        ci10 = NA_real_,
        cimax = NA_real_,
        hIndex = NA_real_,
        project_id = project_id,
        stringsAsFactors = FALSE
      )
    ))
  })
}


# ==========================================
# NASTAVITEV PARALELNEGA PROCESIRANJA
# ==========================================

num_cores <- detectCores()


if (num_cores > 16) {
  cores_to_use <- floor(num_cores * 0.8)
} else if (num_cores >= 8) {
  cores_to_use <- num_cores - 2
} else if (num_cores >= 4) {
  cores_to_use <- num_cores - 1
} else {
  cores_to_use <- max(1, num_cores)
}


# ==========================================
# PARALELNI IZVOZ PODATKOV
# ==========================================


# Ustvarimo cluster
cl <- makeCluster(cores_to_use)

# Izvozimo jwt_token IN funkcijo
clusterExport(cl, c("jwt_token", "pridobi_podatke_projekta"), envir = environment())

# Naložimo pakete v cluster
clusterEvalQ(cl, {
  library(httr)
  library(jsonlite)
})

# Paralelno procesiranje
vsi_podatki_projekti <- parLapplyLB(cl, proj_ids, function(x) {
  pridobi_podatke_projekta(x)
})

stopCluster(cl)


# ==========================================
# PRIPRAVA KONČNIH TABEL
# ==========================================



# ------------------------------------------------------------------
# TABELA PROJEKTOV (organizacije)
t_final <- vsi_podatki_projekti %>%
  map("project_data") %>%
  compact() %>%
  bind_rows()

# Projekti, kjer je UKCL nosilec
ukc <- t_final %>% 
  separate(col = id, into = c("id1", "id2"), remove = FALSE,
           extra = "merge") %>% 
  filter(id1 == 618) %>% 
  select(project_id, remark, id_proj, counter, zacetek, konec)
# -----------------------------------------------------------------

# -----------------------------------------
# TABELA RAZISKOVALCEV
t_final_researchers <- vsi_podatki_projekti %>%
  map("researchers") %>%
  compact() %>%
  bind_rows()
# -----------------------------------------


# -----------------------------------------
# TABELA TOČKE 
t_tocke_proj_aris <- vsi_podatki_projekti %>%
  map("evaluation") %>%
  compact() %>%
  bind_rows()

# -----------------------------------------


# ===================================
# Združimo v skupno tabelo
# ===================================

proj %>% 
  select(sicris_id_proj = id,
         counter, code, name,
         sicris_id_raz = rsrid, st_raz = rsrCode, raz = resaercherFullName) %>% 
  left_join(
    t_final %>% 
      select(sicris_id_proj = project_id,
             org_sifra = id, org = remark,
             zacetek, konec)
  ) %>% 
  left_join(
    t_tocke_proj_aris %>% rename(sicris_id_proj = project_id) 
  ) -> aris_proj



#---------------------------------------
# Shranimo podatke za kasnejšo uporabo
# --------------------------------------


saveRDS(aris_proj, "./data/aris_projekti.RDS")


saveRDS(t_final_researchers, "./data/aris_proj_raziskovalci.RDS")


#--------------------------------------------------------
