

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



# ==========================================
# ZDRUŽENA FUNKCIJA - en GET klic za vse podatke
# ==========================================


  
url <- "https://cris.cobiss.net/ecris/si/sl/service/researcher/search?query=*&limit=ALL"
  

a <- GET(url, add_headers(Authorization = jwt_token))

vsi_raz <- fromJSON(content(a, 'text'))
    
    
id_raz <- vsi_raz$id


# ==========================================
# OPTIMIZIRAN IZVOZ - RAZISKOVALCI (researchload)
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
# ENOSTAVNA FUNKCIJA
# ==========================================

pridobi_podatke_vseh_raz <- function(id_raz) {
  
  url <- paste0("https://cris.cobiss.net/ecris/si/sl/service/researcher/", id_raz)
  
  tryCatch({
    a <- GET(url, add_headers(Authorization = jwt_token))
    x1 <- fromJSON(content(a, 'text'))
    
    rezultat <- data.frame(
      sicris_id_raz = id_raz,                                                                ## sicris_id_raz
      researchload = if(!is.null(x1$employs) && "researchload" %in% names(x1$employs)) {     ## Delež zaposlitve
        x1$employs$researchload[1]
      } else NA_real_,
      org = if(!is.null(x1$employs) && "orgName" %in% names(x1$employs)) {                   ## Organizacija kjer je zaposlen
        x1$employs$orgName[1]
      } else NA,
      org_id = if(!is.null(x1$employs) && "orgCode" %in% names(x1$employs)) {                ## Organizacija šifra
        x1$employs$orgCode[1]
      } else NA,
      vloga = x1$type,
      stringsAsFactors = FALSE
    )
    
    return(rezultat)
    
  }, error = function(e) {
    return(data.frame(
      sicris_id_raz = id_raz,
      researchload = NA_real_,
      stringsAsFactors = FALSE
    ))
  })
}

# ==========================================
# PARALELNI IZVOZ PODATKOV
# ==========================================


start_time <- Sys.time()

cl <- makeCluster(cores_to_use)
clusterExport(cl, c("jwt_token", "pridobi_podatke_vseh_raz"), envir = environment())
clusterEvalQ(cl, {
  library(httr)
  library(jsonlite)
})

vsi_podatki_raz <- parLapplyLB(cl, id_raz, function(x) {
  pridobi_podatke_vseh_raz(x)
})

stopCluster(cl)

end_time <- Sys.time()

# ==========================================
# PRIPRAVA KONČNE TABELE
# ==========================================

tabela_researchload <- bind_rows(vsi_podatki_raz)


#---------------------------------------------

saveRDS(tabela_researchload, "./data/vsi_raziskovalci_del_zap.RDS")
