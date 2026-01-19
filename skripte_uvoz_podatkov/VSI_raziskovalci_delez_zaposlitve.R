

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
r <- fromJSON(content(a, 'text'))
    
    
id_raz <- r$id


# ==========================================
# OPTIMIZIRAN IZVOZ - RAZISKOVALCI (researchload)
# ==========================================


num_cores <- detectCores()
cat("Število razpoložljivih jeder:", num_cores, "\n")

if (num_cores > 16) {
  cores_to_use <- floor(num_cores * 0.8)
} else if (num_cores >= 8) {
  cores_to_use <- num_cores - 2
} else if (num_cores >= 4) {
  cores_to_use <- num_cores - 1
} else {
  cores_to_use <- max(1, num_cores)
}

cat("Uporabljam", cores_to_use, "jeder za procesiranje\n")

# ==========================================
# ENOSTAVNA FUNKCIJA
# ==========================================

pridobi_podatke_vseh_raz <- function(id_raz) {
  
  url <- paste0("https://cris.cobiss.net/ecris/si/sl/service/researcher/", id_raz)
  
  tryCatch({
    a <- GET(url, add_headers(Authorization = jwt_token))
    x1 <- fromJSON(content(a, 'text'))
    
    rezultat <- data.frame(
      sicris_id_raz = id_raz,
      researchload = if(!is.null(x1$employs) && "researchload" %in% names(x1$employs)) {
        x1$employs$researchload[1]
      } else NA_real_,
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

cat("\n===========================================\n")
cat("Uvažam podatke za", length(id_raz), "raziskovalcev\n")
cat("Uporabljam", cores_to_use, "jeder\n")
cat("===========================================\n\n")

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
cas_izvajanja <- round(difftime(end_time, start_time, units = "mins"), 2)
cat("\n✓ Uvoz končan v", cas_izvajanja, "minutah\n")

# ==========================================
# PRIPRAVA KONČNE TABELE
# ==========================================

tabela_researchload <- bind_rows(vsi_podatki_raz)


#---------------------------------------------

saveRDS(tabela_researchload, "./data/vsi_raziskovalci_del_zap.RDS")
