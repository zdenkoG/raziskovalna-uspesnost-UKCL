


#------------------------------------------------------------------------------------

####################################################################
##############
## POVEZAVA NA SICRIS PREKO API
##############

## JWT žeton

# URL za pridobitev JWT žetona
token_url <- "https://cris.cobiss.net/ecris/si/sl/service/getjwt"

# Podatki za pošiljanje v POST zahtevo (npr. uporabniško ime in geslo)
credentials <- list(
  username = Sys.getenv("API_USERNAME"),
  password = Sys.getenv("API_PASSWORD")
)



# Izvedba POST zahteve za pridobitev JWT žetona
token_response <- POST(
  url = token_url,
  body = credentials,
  encode = "json"
)


token_data <- fromJSON(content(token_response, "text"))

# Pridobivanje JWT žetona
jwt_token <- token_data$jwt


#-----------------------------------------------------------
#-----------------------------------------------------------


####################################################################
##############
## POVEZAVA NA SICRIS PREKO API - PODATKI ZA CELOTEN UKCL
##############
####################################################################


## izberemo vse podatke iz sicris - ukcl strani

url <- "https://cris.cobiss.net/ecris/si/sl/service/organization/618"      ## 618 je UKCL

a <- GET(url, add_headers(Authorization = jwt_token))


## dobimo seznam vseh podatkov iz spletne strani UKCL 

x <-  fromJSON(content(a, "text")) 


#-----------------------------------------


############################################
## prirpava tabela s podatki za celoten ukcl
#############################################


ukcl_podatki <- 
  as_tibble(x[["evaluation"]]) %>%                                              ## Točke za celoten UKCL
  mutate(org = "618",
         st_razisk = nrow(x[["researchers"]]),                                  # št raziskovalcev
         st_skupin = nrow(x[["groups"]]),                                       # št. skupin
         st_projek = nrow(x[["projects"]]),                                     # št projektov ARIS
         st_progra = nrow(x[["programs"]]),                                     # št programov ARIS
         st_mr_men = nrow(x[["mentors"]]),                                      # št mentorjev mladim raziskovalcem
         .before = a11) %>% 
  bind_cols(as_tibble(x[["cit"]]) %>%                                           # Dodamo podatke o citiranosti (Wos - Scopus)
              slice(1) %>% 
              select(-1))


#------------------------------------------------------------

saveRDS(ukcl_podatki, "./data/ukcl_podatki.RDS")


#-----------------------------------------------------------
