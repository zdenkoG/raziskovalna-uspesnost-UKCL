

# ==============================
# Uvoz podatkov iz REDCap-a
# ==============================

# -------------------------------------------
# Podatki iz baze: Podatki za kazalnike - CKR
# -------------------------------------------

# token za bazo "podatki za kazalnike"
url <- Sys.getenv("API_URL")



formData <- list("token"=Sys.getenv("API_TOKEN_EV_CKR"),
                 content='record',
                 action='export',
                 format='csv',
                 type='flat',
                 csvDelimiter='',
                 'forms[0]'='projekt',
                 rawOrLabel='label',
                 rawOrLabelHeaders='raw',
                 exportCheckboxLabel='false',
                 exportSurveyFields='false',
                 exportDataAccessGroups='false',
                 returnFormat='json'
)
response <- httr::POST(url, body = formData, encode = "form")


## PODATKI

podatki_ckr <- httr::content(response) %>% 
  select(-8)
