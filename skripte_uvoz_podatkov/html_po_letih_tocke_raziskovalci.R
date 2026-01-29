

# -------------------------------------------
# Uvoz vseh datotek v mapi "uspesnost_HTML"
# -------------------------------------------

# V mapo se shranijo HTML datoteke iz sicrisa
# v Sicrisu -> Vrednotenje bibliografskih kazalcev
# Izbere se obdobje preteklih 5 let

# Pot do HTML datotek
html_folder <- "./UKCL_HTML/"

# Linki do vseh datotek v mapi, ki se končajo z .html
html_files <- list.files(html_folder, pattern = "\\.html$", full.names = TRUE)

html_file <- html_files[1]





#' Ekstrahiraj podatke o točkah raziskovalcev iz HTML datoteke
#'
#' @description
#' Funkcija prebere HTML datoteko z raziskovalnimi podatki in ekstrahira:
#' \itemize{
#'   \item Leto iz H2 naslova (besedilo znotraj ( ))
#'   \item Prvo tabelo iz HTML-ja z podatki o raziskovalcih in njihovih točkah
#' }
#'
#' @param html_file Pot do HTML datoteke, ki vsebuje raziskovalne podatke
#'
#' @return Data frame s 11 stolpci:
#' \itemize{
#'   \item \code{leto} - Leto podatkov (ekstrahirano iz H2)
#'   \item Prvih 10 stolpcev iz prve tabele (običajno podatki o raziskovalcih in točkah)
#' }
#'
#' @details
#' Funkcija izvede naslednje korake:
#' \enumerate{
#'   \item Prebere HTML datoteko
#'   \item Iz H2 naslova z regularnim izrazom ekstrahira leto (besedilo med ( in ))
#'   \item Ekstrahira prvo tabelo iz HTML dokumenta
#'   \item Doda stolpec \code{leto} na začetek tabele
#'   \item Obdrži samo prvih 11 stolpcev (leto + 10 stolpcev podatkov)
#' }
#'
#' @note
#' Uporablja naslednje pakete:
#' \itemize{
#'   \item \strong{rvest} - \code{read_html()}, \code{html_node()}, \code{html_text()}, \code{html_table()}
#'   \item \strong{stringr} - \code{str_extract()}
#'   \item \strong{dplyr} - \code{mutate()}, \code{select()}, \code{\%>\%}
#' }
#'
#' @section Predpostavke:
#' \itemize{
#'   \item HTML mora vsebovati H2 z besedilom v obliki "... (LETO) ..."
#'   \item HTML mora vsebovati vsaj 1 tabelo
#'   \item Prva tabela mora vsebovati podatke o raziskovalcih in njihovih točkah
#' }
#'
#' @examples
#' \dontrun{
#' # Ekstrahiraj podatke iz ene datoteke
#' raz_tocke_df <- extract_tocke_raz("podatki/raziskovalci_2024.html")
#' 
#' # Obdelaj več datotek
#' html_files <- list.files("podatki", pattern = "\\.html$", full.names = TRUE)
#' vsi_raz_podatki <- lapply(html_files, extract_tocke_raz) %>% 
#'   bind_rows()
#' }
#'
#' @seealso \code{\link{extract_tocke}} za ekstrakcijo točk raziskovalnih skupin
#'
#' @export
extract_tocke_raz <- function(html_file) {
  
  # Read HTML
  html <- read_html(html_file)
  
  # Extract year from H2
  h2_text <- html %>% 
    html_node("body h2") %>% 
    html_text()
  
  leto <- str_extract(h2_text, "(?<=\\().+?(?=\\))")
  
  # Extract PRVO TABELO
  table1_raw <- html %>% 
    html_table(fill = TRUE) %>% 
    .[[1]]
  
  
  # Create slice and use first row as header
  raz_tocke <- table1_raw %>% 
    mutate(leto = leto,
           .before = 1
    ) %>% 
    select(1:8)
  
  return(raz_tocke)
}



## Združimo v eno tabelo

raziskovalci_leto <- map_dfr(html_files, ~extract_tocke_raz(.x))


#--------------------------------------------------------------------ž


## shranimo
saveRDS(raziskovalci_leto, "./data/raziskovalci_leto.RDS")



  