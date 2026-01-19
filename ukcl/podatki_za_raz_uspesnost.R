

# ==================================================
# Izvoz podatkov o raziskovalni uspešnosti po letih
# =================================================

# --------------------------------------------
# Priprava podatkov o št. raz in FTE po letih
# ---------------------------------------------



# -------------------------------------------
# Uvoz vseh datotek v mapi "uspesnost_HTML"
# -------------------------------------------

# V mapo se shranijo HTML datoteke iz sicrisa
# v Sicrisu -> Vrednotenje bibliografskih kazalcev
# Izbere se obdobje preteklih 5 let

# Pot do HTML datotek
html_folder <- "./uspesnost_HTML/"

# Linki do vseh datotek v mapi, ki se končajo z .html
html_files <- list.files(html_folder, pattern = "\\.html$", full.names = TRUE)


# ------------------------------------------------------
# Izvoz podatkov o A1 točkah (upoštevanje točke, A"...)
#------------------------------------------------------

## FUNKCIJA ZA UREJANJE TABELE - prva vrstica = header

#' Uporabi prvo vrstico kot imena stolpcev
#'
#' @description
#' Funkcija pretvori prvo vrstico data frame-a v imena stolpcev.
#' Uporabno pri branju podatkov, kjer so imena stolpcev del podatkovne tabele
#' namesto v glavi tabele.
#'
#' @param df Data frame, katerega prvo vrstico želimo uporabiti kot imena stolpcev
#'
#' @return Data frame brez prve vrstice, z novimi imeni stolpcev iz prve vrstice.
#'   Če se imena podvajajo, funkcija doda pripono "_1", "_2" itd.
#'
#' @details
#' Funkcija:
#' \itemize{
#'   \item Ekstrahira vrednosti iz prve vrstice in jih pretvori v znake (character)
#'   \item Odstrani prvo vrstico iz data frame-a
#'   \item Nastavi nova imena stolpcev
#'   \item Preveri za podvojena imena in jih po potrebi naredi unikatna z dodajanjem "_1", "_2" itd.
#' }
#'
#' Če ima data frame le eno vrstico, funkcija vrne originalni data frame brez sprememb.
#'
#' @note
#' Uporablja funkcije iz base R paketa (ni potrebe po dodatnih paketih):
#' \itemize{
#'   \item \code{as.character()} - pretvorba v znake
#'   \item \code{duplicated()} - preverjanje podvojenih vrednosti
#'   \item \code{make.unique()} - ustvarjanje unikatnih imen
#' }
#'
#' @examples
#' # Primer z normalnimi podatki
#' df <- data.frame(
#'   V1 = c("Ime", "Janez", "Marija"),
#'   V2 = c("Priimek", "Novak", "Horvat"),
#'   V3 = c("Starost", "25", "30")
#' )
#' use_first_row_as_header(df)
#' # Vrne data frame z imeni stolpcev: "Ime", "Priimek", "Starost"
#'
#' # Primer s podvojenimi imeni
#' df2 <- data.frame(
#'   V1 = c("Leto", "2023", "2024"),
#'   V2 = c("Leto", "50", "60"),
#'   V3 = c("Vrednost", "100", "120")
#' )
#' use_first_row_as_header(df2)
#' # Vrne data frame z imeni stolpcev: "Leto", "Leto_1", "Vrednost"
#'
#' @export

use_first_row_as_header <- function(df) {
  if (nrow(df) > 1) {
    # Extract column names from first row
    new_names <- as.character(df[1, ])
    
    # Remove first row and set new column names
    df <- df[-1, ]
    names(df) <- new_names
    
    # Check for duplicate names and fix them
    if (any(duplicated(names(df)))) {
      names(df) <- make.unique(names(df), sep = "_")
    }
  }
  return(df)
}


## FUNKCIIJA ZA EKSTRAHIRANJE PODATKOV IZ TABELE IZ HTML-JA 
## prvi del tabele - točke v tem obdobju

#' Ekstrahiraj podatke o točkah iz HTML datoteke
#'
#' @description
#' Funkcija prebere HTML datoteko z raziskovalnimi podatki in ekstrahira:
#' \itemize{
#'   \item Šifro raziskovalne skupine iz H1 naslova (besedilo znotraj [ ])
#'   \item Leto iz H2 naslova (besedilo znotraj ( ))
#'   \item Tretjo tabelo iz HTML-ja (vrstice 1-5), pri čemer uporabi prvo vrstico kot imena stolpcev
#' }
#'
#' @param html_file Pot do HTML datoteke, ki vsebuje raziskovalne podatke
#'
#' @return Data frame z naslednjimi stolpci:
#' \itemize{
#'   \item \code{sifra_skupine} - Šifra raziskovalne skupine (ekstrahirana iz H1)
#'   \item \code{leto} - Leto podatkov (ekstrahirano iz H2)
#'   \item \code{source_file} - Ime izvorne HTML datoteke
#'   \item Ostali stolpci iz tretje tabele (imena iz prve vrstice)
#' }
#'
#' @details
#' Funkcija izvede naslednje korake:
#' \enumerate{
#'   \item Prebere HTML datoteko
#'   \item Iz H1 naslova z regularnim izrazom ekstrahira šifro (besedilo med [ in ])
#'   \item Iz H2 naslova z regularnim izrazom ekstrahira leto (besedilo med ( in ))
#'   \item Ekstrahira tretjo tabelo iz HTML dokumenta
#'   \item Obdela podvojena imena stolpcev (če obstajajo)
#'   \item Vzame vrstice 1-5 iz tabele
#'   \item Uporabi funkcijo \code{use_first_row_as_header()} za pretvorbo prve vrstice v imena stolpcev
#'   \item Doda metadata stolpce (šifra, leto, ime datoteke)
#' }
#'
#' @note
#' Uporablja naslednje pakete:
#' \itemize{
#'   \item \strong{rvest} - \code{read_html()}, \code{html_node()}, \code{html_text()}, \code{html_table()}
#'   \item \strong{stringr} - \code{str_extract()}
#'   \item \strong{dplyr} - \code{slice()}, \code{mutate()}
#'   \item \strong{base R} - \code{make.unique()}, \code{basename()}
#' }
#'
#' @section Predpostavke:
#' \itemize{
#'   \item HTML mora vsebovati H1 z besedilom v obliki "... [ŠIFRA] ..."
#'   \item HTML mora vsebovati H2 z besedilom v obliki "... (LETO) ..."
#'   \item HTML mora vsebovati vsaj 3 tabele
#'   \item Tretja tabela mora imeti vsaj 5 vrstic
#'   \item Funkcija \code{use_first_row_as_header()} mora biti na voljo
#' }
#'
#' @examples
#' \dontrun{
#' # Ekstrahiraj podatke iz ene datoteke
#' tocke_df <- extract_tocke("podatki/skupina_P1-0123_2024.html")
#' 
#' # Obdelaj več datotek
#' html_files <- list.files("podatki", pattern = "\\.html$", full.names = TRUE)
#' vsi_podatki <- lapply(html_files, extract_tocke) %>% 
#'   bind_rows()
#' }
#'
#' @seealso \code{\link{use_first_row_as_header}}
#'
#' @export
extract_tocke <- function(html_file) {
  
  # Read HTML
  html <- read_html(html_file)
  
  # Extract group code from H1
  h1_text <- html %>% 
    html_node("body h1") %>% 
    html_text()
  
  sifra <- str_extract(h1_text, "(?<=\\[).+?(?=\\])")
  
  # Extract year from H2
  h2_text <- html %>% 
    html_node("body h2") %>% 
    html_text()
  
  leto <- str_extract(h2_text, "(?<=\\().+?(?=\\))")
  
  # Extract TRETJO TABELO
  table3_raw <- html %>% 
    html_table(fill = TRUE) %>% 
    .[[3]]
  
  # Fix duplicate column names in raw table
  if (any(duplicated(names(table3_raw)))) {
    names(table3_raw) <- make.unique(names(table3_raw), sep = "_")
  }
  
  # Create first slice (rows 1-5) and use row 1 as header
  table3a <- table3_raw %>% 
    slice(1:5) %>% 
    use_first_row_as_header() %>% 
    mutate(
      sifra_skupine = sifra,
      leto = leto,
      source_file = basename(html_file),
      .before = 1
    )
  
  return(table3a)
}


## Prirpava tabele iz vseh HTML-jev

# Tabela točke po letih
tocke_po_letih <- map_dfr(html_files, extract_tocke) %>% 
  select(2, 4, 5) 

colnames(tocke_po_letih) <- c(
  "obd",
  "objave",
  "tocke"
)

## Prirpava tabele za točke za kazalnike po letih

tabela_kaz_tocke_ukcl <- tocke_po_letih %>% 
  mutate(tocke = as.numeric(tocke)) %>% 
  separate(obd, c("od", "leto"), sep = "-", remove = FALSE) %>% 
  select(-od) %>% 
  left_join(
    raz_fte_leto,
    by = "leto"
  ) %>% 
  mutate(kaz_fte = round(tocke/fte, 1),
         kaz_raz = round(tocke/st_raz, 1))
  





