


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
html_folder <- "./UKCL_HTML/"

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


#' Ekstrahiraj podatke o točkah iz HTML datoteke
#'
#' @description
#' Funkcija prebere HTML datoteko z raziskovalnimi podatki in ekstrahira:
#' \itemize{
#'   \item Šifro raziskovalne skupine iz H1 naslova (besedilo znotraj [ ])
#'   \item Leto iz H2 naslova (besedilo znotraj ( ))
#'   \item Četrto tabelo iz HTML-ja, pri čemer uporabi prvo vrstico kot imena stolpcev
#' }
#'
#' @param html_file Pot do HTML datoteke, ki vsebuje raziskovalne podatke
#' @param which_slice Kateri slice tabele naj ekstrahira. Možnosti:
#' \itemize{
#'   \item \code{"first"} - Vrstice 1-5 (privzeto)
#'   \item \code{"second"} - Vrstice 7-10
#' }
#'
#' @return Data frame z naslednjimi stolpci:
#' \itemize{
#'   \item \code{sifra_skupine} - Šifra rorganizacije (ekstrahirana iz H1)
#'   \item \code{leto} - Leto podatkov (ekstrahirano iz H2)
#'   \item \code{source_file} - Ime izvorne HTML datoteke
#'   \item \code{slice_type} - Tip slice-a ("first" ali "second")
#'   \item Ostali stolpci iz tretje tabele (imena iz prve vrstice)
#' }
#'
#' @details
#' Funkcija izvede naslednje korake:
#' \enumerate{
#'   \item Prebere HTML datoteko
#'   \item Iz H1 naslova z regularnim izrazom ekstrahira šifro (besedilo med [ in ])
#'   \item Iz H2 naslova z regularnim izrazom ekstrahira leto (besedilo med ( in ))
#'   \item Ekstrahira četrto tabelo iz HTML dokumenta
#'   \item Obdela podvojena imena stolpcev (če obstajajo)
#'   \item Glede na parameter \code{which_slice} vzame vrstice 1-5 ali 7-10
#'   \item Uporabi funkcijo \code{use_first_row_as_header()} za pretvorbo prve vrstice v imena stolpcev
#'   \item Doda metadata stolpce (šifra, leto, ime datoteke, tip slice-a)
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
#'   \item Tretja tabela mora imeti vsaj 5 vrstic (za first slice) ali 10 vrstic (za second slice)
#'   \item Funkcija \code{use_first_row_as_header()} mora biti na voljo
#' }
#'
#' @examples
#' \dontrun{
#' # Ekstrahiraj prvi slice (vrstice 1-5) iz ene datoteke
#' tocke_first <- extract_tocke("podatki/skupina_P1-0123_2024.html", which_slice = "first")
#' 
#' # Ekstrahiraj drugi slice (vrstice 7-10) iz ene datoteke
#' tocke_second <- extract_tocke("podatki/skupina_P1-0123_2024.html", which_slice = "second")
#' 
#' # Obdelaj več datotek - oba slice-a
#' html_files <- list.files("podatki", pattern = "\\.html$", full.names = TRUE)
#' 
#' library(purrr)
#' library(dplyr)
#' 
#' vsi_podatki_first <- map_dfr(html_files, ~extract_tocke(.x, which_slice = "first"))
#' vsi_podatki_second <- map_dfr(html_files, ~extract_tocke(.x, which_slice = "second"))
#' 
#' # Ali združi oba slice-a skupaj
#' vsi_podatki <- bind_rows(
#'   map_dfr(html_files, ~extract_tocke(.x, which_slice = "first")),
#'   map_dfr(html_files, ~extract_tocke(.x, which_slice = "second"))
#' )
#' }
#'
#' @seealso \code{\link{use_first_row_as_header}}
#'
#' @export
extract_tocke <- function(html_file, which_slice = "tocke") {
  
  # Validate which_slice parameter
  if (!which_slice %in% c("tocke", "citati")) {
    stop("Parameter 'which_slice' mora biti 'tocke' ali 'citati'")
  }
  
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
  table4_raw <- html %>% 
    html_table(fill = TRUE) %>% 
    .[[4]]
  
  # Fix duplicate column names in raw table
  if (any(duplicated(names(table4_raw)))) {
    names(table4_raw) <- make.unique(names(table4_raw), sep = "_")
  }
  
  # Določi kateri slice uporabiti
  if (which_slice == "tocke") {
    slice_rows <- 1:5
  } else {  # which_slice == "second"
    slice_rows <- 7:10
  }
  
  # Create slice and use first row as header
  table4_slice <- table4_raw %>% 
    slice(slice_rows) %>% 
    use_first_row_as_header() %>% 
    mutate(
      sifra_skupine = sifra,
      leto = leto,
      source_file = basename(html_file),
      slice_type = which_slice,  # Dodaj info o tem kateri slice je
      .before = 1
    )
  
  return(table4_slice)
}

#-----------------------------------------
#-----------------------------------------

## Prirpava tabele iz vseh HTML-jev

# ---------------------------
# Tabela točke po letih
#----------------------------

tocke_po_letih <- map_dfr(html_files, ~extract_tocke(.x, which_slice = "tocke")) %>% 
  select(2,5, 6) 


colnames(tocke_po_letih) <- c(
  "leto",
  "objave",
  "tocke"
)

## Prirpava tabele za točke za KAZALNIKE po letih

tabela_kaz_ukcl_leto <- tocke_po_letih %>% 
  mutate(tocke = as.numeric(tocke)) %>% 
  left_join(
    raz_fte_leto,
    by = "leto"
  ) %>% 
  arrange(desc(leto)) %>% 
  fill(st_raz, fte) %>% 
  mutate(kaz_fte = round(tocke/fte, 1),
         kaz_raz = round(tocke/st_raz, 1)) %>% 
  arrange(desc(leto))



#----------------------------------------------------


# ---------------------------------------------------------
# Prirpava tabel za izpise pred grafi - po obdobjih - TOČKE
# ---------------------------------------------------------

## Upoštevane točke

tabela_kaz_ukcl_leto %>% 
  filter(objave == "Upoštevane točke") %>% 
  arrange(desc(leto)) -> up_tocke_leto


## Izjemni dosežki - A''

tabela_kaz_ukcl_leto %>% 
  filter(objave == "A'' - izjemni dosežki") %>% 
  arrange(desc(leto)) -> a2_leto


## Kvalitetni dosežki

tabela_kaz_ukcl_leto %>% 
  filter(objave == "A' - zelo kvalitetni dosežki") %>% 
  arrange(desc(leto)) -> a1_leto

## Pomembni dosežki

tabela_kaz_ukcl_leto %>% 
  filter(objave == "A1/2 - pomembni dosežki") %>% 
  arrange(desc(leto)) -> a1_2_leto


