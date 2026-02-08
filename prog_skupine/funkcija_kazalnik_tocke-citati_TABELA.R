
#------------------------------------------------------------------
## Funkcije za izpis tabel - programi: raziskovalna uspešnost ##
# Funkcija za tabele s kazalniki (vrstica 8)
# Funkcija za tabele z abs št (CImax in H-indeks) (vrstica 125)
#-----------------------------------------------------------------

# =============================================================
# FUNKCIJA: Izpis podatkov o raziskovalni uspešnosti programov
# s crosstalk filtrom
# =============================================================

#' Izpiši interaktivno tabelo raziskovalne uspešnosti programskih skupin s filtrom
#'
#' @description
#' Funkcija ustvari interaktivno tabelo z raziskovalno uspešnostjo programskih
#' skupin z uporabo paketa reactable in crosstalk filtrom za vodilno organizacijo.
#'
#' @param data Data frame s podatki o programskih skupinah, ki mora vsebovati
#'   9 stolpcev v naslednjem vrstnem redu:
#' \itemize{
#'   \item Šifra programske skupine
#'   \item Vodja programske skupine
#'   \item Naslov programske skupine
#'   \item Število raziskovalcev
#'   \item FTE (Full-Time Equivalent)
#'   \item Točke (upoštevane/A'/A''/A1/2 - odvisno od parametra \code{kaj})
#'   \item Kazalnik 1 (točke na FTE - odvisno od parametra \code{kaz1})
#'   \item Kazalnik 2 (točke na raziskovalca - odvisno od parametra \code{kaz2})
#'   \item Indikator ali je UKCL vodilna organizacija (1 = Da, 0 = Ne)
#' }
#'
#' @param kaj Naziv vrste točk za prikaz v glavi stolpca.
#'   Privzeto: "Upoštevane točke"
#' @param kaz1 Naziv prvega kazalnika za prikaz v glavi stolpca.
#'   Privzeto: "Točke/FTE"
#' @param kaz2 Naziv drugega kazalnika za prikaz v glavi stolpca.
#'   Privzeto: "Točke/raz"
#'
#' @return Bscols objekt s filtrom in reactable tabelo
#'
#' @note
#' Uporablja naslednje pakete:
#' \itemize{
#'   \item \strong{reactable} - \code{reactable()}, \code{colDef()}
#'   \item \strong{dplyr} - \code{mutate()}, \code{if_else()}, \code{\%>\%}
#'   \item \strong{crosstalk} - \code{SharedData$new()}, \code{filter_checkbox()}
#'   \item \strong{stringr} - \code{str_replace_all()}
#' }
#'
#' @export
izpis_tab_prog_us <- function(data, kaj = "Upoštevane točke", kaz1 = "Točke/FTE", kaz2 = "Točke/raz"){
  
  
  # Preimenuj stolpce
  colnames(data) <- c(
    "Šifra prog",
    "Vodja",
    "Naslov",
    "Št. raz",
    "FTE",
    kaj,
    kaz1,
    kaz2,
    "UKCL vodilna org"
  )
  
  # Pripravi podatke
  kc_prog_num <- data %>%
    mutate(` ` = "", .before = 1) %>%                                         # Prazen stolpec za vrstni red
    mutate(`UKCL vodilna org` = if_else(`UKCL vodilna org` == 1, "Da", "Ne"),
           Vodja = str_replace_all(Vodja, "dr. ", ""))
  
  # Ustvari SharedData objekt za crosstalk
  # Ključni korak: omogoča povezavo med filtrom in tabelo
  shared_data <- SharedData$new(kc_prog_num)
  
  # Ustvari filter checkbox za "UKCL vodilna org"
  filter_widget <- filter_checkbox(
    id = "ukcl_filter",
    label = "UKCL vodilna organizacija",
    sharedData = shared_data,
    group = ~`UKCL vodilna org`,
    inline = TRUE
  )
  
  # Ustvari reactable tabelo
  table_widget <- reactable(
    shared_data,  # Uporabi shared_data namesto kc_prog_num
    columns = list(
      ` ` = colDef(
        name = "#",
        width = 60,
        align = "center",
        sortable = FALSE,
        cell = JS("function(cellInfo) { return cellInfo.viewIndex + 1; }"),
        style = list(color = "#666")
      ),
      Vodja = colDef(align = "left"),
      Naslov = colDef(align = "left", minWidth = 250)
    ),
    defaultColDef = colDef(
      align = "center",
      cell = function(value) suppressWarnings(format_numeric_column(value)),
      headerStyle = list(fontSize = "14px"),
      style = list(fontSize = "14px")
    ),
    striped = FALSE,
    bordered = TRUE,
    highlight = TRUE,
    compact = TRUE,
    searchable = TRUE,
    pagination = FALSE,
    height = 700
  )
  
  # Vrni filter in tabelo v vertikalni postavitvi (filter zgoraj, tabela spodaj)
  tagList(
    div(
      style = "margin-bottom: 15px; margin-left: 80px;",  # Dodaj levi margin za poravnavo
      filter_widget
    ),
    table_widget
  )
}


#---------------------------------------------------------------------------------
#---------------------------------------------------------------------------------


# -------------------------------------------
# FUNKCIJA: najodmevnejše delo - h-indeks
# -------------------------------------------

#' Ustvari interaktivno tabelo za prikaz citatnih kazalnikov programov
#'
#' Funkcija preoblikuje podatke o citatih po programih in ustvari interaktivno
#' tabelo z možnostjo filtriranja po tem, ali je UKCL vodilna organizacija.
#' Uporablja crosstalk za povezavo med filtrom in tabelo.
#'
#' @param cit_prog Data frame s podatki o citatih po programih. Mora vsebovati
#'   9 stolpcev v naslednjem vrstnem redu: šifra programa, vodja, naslov,
#'   število članov, število raziskovalcev, FTE, najodmevnejše delo, H-indeks,
#'   UKCL vodilna organizacija (1/0)
#'
#' @return HTML widget (tagList) s filtrom checkboxa in reactable tabelo
#'
#' @details
#' Funkcija izvede naslednje transformacije:
#' - Preimenuje stolpce v slovenščino
#' - Doda prazen stolpec za vrstni red
#' - Pretvori UKCL vodilna org iz 1/0 v Da/Ne
#' - Odstrani predpono "dr. " iz imen vodij
#' - Ustvari SharedData objekt za crosstalk povezavo
#' - Nastavi formatiranje za numerične stolpce
#' - Omogoča iskanje in filtriranje
#'
#' @note Zahtevani paketi: dplyr, stringr, reactable, crosstalk
#'
#' @examples
#' \dontrun{
#' # Uporaba v Quarto dokumentu
#' tabela_citati_programi(cit_prog)
#' }
#'
#' @export
tabela_citati_programi <- function(cit_prog) {
  
  # Preimenuj stolpce
  colnames(cit_prog) <- c(
    "Šifra prog",
    "Vodja",
    "Naslov",
    "Št. članov",
    "Št. raz",
    "FTE",
    "Najodmevnejše delo",
    "H-indeks",
    "UKCL vodilna org"
  )
  
  # Pripravi podatke
  kc_prog_num <- cit_prog %>%
    mutate(` ` = "", .before = 1) %>%  # Prazen stolpec za vrstni red
    mutate(
      `UKCL vodilna org` = if_else(`UKCL vodilna org` == 1, "Da", "Ne"),
      Vodja = str_replace_all(Vodja, "dr. ", "")
    )
  
  # Ustvari SharedData objekt za crosstalk
  # Ključni korak: omogoča povezavo med filtrom in tabelo
  shared_data <- SharedData$new(kc_prog_num)
  
  # Ustvari filter checkbox za "UKCL vodilna org"
  filter_widget <- filter_checkbox(
    id = "ukcl_filter",
    label = "UKCL vodilna organizacija",
    sharedData = shared_data,
    group = ~`UKCL vodilna org`,
    inline = TRUE
  )
  
  # Ustvari reactable tabelo
  table_widget <- reactable(
    shared_data,  # Uporabi shared_data namesto kc_prog_num
    columns = list(
      ` ` = colDef(
        name = "#",
        width = 60,
        align = "center",
        sortable = FALSE,
        cell = JS("function(cellInfo) { return cellInfo.viewIndex + 1; }"),
        style = list(color = "#666")
      ),
      Vodja = colDef(align = "left"),
      Naslov = colDef(align = "left", minWidth = 250)
    ),
    defaultColDef = colDef(
      align = "center",
      cell = function(value) suppressWarnings(format_numeric_column(value)),
      headerStyle = list(fontSize = "14px"),
      style = list(fontSize = "14px")
    ),
    striped = FALSE,
    bordered = TRUE,
    highlight = TRUE,
    compact = TRUE,
    searchable = TRUE,
    pagination = FALSE,
    height = 700
  )
  
  # Vrni filter in tabelo v vertikalni postavitvi (filter zgoraj, tabela spodaj)
  tagList(
    div(
      style = "margin-bottom: 15px; margin-left: 80px;",  # Dodaj levi margin za poravnavo
      filter_widget
    ),
    table_widget
  )
}