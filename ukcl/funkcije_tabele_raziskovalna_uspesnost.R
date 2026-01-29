

# ===================================================
# FUNKCIJE ZA IZPIS TABEL PRI RAZISKOVALNI USPEŠNOSTI
# ===================================================

# ------------------------------------------------------------------------------------
# Preimenujemo spremnljivke v prej prirpavljenih tabelah - podatki_za_raz_uspesnost.R
# ------------------------------------------------------------------------------------

## Točke

colnames(up_tocke_obd) <- c(
  "Obdobje",
  "Leto",
  "objave",
  "Št. upotevanih točk",
  "Št. raziskovalcev",
  "FTE",
  "Točke/FTE",
  "Točke/raziskovalec"
)

colnames(a2_obd) <- c(
  "Obdobje",
  "Leto",
  "objave",
  "A'' točke",
  "Št. raziskovalcev",
  "FTE",
  "Točke/FTE",
  "Točke/raziskovalec"
)


colnames(a1_obd) <- c(
  "Obdobje",
  "Leto",
  "objave",
  "A' točke",
  "Št. raziskovalcev",
  "FTE",
  "Točke/FTE",
  "Točke/raziskovalec"
)


colnames(a1_2_obd) <- c(
  "Obdobje",
  "Leto",
  "objave",
  "A1/2 točke",
  "Št. raziskovalcev",
  "FTE",
  "Točke/FTE",
  "Točke/raziskovalec"
)

## Citati

colnames(ci10) <- c(
  "Obdobje",
  "Leto",
  "objave",
  "Čisti citati",
  "Št. raziskovalcev",
  "FTE",
  "Citati/FTE",
  "Citati/raziskovalec"
)

## Najodmevnejše delo in h-indeks

colnames(cimax_hind) <- c(
  "Obdobje",
  "Leto",
  "objave",
  "Najodmevnejše delo",
  "H-indeks",
  "Št. raziskovalcev",
  "FTE"
)


#--------------------------------------------------------------

# -------------------------
# Izpis tabele po obdobjih
# ----------------------------


#' Izpiši interaktivno tabelo raziskovalne uspešnosti po obdobjih
#'
#' @description
#' Funkcija ustvari interaktivno tabelo z raziskovalnimi točkami po obdobjih
#' z uporabo paketa reactable. Tabela prikazuje točke v številčnem formatu
#' (slovenska notacija) in skriva stolpec z objavami.
#'
#' @param data Data frame z raziskovalnimi točkami, ki mora vsebovati:
#' \itemize{
#'   \item \code{objave} - Stolpec z objavami (bo skrit v izpisu)
#'   \item Ostali stolpci - Različne vrste raziskovalnih točk (A', A'', A1/2, itd.)
#'   \item Vrstice običajno predstavljajo različna obdobja/leta
#' }
#'
#' @return Reactable objekt - interaktivna HTML tabela
#'
#' @details
#' Funkcija ustvari standardizirano tabelo za prikaz raziskovalne uspešnosti z
#' naslednjimi lastnostmi:
#' 
#' \strong{Formatiranje:}
#' \itemize{
#'   \item Vse vrednosti so centrirane
#'   \item Številske vrednosti so formatirane v slovenski notaciji (pika za tisočice, vejica za decimale)
#'   \item Velikost pisave v glavi: 14px
#' }
#' 
#' \strong{Stolpci:}
#' \itemize{
#'   \item Stolpec \code{objave} je skrit (show = FALSE)
#'   \item Vsi ostali stolpci uporabljajo funkcijo \code{format_numeric_column} za prikaz
#' }
#' 
#' Funkcija je del sistema za prikaz raziskovalnih točk in omogoča konsistenten
#' izgled tabel ne glede na vrsto točk (A', A'', A1/2, itd.).
#'
#' @note
#' Uporablja naslednje pakete:
#' \itemize{
#'   \item \strong{reactable} - \code{reactable()}, \code{colDef()}
#' }
#' 
#' Zahteva tudi:
#' \itemize{
#'   \item Funkcijo \code{format_numeric_column} za formatiranje številskih vrednosti
#' }
#'
#' @section Tipična uporaba:
#' Funkcija se uporablja za prikaz različnih vrst raziskovalnih točk:
#' \itemize{
#'   \item Upoštevane točke
#'   \item Točke A'' 
#'   \item Točke A' 
#'   \item Točke A1/2 
#' }
#'
#' @examples
#' \dontrun{
#' # Prikaz tabele z vsemi upoštevanimi točkami
#' izpis_tab_razUsp_obd(up_tocke_obd)  
#'}
#' @seealso 
#' \code{\link{format_numeric_column}} za formatiranje številskih vrednosti
#'
#' @export
izpis_tab_razUsp_obd <- function(data){
  reactable(
    data,
    columns = list(
      objave = colDef(show = FALSE)
    ),
    defaultColDef = colDef(
      align = "center",
      cell = format_numeric_column,
      headerStyle = list(fontSize = "14px")
    )
  )
}





