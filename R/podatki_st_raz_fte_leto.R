
# ======================================================
# PRIRPAVA PODATKOV O ŠT. RAZISKOVALCEV IN FTH PO LETIH
# ======================================================


# ------------------------------------------
# funkcija: Ustvari povzetek raziskovalcev po letu iz celotnih podatkih o raziskovalcih (vloga == "Raziskovalec")
# ------------------------------------------

#' Ustvari povzetek raziskovalcev po letu
#'
#' @description
#' Funkcija ustvari agregirani povzetek podatkov o raziskovalcih, pri čemer
#' izračuna število raziskovalcev in skupni FTE (Full-Time Equivalent) ter
#' avtomatsko določi leto iz imena vhodnega objekta.
#'
#' @param data Data frame z raziskovalnimi podatki, ki mora vsebovati stolpec:
#' \itemize{
#'   \item \code{del_zap} - Delež zaposlitve v odstotkih (npr. 100 za polni delovni čas)
#' }
#'
#' @return Data frame z eno vrstico in tremi stolpci:
#' \itemize{
#'   \item \code{st_raz} - Število raziskovalcev (število vrstic v vhodnih podatkih)
#'   \item \code{fte} - Skupni FTE (Full-Time Equivalent), izračunan kot vsota del_zap/100
#'   \item \code{leto} - Leto, ekstrahirano iz imena vhodnega objekta (vse številke iz imena)
#' }
#'
#' @details
#' Funkcija avtomatsko ekstrahira leto iz imena objekta, ki ji je podan kot argument.
#' To omogoča enostavno procesiranje več data frame-ov z imeni kot npr. 
#' \code{raziskovalci_2023}, \code{raz2024}, \code{df_2025} itd.
#' 
#' Proces ekstrakcije leta:
#' \enumerate{
#'   \item Z \code{deparse(substitute(data))} dobi ime objekta kot niz znakov
#'   \item Z \code{gsub("[^0-9]", "", ime)} odstrani vse ne-številske znake
#'   \item Preostale številke pretvori v numerično vrednost
#' }
#' 
#' FTE izračun:
#' \itemize{
#'   \item Delež zaposlitve je shranjen v odstotkih (npr. 100 = 100%)
#'   \item FTE = vsota(del_zap) / 100
#'   \item Manjkajoče vrednosti (NA) so izključene iz izračuna
#' }
#'
#' @note
#' Uporablja naslednje pakete:
#' \itemize{
#'   \item \strong{dplyr} - \code{summarise()}, \code{\%>\%}
#'   \item \strong{base R} - \code{deparse()}, \code{substitute()}, \code{gsub()}, \code{as.numeric()}
#' }
#'
#' @section Opozorila:
#' \itemize{
#'   \item Ime objekta MORA vsebovati številke, da funkcija pravilno deluje
#'   \item Če ime vsebuje več številk (npr. "raz_2023_v2"), bodo vse združene ("20232")
#'   \item Stolpec \code{del_zap} mora biti številski in v odstotkih
#' }
#'
#' @examples
#' \dontrun{
#' # Priprava testnih podatkov
#' raziskovalci_2023 <- data.frame(
#'   ime = c("Janez Novak", "Marija Horvat", "Peter Kovač"),
#'   del_zap = c(100, 50, 75)
#' )
#' 
#' # Ustvari povzetek
#' povzetek_raz(raziskovalci_2023)
#' # Vrne: st_raz = 3, fte = 2.25, leto = 2023
#' 
#' # Primer z drugo konvencijo poimenovanja
#' raz2024 <- data.frame(
#'   ime = c("Ana Novak", "Tone Zupan"),
#'   del_zap = c(100, 100)
#' )
#' 
#' povzetek_raz(raz2024)
#' # Vrne: st_raz = 2, fte = 2, leto = 2024
#' 
#' # Uporaba z več leti
#' leta <- c(2020, 2021, 2022, 2023, 2024)
#' 
#' # Predpostavljamo, da imamo objekte: raz_2020, raz_2021, ...
#' vsi_povzetki <- lapply(
#'   paste0("raz_", leta),
#'   function(x) povzetek_raz(get(x))
#' ) %>% bind_rows()
#' }
#'
#' @export

povzetek_raz <- function(data) {
  ime <- deparse(substitute(data))
  stevilka <- as.numeric(gsub("[^0-9]", "", ime))
  
  data %>%
    summarise(
      st_raz = n(),
      fte = sum(del_zap, na.rm = TRUE)/100,
      leto = stevilka
    )
}

# ---------------------------------
# Združitev podatkov v  eno tabelo
# ---------------------------------


bind_rows(
  povzetek_raz(raz23),
  povzetek_raz(raz24),
  povzetek_raz(raz25)
) %>% 
  mutate(fte = round(fte, 1)) -> t_fte


## Dopolnimo za pretekla leta, za katera nimamo podatkov

t_fte %>% 
  bind_rows(data.frame(                             
    st_raz = t_fte$st_raz[t_fte$leto == 23],                ## Uporabimo najstarejše podatke kar imamo
    fte = t_fte$fte[t_fte$leto == 23],
    leto = c(19, 20, 21, 22)
  )) %>% 
  arrange(leto) %>% 
  mutate(leto = paste0("20", leto)) -> raz_fte_leto




