
# =================================================================
# SKUPNE FUNKCIJE - formatiranje številčnih vrednosti v reactable
# =================================================================


# V tej skripti so funkcije za formatiranje števik:
# - brez simbola €
# - finančne podatke s simbolom €
# - odstotke
# - heatmap


# ---------------------------------------------------
# Formatiranje številk pri izpisu - ractable - brez €
# ---------------------------------------------------

#' Formatiraj numerične vrednosti za prikaz v reactable tabelah
#' 
#' Univerzalna funkcija za formatiranje numeric/integer stolpcev v reactable
#' tabelah. Uporablja slovenski zapis števil (vejica za decimale, pika za 
#' tisočice). Cela števila prikaže brez decimalk, decimalna števila pa 
#' zaokroži na 1 decimalno mesto.
#' 
#' @param value Numerična vrednost iz stolpca
#' 
#' @return Character string formatiranega števila ali originalna vrednost 
#'   če ni numerična
#' 
#' @details
#' Formatiranje:
#' - Cela števila: brez decimalk (25 -> "25", 1234 -> "1.234")
#' - Decimalna števila: 1 decimalno mesto (4.567 -> "4,6", 5678.4 -> "5.678,4")
#' - Decimalni separator: vejica (,)
#' - Separator tisočic: pika (.)
#' - Ne-numerične vrednosti ostanejo nespremenjene
#' 
#' @examples
#' # Uporaba v reactable tabeli
#' library(reactable)
#' reactable(
#'   data = tvoji_podatki,
#'   defaultColDef = colDef(
#'     cell = format_numeric_column
#'   )
#' )
#' 
#' # Testni primeri
#' format_numeric_column(25)        # "25"
#' format_numeric_column(4.567)     # "4,6"
#' format_numeric_column(1234)      # "1.234"
#' format_numeric_column(5678.4)    # "5.678,4"
#' format_numeric_column("tekst")   # "tekst"
format_numeric_column <- function(value) {
  # Preveri, če je vrednost numeric ali integer
  if (is.numeric(value)) {
    # Zaokroži na 1 decimalno mesto
    rounded <- round(value, 1)
    
    # Preveri, če je število celo (brez decimalnega dela)
    if (rounded == floor(rounded)) {
      # Za cela števila: prikaži brez decimalk
      formatted <- format(
        rounded,
        big.mark = ".",
        scientific = FALSE,
        trim = TRUE
      )
    } else {
      # Za decimalna števila: prikaži 1 decimalno mesto
      formatted <- format(
        rounded,
        decimal.mark = ",",
        big.mark = ".",
        nsmall = 1,
        trim = TRUE
      )
    }
    
    return(formatted)
  }
  
  # Če ni numeric/integer, vrni vrednost kot je
  return(value)
}



#--------------------------------------------------------------------------------


# ---------------------------------------------------
# Formatiranje številk pri izpisu - ractable -  €
# ---------------------------------------------------


#' Formatiraj numerične vrednosti z € za prikaz v reactable tabelah
#' 
#' Funkcija formatira finančne zneske v slovenskih standardih (vejica za 
#' decimale, pika za tisočice) in doda simbol €. Cela števila prikaže brez 
#' decimalk, decimalna števila pa zaokroži na 1 decimalno mesto.
#' 
#' @param value Numerična vrednost iz stolpca
#' 
#' @return Character string formatiranega zneska z € simbolom ali originalna 
#'   vrednost če ni numerična
#' 
#' @details
#' Formatiranje:
#' - Cela števila: brez decimalk (1234 -> "1.234 €")
#' - Decimalna števila: 1 decimalno mesto (4567.8 -> "4.567,8 €")
#' - Decimalni separator: vejica (,)
#' - Separator tisočic: pika (.)
#' - Ne-numerične vrednosti ostanejo nespremenjene
#' 
#' @examples
#' # Uporaba v reactable tabeli
#' library(reactable)
#' reactable(
#'   data = financni_podatki,
#'   defaultColDef = colDef(
#'     cell = format_numeric_eur
#'   )
#' )
#' 
#' # Testni primeri
#' format_numeric_eur(1234)        # "1.234 €"
#' format_numeric_eur(4567.8)      # "4.567,8 €"
#' format_numeric_eur(25)          # "25 €"
#' format_numeric_eur("tekst")     # "tekst"
format_numeric_eur <- function(value) {
  # Preveri, če je vrednost numeric ali integer
  if (is.numeric(value)) {
    # Zaokroži na 1 decimalno mesto
    rounded <- round(value, 1)
    
    # Preveri, če je število celo (brez decimalnega dela)
    if (rounded == floor(rounded)) {
      # Za cela števila: prikaži brez decimalk
      formatted <- format(
        rounded,
        big.mark = ".",
        scientific = FALSE,
        trim = TRUE
      )
    } else {
      # Za decimalna števila: prikaži 1 decimalno mesto
      formatted <- format(
        rounded,
        decimal.mark = ",",
        big.mark = ".",
        nsmall = 1,
        trim = TRUE
      )
    }
    
    return(paste0(formatted, " €"))
  }
  
  # Če ni numeric/integer, vrni vrednost kot je
  return(value)
}


#-------------------------------------------------------------------------------


# ---------------------------------------------------------
# Formatiranje številk pri izpisu - ractable -  odstotki %
# ---------------------------------------------------------

#' Formatiraj numerične vrednosti kot odstotke za prikaz v reactable tabelah
#' 
#' Funkcija formatira numerične vrednosti kot odstotke v slovenskih standardih 
#' (vejica za decimale, pika za tisočice) in doda simbol %. Cela števila 
#' prikaže brez decimalk, decimalna števila pa zaokroži na 1 decimalno mesto.
#' 
#' @param value Numerična vrednost iz stolpca (že v obliki odstotka, npr. 45.6 za 45,6%)
#' @param digits Število decimalnih mest (default = 1)
#' 
#' @return Character string formatiranega odstotka z % simbolom ali originalna 
#'   vrednost če ni numerična
#' 
#' @details
#' Formatiranje:
#' - Cela števila: brez decimalk (45 -> "45 %", 100 -> "100 %")
#' - Decimalna števila: privzeto 1 decimalno mesto (45.67 -> "45,7 %")
#' - Decimalni separator: vejica (,)
#' - Separator tisočic: pika (.) - če je potrebno
#' - Ne-numerične vrednosti ostanejo nespremenjene
#' 
#' POMEMBNO: Funkcija pričakuje, da so vrednosti že v obliki odstotka 
#' (npr. 45.6, ne 0.456). Če so tvoje vrednosti v obliki decimalk (0-1), 
#' jih pred formatiranjem pomnoži s 100.
#' 
#' @examples
#' # Uporaba v reactable tabeli
#' library(reactable)
#' reactable(
#'   data = podatki_z_odstotki,
#'   defaultColDef = colDef(
#'     cell = format_percent_sl
#'   )
#' )
#' 
#' # Z različnim številom decimalnih mest
#' reactable(
#'   data = podatki_z_odstotki,
#'   columns = list(
#'     natancnost = colDef(
#'       cell = function(value) format_percent_sl(value, digits = 2)
#'     )
#'   )
#' )
#' 
#' # Testni primeri - vrednosti že v odstotkih
#' format_percent_sl(45)           # "45 %"
#' format_percent_sl(45.67)        # "45,7 %"
#' format_percent_sl(45.67, 2)     # "45,67 %"
#' format_percent_sl(100)          # "100 %"
#' format_percent_sl(0.5)          # "0,5 %"
#' format_percent_sl("tekst")      # "tekst"
#' 
#' # Če so tvoje vrednosti v obliki 0-1 (decimale)
#' data$odstotek_formatted <- sapply(data$decimalna_vrednost * 100, format_percent_sl)
#' # Ali v reactable:
#' reactable(
#'   data,
#'   columns = list(
#'     decimalna_vrednost = colDef(
#'       cell = function(value) format_percent_sl(value * 100)
#'     )
#'   )
#' )


format_percent_sl <- function(value, digits = 1) {
  # Preveri, če je vrednost numeric ali integer
  if (is.numeric(value)) {
    # Zaokroži na izbrano število decimalnih mest
    rounded <- round(value, digits)
    
    # Preveri, če je število celo (brez decimalnega dela)
    if (rounded == floor(rounded)) {
      # Za cela števila: prikaži brez decimalk
      formatted <- format(
        rounded,
        big.mark = ".",
        scientific = FALSE,
        trim = TRUE
      )
    } else {
      # Za decimalna števila: prikaži izbrano število decimalnih mest
      formatted <- format(
        rounded,
        decimal.mark = ",",
        big.mark = ".",
        nsmall = digits,
        trim = TRUE
      )
    }
    
    return(paste0(formatted, " %"))
  }
  
  # Če ni numeric/integer, vrni vrednost kot je
  return(value)
}


# ------------------------------------------------------------------


# -------------------------------
# HEATMAP za Reactable tabele
# -------------------------------

#' Ustvari barvo za heatmap celico v reactable tabeli
#' 
#' Funkcija normalizira numerično vrednost glede na min/max obseg in vrne 
#' ustrezno barvo iz modrega gradienta. Uporablja se za vizualizacijo 
#' finančnih podatkov ali drugih numeričnih metrik v reactable tabelah.
#' 
#' @param value Numerična vrednost celice
#' @param min_val Minimalna vrednost v obsegu (najsvetlejša barva)
#' @param max_val Maksimalna vrednost v obsegu (najtemnejša barva)
#' 
#' @return RGB barva kot character string (npr. "#e6f0f7")
#' 
#' @details
#' Gradient barv (od svetle do temne):
#' - Minimalna vrednost: #e6f0f7 (zelo svetlo modra)
#' - Srednje vrednosti: #b8cee6, #8aabcf (srednje modra)
#' - Maksimalna vrednost: #5a82b8 (temno modra, svetlejša od header #3660a0)
#' 
#' Funkcija linearno interpolira med štirimi barvami glede na normalizirano 
#' pozicijo vrednosti v obsegu [min_val, max_val].
#' 
#' @examples
#' # Enostaven primer
#' heatmap_color(50, 0, 100)  # Srednja vrednost -> srednja barva
#' heatmap_color(0, 0, 100)   # Minimum -> #e6f0f7 (svetlo)
#' heatmap_color(100, 0, 100) # Maksimum -> #5a82b8 (temno)
#' 
#' # Uporaba v reactable tabeli - globalni gradient (vsi stolpci skupaj)
#' library(reactable)
#' 
#' # Izračunaj globalni min/max
#' all_values <- unlist(data[, 2:6])
#' global_min <- min(all_values, na.rm = TRUE)
#' global_max <- max(all_values, na.rm = TRUE)
#' 
#' reactable(
#'   data,
#'   columns = list(
#'     stolpec1 = colDef(
#'       style = function(value) {
#'         list(background = heatmap_color(value, global_min, global_max))
#'       }
#'     )
#'   )
#' )
#' 
#' # Uporaba v reactable tabeli - ločeni gradienti
#' # Primer: stolpci 2-5 skupaj, stolpec 6 ločeno
#' values_2_5 <- unlist(data[, 2:5])
#' min_2_5 <- min(values_2_5, na.rm = TRUE)
#' max_2_5 <- max(values_2_5, na.rm = TRUE)
#' 
#' min_6 <- min(data[, 6], na.rm = TRUE)
#' max_6 <- max(data[, 6], na.rm = TRUE)
#' 
#' reactable(
#'   data,
#'   columns = list(
#'     stolpec2 = colDef(
#'       style = function(value) {
#'         list(background = heatmap_color(value, min_2_5, max_2_5))
#'       }
#'     ),
#'     stolpec6 = colDef(
#'       style = function(value) {
#'         list(background = heatmap_color(value, min_6, max_6))
#'       }
#'     )
#'   )
#' )


heatmap_color <- function(value, min_val, max_val) {
  # Normaliziraj vrednost med 0 in 1
  normalized <- (value - min_val) / (max_val - min_val)
  
  # Barve od svetlo modre (#e6f0f7) do srednje modre (#5a82b8)
  # Gradient je svetlejši od header barve (#3660a0) za boljšo berljivost
  colors <- colorRamp(c("#e6f0f7", "#b8cee6", "#8aabcf", "#5a82b8"))(normalized)
  
  # Vrni RGB barvo kot string
  rgb(colors[1], colors[2], colors[3], maxColorValue = 255)
}
