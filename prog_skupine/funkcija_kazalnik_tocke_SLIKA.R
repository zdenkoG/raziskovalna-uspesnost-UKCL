#' Naredi panel z tremi grafi za prikaz kazalnikov
#' 
#' @param podatki Tibble s podatki (mora vsebovati stolpce: code, name, vodja, ukc_vod, raz, fte, kaz_fte, kaz_raz)
#' @param kazalnik Character, "kaz_fte" ali "kaz_raz"
#' @param naslov_levi Character, naslov za levi graf (celota)
#' @param naslov_ukc Character, naslov za desni zgornji graf (UKC vodilni)
#' @param naslov_zunanji Character, naslov za desni spodnji graf (zunanji)
#' @param os_naslov Character, naslov za Y os
#' @return HTML output z tremi grafi
#' 
graf_panel_programi <- function(podatki,
                                kazalnik = "kaz_fte",
                                naslov_levi = "Raziskovalna uspešnost po programih",
                                naslov_ukc = "UKCL vodilna organizacija",
                                naslov_zunanji = "UKCL sodelujoča organizacija",
                                os_naslov = NULL) {
  
  library(highcharter)
  library(dplyr)
  library(bslib)
  
  # Privzeti naslov osi glede na kazalnik
  if (is.null(os_naslov)) {
    os_naslov <- ifelse(kazalnik == "kaz_fte",
                        "Kazalnik (Točke na FTE)",
                        "Kazalnik (Točke na raziskovalca)")
  }
  
  # Funkcija za izdelavo posameznega grafa
  naredi_graf <- function(df, naslov, podnaslov = NULL, visina = "600px") {
    
    df <- df %>%
      arrange(desc(.data[[kazalnik]])) %>%
      mutate(
        program_label = code,  # Samo koda programa
        alpha = 0.3 + (fte / max(fte)) * 0.7,
        barva_rgba = ifelse(
          ukc_vod == 1,
          paste0("rgba(0, 81, 165, ", alpha, ")"),
          paste0("rgba(153, 153, 153, ", alpha, ")")
        )
      )
    
    hc <- highchart() %>%
      hc_chart(inverted = TRUE, height = visina) %>%
      hc_add_series(
        data = lapply(1:nrow(df), function(i) {
          list(
            x = i - 1,
            y = df[[kazalnik]][i],
            color = df$barva_rgba[i],
            code = df$code[i],
            name = df$name[i],
            vodja = df$vodja[i],
            kaz_fte = df$kaz_fte[i],
            kaz_raz = df$kaz_raz[i],
            fte = df$fte[i],
            raz = df$raz[i],
            ukc_vod = df$ukc_vod[i],
            kazalnik_tip = kazalnik
          )
        }),
        type = "bar",
        name = "Kazalnik",
        borderWidth = 0,
        dataLabels = list(enabled = FALSE),
        tooltip = list(
          pointFormatter = JS("function() {
            var ukc = this.ukc_vod == 1 ? 'DA' : 'NE';
            var kazalnik_text = '';
            
            // Pogojni izpis glede na tip kazalnika
            if (this.kazalnik_tip === 'kaz_fte') {
              kazalnik_text = 'Točke/FTE: <b>' + this.kaz_fte.toFixed(0) + '</b><br/>';
            } else if (this.kazalnik_tip === 'kaz_raz') {
              kazalnik_text = 'Točke/raziskovalca: <b>' + this.kaz_raz.toFixed(0) + '</b><br/>';
            }
            
            return '<b>' + this.code + '</b><br/>' +
                   '<i>' + this.name + '</i><br/>' +
                   this.vodja + '<br/><br/>' +
                   kazalnik_text +
                   'FTE: <b>' + this.fte.toFixed(1) + '</b><br/>' +
                   'Raziskovalci: <b>' + this.raz + '</b><br/>' +
                   'UKC vodilna: <b>' + ukc + '</b>';
          }")
        )
      ) %>%
      hc_xAxis(
        type = "category",
        categories = df$program_label,
        title = list(text = ""),
        labels = list(style = list(fontSize = "10px"))
      ) %>%
      hc_yAxis(
        title = list(text = os_naslov),
        min = 0
      ) %>%
      hc_title(
        text = naslov,
        style = list(fontSize = "16px", fontWeight = "bold")
      ) %>%
      hc_legend(enabled = FALSE) %>%
      hc_exporting(enabled = TRUE) %>%
      hc_credits(enabled = FALSE)
    
    # Dodaj podnaslov če obstaja
    if (!is.null(podnaslov)) {
      hc <- hc %>%
        hc_subtitle(
          text = podnaslov,
          useHTML = TRUE
        )
    }
    
    return(hc)
  }
  
  # Razdeli podatke
  vsi_programi <- podatki
  ukc_programi <- podatki %>% filter(ukc_vod == 1)
  zunanji_programi <- podatki %>% filter(ukc_vod == 0)
  
  # Naredi grafe
  graf_vsi <- naredi_graf(
    vsi_programi, 
    naslov_levi,
    podnaslov = "",
    visina = "900px"
  )
  
  graf_ukc <- naredi_graf(
    ukc_programi,
    naslov_ukc,
    visina = "450px"
  )
  
  graf_zunanji <- naredi_graf(
    zunanji_programi,
    naslov_zunanji,
    visina = "350px"
  )
  
  # Sestavi layout z bslib
  layout_columns(
    col_widths = c(6, 6),
    graf_vsi,
    layout_column_wrap(
      width = 1,
      graf_ukc,
      graf_zunanji
    )
  )
}


