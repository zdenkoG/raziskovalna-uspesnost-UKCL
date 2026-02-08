#' Naredi panel z tremi grafi za prikaz kazalnikov
#' 
#' @param podatki Tibble s podatki (mora vsebovati stolpce: code, name, vodja, ukc_vod, raz, fte, kaz_fte, kaz_raz)
#' @param kazalnik Character, "kaz_fte" ali "kaz_raz" ali "hIndex" ali "cimax"
#' @param naslov_levi Character, naslov za levi graf (celota)
#' @param naslov_ukc Character, naslov za desni zgornji graf (UKC vodilni)
#' @param naslov_zunanji Character, naslov za desni spodnji graf (zunanji)
#' @param os_naslov Character, naslov za Y os
#' @param vir_podatkov Character, ime objekta s podatki (npr. "ci10_prog", "cit_prog") - uporablja se za avtomatsko določanje naslova osi
#' @return HTML output z tremi grafi
#' @note Zahtevani paketi: highcharter, dplyr, bslib
#' 
graf_panel_programi <- function(podatki,
                                kazalnik = "kaz_fte",
                                naslov_levi = "Raziskovalna uspešnost po programih",
                                naslov_ukc = "UKCL vodilna organizacija",
                                naslov_zunanji = "UKCL sodelujoča organizacija",
                                os_naslov = NULL,
                                vir_podatkov = NULL) {
  
  # Privzeti naslov osi glede na kazalnik in vir podatkov
  if (is.null(os_naslov)) {
    # Preveri, ali so podatki iz cit_prog
    if (!is.null(vir_podatkov) && vir_podatkov == "cit_prog") {
      # Za cit_prog podatke preveri kazalnik
      os_naslov <- dplyr::case_when(
        kazalnik == "cimax" ~ "Število citatov",
        kazalnik == "hIndex" ~ "H-indeks",
        TRUE ~ "Kazalnik"
      )
    } else if (!is.null(vir_podatkov) && vir_podatkov == "ci10_prog") {
      # Za ci10_prog podatke
      os_naslov <- ifelse(kazalnik == "kaz_fte",
                          "Kazalnik (Citati na FTE)",
                          "Kazalnik (Citati na raziskovalca)")
    } else {
      # Za točke in ostale podatke
      os_naslov <- ifelse(kazalnik == "kaz_fte",
                          "Kazalnik (Točke na FTE)",
                          "Kazalnik (Točke na raziskovalca)")
    }
  }
  
  # Določi ali gre za citate - shrani v lokalno spremenljivko
  je_citati <- !is.null(vir_podatkov) && vir_podatkov == "ci10_prog"
  
  # Shrani kazalnik v lokalno spremenljivko za nested funkcijo
  kazalnik_vrednost <- kazalnik
  os_naslov_vrednost <- os_naslov
  
  # Funkcija za izdelavo posameznega grafa
  naredi_graf <- function(df, naslov, podnaslov = NULL, visina = "600px") {
    
    df <- df %>%
      dplyr::arrange(dplyr::desc(.data[[kazalnik_vrednost]])) %>%
      dplyr::mutate(
        program_label = code,  # Samo koda programa
        alpha = 0.3 + (fte / max(fte, na.rm = TRUE)) * 0.7,
        barva_rgba = ifelse(
          ukc_vod == 1,
          paste0("rgba(0, 81, 165, ", alpha, ")"),
          paste0("rgba(153, 153, 153, ", alpha, ")")
        )
      )
    
    hc <- highcharter::highchart() %>%
      highcharter::hc_chart(inverted = TRUE, height = visina) %>%
      highcharter::hc_add_series(
        data = lapply(1:nrow(df), function(i) {
          list(
            x = i - 1,
            y = df[[kazalnik_vrednost]][i],
            color = df$barva_rgba[i],
            code = df$code[i],
            name = df$name[i],
            vodja = df$vodja[i],
            kaz_fte = if("kaz_fte" %in% names(df)) df$kaz_fte[i] else NULL,
            kaz_raz = if("kaz_raz" %in% names(df)) df$kaz_raz[i] else NULL,
            hIndex = if("hIndex" %in% names(df)) df$hIndex[i] else NULL,
            cimax = if("cimax" %in% names(df)) df$cimax[i] else NULL,
            fte = df$fte[i],
            raz = df$raz[i],
            ukc_vod = df$ukc_vod[i],
            kazalnik_tip = kazalnik_vrednost,
            je_citati = je_citati
          )
        }),
        type = "bar",
        name = "Kazalnik",
        borderWidth = 0,
        dataLabels = list(enabled = FALSE),
        tooltip = list(
          pointFormatter = htmlwidgets::JS("function() {
            var ukc = this.ukc_vod == 1 ? 'DA' : 'NE';
            var kazalnik_text = '';
            
            // Določi oznako glede na tip podatkov (citati ali točke)
            var metrika = this.je_citati ? 'Citati' : 'Točke';
            
            // Pogojni izpis glede na tip kazalnika
            if (this.kazalnik_tip === 'kaz_fte') {
              kazalnik_text = metrika + '/FTE: <b>' + this.kaz_fte.toFixed(0) + '</b><br/>';
            } else if (this.kazalnik_tip === 'kaz_raz') {
              kazalnik_text = metrika + '/raziskovalca: <b>' + this.kaz_raz.toFixed(0) + '</b><br/>';
            } else if (this.kazalnik_tip === 'hIndex') {
              kazalnik_text = 'H-indeks: <b>' + this.hIndex + '</b><br/>';
            } else if (this.kazalnik_tip === 'cimax') {
              kazalnik_text = 'Število citatov: <b>' + this.cimax + '</b><br/>';
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
      highcharter::hc_xAxis(
        type = "category",
        categories = df$program_label,
        title = list(text = ""),
        labels = list(style = list(fontSize = "10px"))
      ) %>%
      highcharter::hc_yAxis(
        title = list(text = os_naslov_vrednost),
        min = 0
      ) %>%
      highcharter::hc_title(
        text = naslov,
        style = list(fontSize = "16px", fontWeight = "bold")
      ) %>%
      highcharter::hc_legend(enabled = FALSE) %>%
      highcharter::hc_exporting(enabled = TRUE)
    
    # Dodaj podnaslov če obstaja
    if (!is.null(podnaslov)) {
      hc <- hc %>%
        highcharter::hc_subtitle(
          text = podnaslov,
          useHTML = TRUE
        )
    }
    
    return(hc)
  }
  
  # Razdeli podatke
  vsi_programi <- podatki
  ukc_programi <- podatki %>% dplyr::filter(ukc_vod == 1)
  zunanji_programi <- podatki %>% dplyr::filter(ukc_vod == 0)
  
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
  bslib::layout_columns(
    col_widths = c(6, 6),
    graf_vsi,
    bslib::layout_column_wrap(
      width = 1,
      graf_ukc,
      graf_zunanji
    )
  )
}