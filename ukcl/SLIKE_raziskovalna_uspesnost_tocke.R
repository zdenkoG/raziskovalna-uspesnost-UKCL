
#' Izračunaj letne spremembe za točke
#'
#' @param data Tibble/data.frame s stolpci: leto, objave, tocke
#' @param kategorija Ime kategorije objav
#'
#' @return Tibble z dodanimi stolpci za spremembe točk
izracunaj_spremembe_tocke <- function(data, kategorija) {
  
  data %>%
    filter(objave == kategorija) %>%
    arrange(leto) %>%
    mutate(
      # Absolutna sprememba točk
      sprememba_tocke_abs = tocke - lag(tocke),
      # Relativna sprememba točk v %
      sprememba_tocke_pct = ((tocke - lag(tocke)) / lag(tocke)) * 100,
      # Barva glede na predznak
      barva = ifelse(sprememba_tocke_abs >= 0, "#2d8659", "#c73a3a")  # zelena / rdeča
    )
}

#' Izračunaj letne spremembe kazalnika
#'
#' @param data Tibble/data.frame s stolpci: leto, objave, in kazalnikom
#' @param kategorija Ime kategorije objav
#' @param kazalnik Ime stolpca s kazalnikom (npr. "kaz_fte" ali "kaz_raz")
#'
#' @return Tibble z dodanimi stolpci za spremembe
izracunaj_spremembe_kazalnik <- function(data, kategorija, kazalnik = "kaz_fte") {
  
  data %>%
    filter(objave == kategorija) %>%
    arrange(leto) %>%
    mutate(
      # Absolutna sprememba za izbrani kazalnik
      sprememba_abs = .data[[kazalnik]] - lag(.data[[kazalnik]]),
      # Relativna sprememba v %
      sprememba_pct = ((.data[[kazalnik]] - lag(.data[[kazalnik]])) / lag(.data[[kazalnik]])) * 100,
      # Barva glede na predznak
      barva = ifelse(sprememba_abs >= 0, "#2d8659", "#c73a3a")  # zelena / rdeča
    )
}

#' Izračunaj optimalno y-os za lollipop graf
#'
#' @param values Vektor vrednosti
#' @return Seznam z min in max
izracunaj_y_limiti <- function(values) {
  min_val <- min(values, na.rm = TRUE)
  max_val <- max(values, na.rm = TRUE)
  
  # Če so vse vrednosti pozitivne
  if (min_val >= 0) {
    list(
      min = 0,
      max = ceiling(max_val * 1.15)
    )
  } 
  # Če so vse vrednosti negativne
  else if (max_val <= 0) {
    list(
      min = floor(min_val * 1.15),
      max = 0
    )
  } 
  # Če so mešane vrednosti - uporabljamo razmerje
  else {
    # Izračunaj razmerje pozitivnih in negativnih vrednosti
    abs_min <- abs(min_val)
    abs_max <- abs(max_val)
    ratio <- abs_max / abs_min
    
    # Določi limite, da bo 0 na pravem mestu glede na razmerje
    if (ratio > 1) {
      # Več pozitivnih vrednosti
      y_max <- ceiling(max_val * 1.1)
      y_min <- -y_max / ratio
    } else {
      # Več negativnih vrednosti
      y_min <- floor(min_val * 1.1)
      y_max <- -y_min * ratio
    }
    
    list(
      min = y_min,
      max = y_max
    )
  }
}

#' Nariši lollipop graf spremembe točk
#'
#' @param data Tibble/data.frame s stolpci: leto, objave, tocke
#' @param kategorija Ime kategorije objav
#' @param naslov Naslov grafa
#'
#' @return Highchart objekt
narisi_lollipop_sprememb_tocke <- function(data, 
                                           kategorija,
                                           naslov_lp = "Letna sprememba točk",
                                           y_os = "Sprememba točk") {
  
  # Pripravi podatke
  podatki <- izracunaj_spremembe_tocke(data, kategorija)
  
  # Odstrani prvo leto (nima spremembe)
  podatki <- podatki %>% filter(!is.na(sprememba_tocke_abs))
  
  # Izračunaj optimalne limite
  limiti <- izracunaj_y_limiti(podatki$sprememba_tocke_abs)
  
  # Ustvari lollipop graf
  highchart() %>%
    hc_chart(
      type = "line",
      height = 400,
      backgroundColor = "#FFFFFF"  # Belo ozadje
    ) %>%
    hc_title(text = naslov_lp) %>%
    hc_xAxis(
      categories = podatki$leto,
      title = list(text = "Leto")
    ) %>%
    hc_yAxis(
      title = list(text = y_os),
      labels = list(format = "{value:,.0f}"),
      min = limiti$min,
      max = limiti$max,
      plotLines = list(
        list(
          value = 0,
          color = "#333333",
          width = 2,
          zIndex = 5
        )
      )
    ) %>%
    hc_add_series(
      name = "Sprememba",
      data = lapply(1:nrow(podatki), function(i) {
        list(
          y = round(podatki$sprememba_tocke_abs[i], 1),
          color = podatki$barva[i],
          marker = list(
            enabled = TRUE,
            radius = 8,
            fillColor = podatki$barva[i]
          )
        )
      }),
      lineWidth = 0,
      states = list(
        hover = list(
          lineWidthPlus = 0
        )
      ),
      dataLabels = list(
        enabled = TRUE,
        format = "{point.y:,.0f}",
        style = list(
          fontWeight = "bold",
          fontSize = "11px"
        ),
        y = -15,
        verticalAlign = "bottom"
      )
    ) %>%
    # Dodaj "palčke" od ničle do točke
    hc_add_series(
      type = "errorbar",
      name = "Palčki",
      data = lapply(1:nrow(podatki), function(i) {
        list(
          low = 0,
          high = round(podatki$sprememba_tocke_abs[i], 1),
          color = podatki$barva[i]
        )
      }),
      whiskerLength = 0,
      stemWidth = 3,
      showInLegend = FALSE,
      enableMouseTracking = FALSE
    ) %>%
    hc_tooltip(
      shared = FALSE,
      pointFormat = "<b>{point.y:,.1f}</b> točk",
      headerFormat = "<span style='font-size:10px'>{point.key}</span><br/>"
    ) %>%
    hc_legend(enabled = FALSE) %>%
    hc_exporting(
      enabled = TRUE,
      chartOptions = list(
        chart = list(
          backgroundColor = "#FFFFFF"  # Belo ozadje v full screen
        )
      )
    )
}

#' Nariši lollipop graf letnih sprememb kazalnika
#'
#' @param data Tibble/data.frame s stolpci: leto, objave, in kazalnikom
#' @param kategorija Ime kategorije objav
#' @param kazalnik Ime stolpca s kazalnikom (npr. "kaz_fte" ali "kaz_raz")
#' @param naslov Naslov grafa
#'
#' @return Highchart objekt
narisi_lollipop_sprememb_kazalnik <- function(data, 
                                              kategorija,
                                              kazalnik = "kaz_fte",
                                              naslov = "Letna sprememba kazalnika (%)") {
  
  # Pripravi podatke
  podatki <- izracunaj_spremembe_kazalnik(data, kategorija, kazalnik)
  
  # Odstrani prvo leto (nima spremembe)
  podatki <- podatki %>% filter(!is.na(sprememba_abs))
  
  # Izračunaj optimalne limite
  limiti <- izracunaj_y_limiti(podatki$sprememba_pct)
  
  # Ustvari lollipop graf
  highchart() %>%
    hc_chart(
      type = "line",
      height = 400,
      backgroundColor = "#FFFFFF"  # Belo ozadje
    ) %>%
    hc_title(text = naslov) %>%
    hc_xAxis(
      categories = podatki$leto,
      title = list(text = "Leto")
    ) %>%
    hc_yAxis(
      title = list(text = "Sprememba kazalnika (%)"),
      labels = list(format = "{value:.1f}%"),
      min = limiti$min,
      max = limiti$max,
      plotLines = list(
        list(
          value = 0,
          color = "#333333",
          width = 2,
          zIndex = 5
        )
      )
    ) %>%
    hc_add_series(
      name = "Sprememba",
      data = lapply(1:nrow(podatki), function(i) {
        list(
          y = round(podatki$sprememba_pct[i], 1),
          color = podatki$barva[i],
          marker = list(
            enabled = TRUE,
            radius = 8,
            fillColor = podatki$barva[i]
          )
        )
      }),
      lineWidth = 0,
      states = list(
        hover = list(
          lineWidthPlus = 0
        )
      ),
      dataLabels = list(
        enabled = TRUE,
        format = "{point.y:.1f}%",
        style = list(
          fontWeight = "bold",
          fontSize = "11px"
        ),
        y = -15,
        verticalAlign = "bottom"
      )
    ) %>%
    # Dodaj "palčke" od ničle do točke
    hc_add_series(
      type = "errorbar",
      name = "Palčki",
      data = lapply(1:nrow(podatki), function(i) {
        list(
          low = 0,
          high = round(podatki$sprememba_pct[i], 1),
          color = podatki$barva[i]
        )
      }),
      whiskerLength = 0,
      stemWidth = 3,
      showInLegend = FALSE,
      enableMouseTracking = FALSE
    ) %>%
    hc_tooltip(
      shared = FALSE,
      pointFormat = "<b>{point.y:.1f}%</b>",
      headerFormat = "<span style='font-size:10px'>{point.key}</span><br/>"
    ) %>%
    hc_legend(enabled = FALSE) %>%
    hc_exporting(
      enabled = TRUE,
      chartOptions = list(
        chart = list(
          backgroundColor = "#FFFFFF"  # Belo ozadje v full screen
        )
      )
    )
}

#' Nariši panel 2x2 s štirimi grafi
#'
#' @param data Tibble/data.frame s stolpci: leto, objave, tocke, in kazalnikom
#' @param kategorija Ime kategorije objav
#' @param naslov Naslov panela
#' @param barva Barva za prva dva grafa (točke in kazalnik)
#' @param kazalnik Ime stolpca s kazalnikom (npr. "kaz_fte" ali "kaz_raz")
#' @param kazalnik_label Oznaka za kazalnik na grafu (npr. "Kazalnik (točke/FTE)")
#'
#' @return HTML div s štirimi grafi v 2x2 mreži
create_panel_2x2 <- function(data, 
                             kategorija,
                             naslov = kategorija,
                             barva = "#3660a0",
                             yaxis_label = "Točke",
                             title_label = "Točke po letih",
                             kazalnik = "kaz_fte",
                             kazalnik_label = "Kazalnik (točke/FTE)",
                             naslov_lp = "Letna sprememba točk",
                             y_os = "Sprememba točk") {
  
  # Filtriraj podatke za izbrano kategorijo
  podatki_kat <- data %>%
    filter(objave == kategorija) %>%
    arrange(leto)
  
  # Preveri, ali obstajajo podatki za to kategorijo
  if (nrow(podatki_kat) == 0) {
    warning(paste0("Ni podatkov za kategorijo: ", kategorija))
    return(NULL)
  }
  
  # Izračunaj min in max za kazalnik
  kaz_min <- min(podatki_kat[[kazalnik]], na.rm = TRUE)
  kaz_max <- max(podatki_kat[[kazalnik]], na.rm = TRUE)
  kaz_range <- kaz_max - kaz_min
  
  # Nova logika: 20% padding nad in pod
  y_min <- floor(kaz_min * 0.8)  # 20% pod minimum
  y_max <- ceiling(kaz_max * 1.2)  # 20% nad maximum
  
  # Določi primeren tick interval
  if (kaz_range < 50) {
    tick_interval <- 10
  } else if (kaz_range < 200) {
    tick_interval <- 50
  } else {
    tick_interval <- 100
  }
  
  # Graf 1: Točke po letih (zgoraj levo)
  graf_tocke <- highchart() %>%
    hc_chart(
      type = "column", 
      height = 400,
      backgroundColor = "#FFFFFF"  # Belo ozadje
    ) %>%
    hc_title(text = title_label) %>%
    hc_xAxis(
      categories = podatki_kat$leto,
      title = list(text = "Leto")
    ) %>%
    hc_yAxis(
      title = list(text = yaxis_label),
      labels = list(format = "{value:,.0f}")
    ) %>%
    hc_add_series(
      name = "Točke",
      data = round(podatki_kat$tocke, 1),
      color = barva,
      dataLabels = list(
        enabled = TRUE,
        format = "{point.y:,.0f}"
      )
    ) %>%
    hc_tooltip(
      pointFormat = "<b>{point.y:,.1f}</b> točk",
      headerFormat = "<span style='font-size:10px'>{point.key}</span><br/>"
    ) %>%
    hc_legend(enabled = FALSE) %>% 
    hc_exporting(
      enabled = TRUE,
      chartOptions = list(
        chart = list(
          backgroundColor = "#FFFFFF"  # Belo ozadje v full screen
        )
      )
    )
  
  # Graf 2: Kazalnik po letih (zgoraj desno)
  graf_kaz <- highchart() %>%
    hc_chart(
      type = "line", 
      height = 400,
      backgroundColor = "#FFFFFF"  # Belo ozadje
    ) %>%
    hc_title(text = "Kazalnik po letih") %>%
    hc_xAxis(
      categories = podatki_kat$leto,
      title = list(text = "Leto")
    ) %>%
    hc_yAxis(
      title = list(text = kazalnik_label),
      labels = list(format = "{value:.0f}"),
      min = y_min,
      max = y_max,
      tickInterval = tick_interval
    ) %>%
    hc_add_series(
      name = "Kazalnik",
      data = round(podatki_kat[[kazalnik]], 1),
      color = barva,
      marker = list(enabled = TRUE, radius = 5),
      dataLabels = list(enabled = TRUE, format = "{point.y:.1f}")
    ) %>%
    hc_tooltip(
      pointFormat = paste0("<b>{point.y:.1f}</b> ", 
                           sub("Kazalnik \\((.+)\\)", "\\1", kazalnik_label)),
      headerFormat = "<span style='font-size:10px'>{point.key}</span><br/>"
    ) %>%
    hc_legend(enabled = FALSE) %>% 
    hc_exporting(
      enabled = TRUE,
      chartOptions = list(
        chart = list(
          backgroundColor = "#FFFFFF"  # Belo ozadje v full screen
        )
      )
    )
  
  # Graf 3: Sprememba točk (spodaj levo)
  graf_sprememba_tocke <- narisi_lollipop_sprememb_tocke(
    data, 
    kategorija,
    naslov_lp,
    y_os
  )
  
  # Graf 4: Sprememba kazalnika (spodaj desno)
  graf_sprememba_kaz <- narisi_lollipop_sprememb_kazalnik(
    data, 
    kategorija,
    kazalnik,
    "Letna sprememba kazalnika (%)"
  )
  
  # Ustvari HTML panel s štirimi grafi v 2x2 mreži
  browsable(
    div(
      style = "margin-bottom: 40px;",
      # Naslov kategorije
      h3(naslov, style = paste0("color: ", barva, "; margin-bottom: 15px;")),
      
      # Vrstica 1: Točke in Kazalnik
      div(
        style = "display: flex; gap: 20px; flex-wrap: wrap; margin-bottom: 20px;",
        # Graf 1: Točke
        div(
          style = "flex: 1; min-width: 400px;",
          graf_tocke
        ),
        # Graf 2: Kazalnik
        div(
          style = "flex: 1; min-width: 400px;",
          graf_kaz
        )
      ),
      
      # Vrstica 2: Spremembe
      div(
        style = "display: flex; gap: 20px; flex-wrap: wrap;",
        # Graf 3: Sprememba točk
        div(
          style = "flex: 1; min-width: 400px;",
          graf_sprememba_tocke
        ),
        # Graf 4: Sprememba kazalnika
        div(
          style = "flex: 1; min-width: 400px;",
          graf_sprememba_kaz
        )
      )
    )
  )
}




# =================================
# =================================

# UPORABA
# -------

# === KAZALNIK FTE (kaz_fte) - modre barve ===

panel1_fte <- create_panel_2x2(
  tabela_kaz_tocke_ukcl, 
  "Upoštevane točke", 
  "Upoštevane točke", 
  "#3660a0",
  yaxis_label = "Točke",
  title_label = "Točke po letih",
  kazalnik = "kaz_fte",
  kazalnik_label = "Kazalnik (točke/FTE)"
)

panel2_fte <- create_panel_2x2(
  tabela_kaz_tocke_ukcl, 
  "A'' - izjemni dosežki", 
  "A'' - izjemni dosežki", 
  "#4a7bc8",
  yaxis_label = "Točke",
  title_label = "Točke po letih",
  kazalnik = "kaz_fte",
  kazalnik_label = "Kazalnik (točke/FTE)"
)

panel3_fte <- create_panel_2x2(
  tabela_kaz_tocke_ukcl, 
  "A' - zelo kvalitetni dosežki", 
  "A' - zelo kvalitetni dosežki", 
  "#5d8dd3",
  yaxis_label = "Točke",
  title_label = "Točke po letih",
  kazalnik = "kaz_fte",
  kazalnik_label = "Kazalnik (točke/FTE)"
)

panel4_fte <- create_panel_2x2(
  tabela_kaz_tocke_ukcl, 
  "A1/2 - pomembni dosežki", 
  "A1/2 - pomembni dosežki", 
  "#7099db",
  yaxis_label = "Točke",
  title_label = "Točke po letih",
  kazalnik = "kaz_fte",
  kazalnik_label = "Kazalnik (točke/FTE)"
)

# === KAZALNIK RAZISKOVALCI (kaz_raz) - sive barve ===

panel1_raz <- create_panel_2x2(
  tabela_kaz_tocke_ukcl, 
  "Upoštevane točke", 
  "Upoštevane točke", 
  "#b0b0b0",
  yaxis_label = "Točke",
  title_label = "Točke po letih",
  kazalnik = "kaz_raz",
  kazalnik_label = "Kazalnik (točke/raziskovalec)"
)

panel2_raz <- create_panel_2x2(
  tabela_kaz_tocke_ukcl, 
  "A'' - izjemni dosežki", 
  "A'' - izjemni dosežki", 
  "#7a7a7a",
  yaxis_label = "Točke",
  title_label = "Točke po letih",
  kazalnik = "kaz_raz",
  kazalnik_label = "Kazalnik (točke/raziskovalec)"
)

panel3_raz <- create_panel_2x2(
  tabela_kaz_tocke_ukcl, 
  "A' - zelo kvalitetni dosežki", 
  "A' - zelo kvalitetni dosežki", 
  "#9a9a9a",
  yaxis_label = "Točke",
  title_label = "Točke po letih",
  kazalnik = "kaz_raz",
  kazalnik_label = "Kazalnik (točke/raziskovalec)"
)

panel4_raz <- create_panel_2x2(
  tabela_kaz_tocke_ukcl, 
  "A1/2 - pomembni dosežki", 
  "A1/2 - pomembni dosežki", 
  "#b0b0b0",
  yaxis_label = "Točke",
  title_label = "Točke po letih",
  kazalnik = "kaz_raz",
  kazalnik_label = "Kazalnik (točke/raziskovalec)"
)



# ---------------------------------

## CITATI

# CI10

panel1_fte_cit <- create_panel_2x2(
  tabela_kaz_citati_ukcl %>% rename(objave = citiranost, tocke = citati), 
  "CI10", 
  "Število čistih citatov znanstvenih del v zadnjih 10 letih",
  "#3660a0",
  yaxis_label = "Citati",
  title_label = "Citati po letih",
  kazalnik = "kaz_fte",
  kazalnik_label = "Kazalnik (citati/FTE)",
  naslov_lp = "Letna sprememba citatov",
  y_os = "Sprememba citatov"
)

panel1_raz_cit <- create_panel_2x2(
  tabela_kaz_citati_ukcl %>% rename(objave = citiranost, tocke = citati), 
  "CI10", 
  "Število čistih citatov znanstvenih del v zadnjih 10 letih", 
  "#b0b0b0",
  yaxis_label = "Citati",
  title_label = "Citati po letih",
  kazalnik = "kaz_raz",
  kazalnik_label = "Kazalnik (citati/raziskovalec)",
  naslov_lp = "Letna sprememba citatov",
  y_os = "Sprememba citatov"
)

#--------------------------------------------------------

# -----------------------------------------------------
# Funkcija za sliko CImax in H-indeks, brez kazalnikov
# -----------------------------------------------------

#' Nariši panel 2x2 za CImax in H-indeks
#'
#' @param data Tibble/data.frame s stolpci: Leto, objave, `Najodmevnejše delo`, `H-indeks`
#' @param naslov Naslov panela
#' @param barva_cimax Barva za grafe CImax
#' @param barva_hind Barva za grafe H-indeks
#'
#' @return HTML div s štirimi grafi v 2x2 mreži
create_panel_citati_hindeks <- function(data, 
                                        naslov = "Citati in H-indeks",
                                        barva_cimax = "#3660a0",
                                        barva_hind = "#2d8659") {
  
  # Pripravi podatke za CImax
  podatki_cimax <- data %>%
    arrange(Leto) %>%
    mutate(
      # Absolutna sprememba citatov
      sprememba_abs = `Najodmevnejše delo` - lag(`Najodmevnejše delo`),
      # Relativna sprememba v %
      sprememba_pct = ((sprememba_abs) / lag(`Najodmevnejše delo`)) * 100,
      # Barva glede na predznak
      barva = ifelse(sprememba_abs >= 0, "#2d8659", "#c73a3a")
    )
  
  # Pripravi podatke za H-indeks
  podatki_hind <- data %>%
    arrange(Leto) %>%
    mutate(
      # Absolutna sprememba H-indeksa
      sprememba_abs = `H-indeks` - lag(`H-indeks`),
      # Relativna sprememba v %
      sprememba_pct = ((sprememba_abs) / lag(`H-indeks`)) * 100,
      # Barva glede na predznak
      barva = ifelse(sprememba_abs >= 0, "#2d8659", "#c73a3a")
    )
  
  # Graf 1: CImax po letih (zgoraj levo)
  graf_cimax <- highchart() %>%
    hc_chart(
      type = "column", 
      height = 400,
      backgroundColor = "#FFFFFF"
    ) %>%
    hc_title(text = "Najodmevnejše delo (citati) po letih") %>%
    hc_xAxis(
      categories = podatki_cimax$Leto,
      title = list(text = "Leto")
    ) %>%
    hc_yAxis(
      title = list(text = "Število citatov"),
      labels = list(format = "{value:,.0f}")
    ) %>%
    hc_add_series(
      name = "Citati",
      data = round(podatki_cimax$`Najodmevnejše delo`, 0),
      color = barva_cimax,
      dataLabels = list(
        enabled = TRUE,
        format = "{point.y:,.0f}"
      )
    ) %>%
    hc_tooltip(
      pointFormat = "<b>{point.y:,.0f}</b> citatov",
      headerFormat = "<span style='font-size:10px'>{point.key}</span><br/>"
    ) %>%
    hc_legend(enabled = FALSE) %>% 
    hc_exporting(
      enabled = TRUE,
      chartOptions = list(
        chart = list(backgroundColor = "#FFFFFF")
      )
    )
  
  # Graf 2: H-indeks po letih (zgoraj desno)
  graf_hind <- highchart() %>%
    hc_chart(
      type = "column", 
      height = 400,
      backgroundColor = "#FFFFFF"
    ) %>%
    hc_title(text = "H-indeks po letih") %>%
    hc_xAxis(
      categories = podatki_hind$Leto,
      title = list(text = "Leto")
    ) %>%
    hc_yAxis(
      title = list(text = "H-indeks"),
      labels = list(format = "{value:,.0f}")
    ) %>%
    hc_add_series(
      name = "H-indeks",
      data = round(podatki_hind$`H-indeks`, 0),
      color = barva_hind,
      dataLabels = list(
        enabled = TRUE,
        format = "{point.y:,.0f}"
      )
    ) %>%
    hc_tooltip(
      pointFormat = "<b>{point.y:,.0f}</b>",
      headerFormat = "<span style='font-size:10px'>{point.key}</span><br/>"
    ) %>%
    hc_legend(enabled = FALSE) %>% 
    hc_exporting(
      enabled = TRUE,
      chartOptions = list(
        chart = list(backgroundColor = "#FFFFFF")
      )
    )
  
  # Odstrani prvo leto (nima spremembe) za lollipop grafe
  podatki_cimax_sprememba <- podatki_cimax %>% filter(!is.na(sprememba_abs))
  podatki_hind_sprememba <- podatki_hind %>% filter(!is.na(sprememba_abs))
  
  # Izračunaj optimalne limite za CImax
  limiti_cimax <- izracunaj_y_limiti(podatki_cimax_sprememba$sprememba_abs)
  
  # Graf 3: Sprememba CImax (spodaj levo)
  graf_sprememba_cimax <- highchart() %>%
    hc_chart(
      type = "line",
      height = 400,
      backgroundColor = "#FFFFFF"
    ) %>%
    hc_title(text = "Letna sprememba števila citatov") %>%
    hc_xAxis(
      categories = podatki_cimax_sprememba$Leto,
      title = list(text = "Leto")
    ) %>%
    hc_yAxis(
      title = list(text = "Sprememba števila citatov"),
      labels = list(format = "{value:,.0f}"),
      min = limiti_cimax$min,
      max = limiti_cimax$max,
      plotLines = list(
        list(
          value = 0,
          color = "#333333",
          width = 2,
          zIndex = 5
        )
      )
    ) %>%
    hc_add_series(
      name = "Sprememba",
      data = lapply(1:nrow(podatki_cimax_sprememba), function(i) {
        list(
          y = round(podatki_cimax_sprememba$sprememba_abs[i], 0),
          color = podatki_cimax_sprememba$barva[i],
          marker = list(
            enabled = TRUE,
            radius = 8,
            fillColor = podatki_cimax_sprememba$barva[i]
          )
        )
      }),
      lineWidth = 0,
      states = list(hover = list(lineWidthPlus = 0)),
      dataLabels = list(
        enabled = TRUE,
        format = "{point.y:,.0f}",
        style = list(fontWeight = "bold", fontSize = "11px"),
        y = -15,
        verticalAlign = "bottom"
      )
    ) %>%
    hc_add_series(
      type = "errorbar",
      name = "Palčki",
      data = lapply(1:nrow(podatki_cimax_sprememba), function(i) {
        list(
          low = 0,
          high = round(podatki_cimax_sprememba$sprememba_abs[i], 0),
          color = podatki_cimax_sprememba$barva[i]
        )
      }),
      whiskerLength = 0,
      stemWidth = 3,
      showInLegend = FALSE,
      enableMouseTracking = FALSE
    ) %>%
    hc_tooltip(
      shared = FALSE,
      pointFormat = "<b>{point.y:,.0f}</b> citatov",
      headerFormat = "<span style='font-size:10px'>{point.key}</span><br/>"
    ) %>%
    hc_legend(enabled = FALSE) %>%
    hc_exporting(
      enabled = TRUE,
      chartOptions = list(
        chart = list(backgroundColor = "#FFFFFF")
      )
    )
  
  # Izračunaj optimalne limite za H-indeks
  limiti_hind <- izracunaj_y_limiti(podatki_hind_sprememba$sprememba_abs)
  
  # Graf 4: Sprememba H-indeks (spodaj desno)
  graf_sprememba_hind <- highchart() %>%
    hc_chart(
      type = "line",
      height = 400,
      backgroundColor = "#FFFFFF"
    ) %>%
    hc_title(text = "Letna sprememba H-indeksa") %>%
    hc_xAxis(
      categories = podatki_hind_sprememba$Leto,
      title = list(text = "Leto")
    ) %>%
    hc_yAxis(
      title = list(text = "Sprememba H-indeksa"),
      labels = list(format = "{value:,.0f}"),
      min = limiti_hind$min,
      max = limiti_hind$max,
      plotLines = list(
        list(
          value = 0,
          color = "#333333",
          width = 2,
          zIndex = 5
        )
      )
    ) %>%
    hc_add_series(
      name = "Sprememba",
      data = lapply(1:nrow(podatki_hind_sprememba), function(i) {
        list(
          y = round(podatki_hind_sprememba$sprememba_abs[i], 0),
          color = podatki_hind_sprememba$barva[i],
          marker = list(
            enabled = TRUE,
            radius = 8,
            fillColor = podatki_hind_sprememba$barva[i]
          )
        )
      }),
      lineWidth = 0,
      states = list(hover = list(lineWidthPlus = 0)),
      dataLabels = list(
        enabled = TRUE,
        format = "{point.y:,.0f}",
        style = list(fontWeight = "bold", fontSize = "11px"),
        y = -15,
        verticalAlign = "bottom"
      )
    ) %>%
    hc_add_series(
      type = "errorbar",
      name = "Palčki",
      data = lapply(1:nrow(podatki_hind_sprememba), function(i) {
        list(
          low = 0,
          high = round(podatki_hind_sprememba$sprememba_abs[i], 0),
          color = podatki_hind_sprememba$barva[i]
        )
      }),
      whiskerLength = 0,
      stemWidth = 3,
      showInLegend = FALSE,
      enableMouseTracking = FALSE
    ) %>%
    hc_tooltip(
      shared = FALSE,
      pointFormat = "<b>{point.y:,.0f}</b>",
      headerFormat = "<span style='font-size:10px'>{point.key}</span><br/>"
    ) %>%
    hc_legend(enabled = FALSE) %>%
    hc_exporting(
      enabled = TRUE,
      chartOptions = list(
        chart = list(backgroundColor = "#FFFFFF")
      )
    )
  
  # Ustvari HTML panel s štirimi grafi v 2x2 mreži
  browsable(
    div(
      style = "margin-bottom: 40px;",
      # Naslov
      h3(naslov, style = "color: #3660a0; margin-bottom: 15px;"),
      
      # Vrstica 1: CImax in H-indeks
      div(
        style = "display: flex; gap: 20px; flex-wrap: wrap; margin-bottom: 20px;",
        # Graf 1: CImax
        div(
          style = "flex: 1; min-width: 400px;",
          graf_cimax
        ),
        # Graf 2: H-indeks
        div(
          style = "flex: 1; min-width: 400px;",
          graf_hind
        )
      ),
      
      # Vrstica 2: Spremembe
      div(
        style = "display: flex; gap: 20px; flex-wrap: wrap;",
        # Graf 3: Sprememba CImax
        div(
          style = "flex: 1; min-width: 400px;",
          graf_sprememba_cimax
        ),
        # Graf 4: Sprememba H-indeks
        div(
          style = "flex: 1; min-width: 400px;",
          graf_sprememba_hind
        )
      )
    )
  )
}


# Ustvari panel za citati in H-indeks iz združene tabele
panel_citati_hindeks <- create_panel_citati_hindeks(
  data = cimax_hind,
  naslov = "Citati in H-indeks",
  barva_cimax = "#3660a0",  # modra za citati
  barva_hind = "#2d8659"    # zelena za H-indeks
)

# Prikaži panel
panel_citati_hindeks


#---------------------------------------------------------

# === UNIVERZALNA UPORABA ===

# Funkcija za programe/skupine
create_panel_2x2_universal <- function(data, 
                                       kategorija_vrednost,
                                       naslov = kategorija_vrednost,
                                       barva = "#3660a0",
                                       kazalnik = "kaz_fte",
                                       kazalnik_label = "Kazalnik (točke/FTE)",
                                       stolpec_kategorija = "program") {
  
  # Preimenuj stolpec kategorije v "objave"
  data_temp <- data %>%
    rename(objave = all_of(stolpec_kategorija))
  
  # Pokliči funkcijo
  create_panel_2x2(
    data_temp,
    kategorija_vrednost,
    naslov,
    barva,
    kazalnik,
    kazalnik_label
  )
}

