
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
                                           naslov = "Letna sprememba točk") {
  
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
    hc_title(text = naslov) %>%
    hc_xAxis(
      categories = podatki$leto,
      title = list(text = "Leto")
    ) %>%
    hc_yAxis(
      title = list(text = "Sprememba točk"),
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
                             kazalnik = "kaz_fte",
                             kazalnik_label = "Kazalnik (točke/FTE)") {
  
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
    hc_title(text = "Točke po letih") %>%
    hc_xAxis(
      categories = podatki_kat$leto,
      title = list(text = "Leto")
    ) %>%
    hc_yAxis(
      title = list(text = "Točke"),
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
    "Letna sprememba točk"
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

# UPORABA
# -------

# === KAZALNIK FTE (kaz_fte) - modre barve ===

panel1_fte <- create_panel_2x2(
  tabela_kaz_tocke_ukcl, 
  "Upoštevane točke", 
  "Upoštevane točke", 
  "#3660a0",
  kazalnik = "kaz_fte",
  kazalnik_label = "Kazalnik (točke/FTE)"
)

panel2_fte <- create_panel_2x2(
  tabela_kaz_tocke_ukcl, 
  "A'' - izjemni dosežki", 
  "A'' - izjemni dosežki", 
  "#4a7bc8",
  kazalnik = "kaz_fte",
  kazalnik_label = "Kazalnik (točke/FTE)"
)

panel3_fte <- create_panel_2x2(
  tabela_kaz_tocke_ukcl, 
  "A' - zelo kvalitetni dosežki", 
  "A' - zelo kvalitetni dosežki", 
  "#5d8dd3",
  kazalnik = "kaz_fte",
  kazalnik_label = "Kazalnik (točke/FTE)"
)

panel4_fte <- create_panel_2x2(
  tabela_kaz_tocke_ukcl, 
  "A1/2 - pomembni dosežki", 
  "A1/2 - pomembni dosežki", 
  "#7099db",
  kazalnik = "kaz_fte",
  kazalnik_label = "Kazalnik (točke/FTE)"
)

# === KAZALNIK RAZISKOVALCI (kaz_raz) - sive barve ===

panel1_raz <- create_panel_2x2(
  tabela_kaz_tocke_ukcl, 
  "Upoštevane točke", 
  "Upoštevane točke", 
  "#b0b0b0",
  kazalnik = "kaz_raz",
  kazalnik_label = "Kazalnik (točke/raziskovalec)"
)

panel2_raz <- create_panel_2x2(
  tabela_kaz_tocke_ukcl, 
  "A'' - izjemni dosežki", 
  "A'' - izjemni dosežki", 
  "#7a7a7a",
  kazalnik = "kaz_raz",
  kazalnik_label = "Kazalnik (točke/raziskovalec)"
)

panel3_raz <- create_panel_2x2(
  tabela_kaz_tocke_ukcl, 
  "A' - zelo kvalitetni dosežki", 
  "A' - zelo kvalitetni dosežki", 
  "#9a9a9a",
  kazalnik = "kaz_raz",
  kazalnik_label = "Kazalnik (točke/raziskovalec)"
)

panel4_raz <- create_panel_2x2(
  tabela_kaz_tocke_ukcl, 
  "A1/2 - pomembni dosežki", 
  "A1/2 - pomembni dosežki", 
  "#b0b0b0",
  kazalnik = "kaz_raz",
  kazalnik_label = "Kazalnik (točke/raziskovalec)"
)

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

