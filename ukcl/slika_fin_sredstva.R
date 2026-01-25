


# Priprava podatkov
podatki <- tabela_sredstva
# Funkcija za formatiranje številk (slovensko)
format_slo <- function(x) {
  format(x, big.mark = ".", decimal.mark = ",", scientific = FALSE)
}

# Potrebni paketi
library(tidyverse)
library(highcharter)
library(htmltools)

# Funkcija za ustvarjanje posameznega grafa
create_chart <- function(data, column_name, title, color) {
  highchart() %>%
    hc_chart(
      type = "column", 
      height = 320,
      backgroundColor = "#FFFFFF",
      events = list(
        load = htmlwidgets::JS("function() { 
          var chart = this;
          setTimeout(function() { chart.reflow(); }, 100);
        }")
      )
    ) %>%
    hc_title(
      text = title, 
      style = list(fontSize = "16px", fontWeight = "bold")
    ) %>%
    hc_xAxis(
      categories = data$leto, 
      title = list(text = "Leto"),
      reversed = TRUE
    ) %>%
    hc_yAxis(
      title = list(text = "Sredstva (mio €)"),
      min = 0,
      startOnTick = TRUE,
      endOnTick = TRUE,
      labels = list(
        format = "{value:.1f}"
      )
    ) %>%
    hc_add_series(
      name = title,
      data = lapply(1:nrow(data), function(i) {
        list(
          y = round(data[[column_name]][i] / 1000000, 1),  # Pretvori v milijone
          dataLabels = list(
            enabled = ifelse(i == 1, TRUE, FALSE)  # Samo prvi stolpič (ker je reversed=TRUE)
          )
        )
      }),
      color = color,
      dataLabels = list(
        format = "{point.y:.1f}",
        style = list(
          fontWeight = "bold",
          fontSize = "11px"
        )
      )
    ) %>%
    hc_tooltip(
      pointFormat = "<b>{point.y:.1f} mio €</b>",
      headerFormat = "<span style='font-size:10px'>{point.key}</span><br/>"
    ) %>%
    hc_legend(enabled = FALSE) %>%
    hc_plotOptions(
      column = list(
        borderRadius = 3,
        minPointLength = 3
      )
    ) %>%
    hc_exporting(
      enabled = TRUE,
      chartOptions = list(
        chart = list(backgroundColor = "#FFFFFF")
      )
    )
}

# Ustvarjanje grafov
graf1 <- create_chart(podatki, "UKCL", "UKCL - Skupaj", "#3660a0")           
graf2 <- create_chart(podatki, "ARIS programi", "ARIS programi", "#4a7bc8")  
graf3 <- create_chart(podatki, "ARIS projekti", "ARIS projekti", "#5d8dd3")  
graf4 <- create_chart(podatki, "Klinična preskušanja", "Klinična preskušanja", "#7099db") 
graf5 <- create_chart(podatki, "Mednarodni projekti", "EU in mednarodni projekti", "#2c4d7c") 
graf6 <- create_chart(podatki, "Terciarni projekti", "Terciarni projekti", "#4e6fa6") 

# HTML output skladen s tvojim CSS-om
html_output <- tagList(
  tags$style(HTML("
    .chart-grid-container {
      display: grid;
      grid-template-columns: repeat(2, 1fr);
      gap: 20px;
      margin-top: 1.5rem;
      margin-bottom: 2rem;
    }
    .chart-grid-item {
      height: 350px;
      overflow: hidden;
    }
    .chart-grid-item .htmlwidget {
      margin: 0 !important;
      padding: 0 !important;
    }
  ")),
  tags$div(class = "chart-grid-container",
           tags$div(class = "chart-grid-item", graf2),
           tags$div(class = "chart-grid-item", graf3),
           tags$div(class = "chart-grid-item", graf4),
           tags$div(class = "chart-grid-item", graf5),
           tags$div(class = "chart-grid-item", graf6),
           tags$div(class = "chart-grid-item", graf1)
  )
)





#-------------------------------------------------------------------

# ------------------------------------
# ZUNANJA FINAČNA SREDSTVA
# ------------------------------------


# podatki
tabela_sredstva %>% 
  select(1, 6, 7) %>% 
  mutate(`Zunanja sredstva` = UKCL - `Terciarni projekti`) %>% 
  select(-UKCL) -> zun_sredstva


#' Nariši panel z dvema finančnima grafoma
#'
#' @param data Tibble/data.frame s stolpci: leto, `Terciarni projekti`, `Zunanja sredstva`
#' @param barva_zunanja Barva za graf zunanjih sredstev (privzeto modra)
#' @param barva_terciarni Barva za graf terciarnih projektov (privzeto svetlo siva)
#'
#' @return HTML div z dvema grafoma eden zraven drugega
narisi_panel_financna_sredstva <- function(data,
                                           barva_zunanja = "#3660a0",
                                           barva_terciarni = "#b0b0b0") {
  
  # Uredi podatke po letu (od najstarejšega do najnovejšega)
  data <- data %>%
    arrange(leto)
  
  # Pretvori v milijone
  data <- data %>%
    mutate(
      zunanja_mio = `Zunanja sredstva` / 1000000,
      terciarni_mio = `Terciarni projekti` / 1000000
    )
  
  # Graf 1: Zunanja sredstva
  graf_zunanja <- highchart() %>%
    hc_chart(
      type = "column",
      height = 400,
      backgroundColor = "#FFFFFF",
      events = list(
        load = htmlwidgets::JS("function() { 
          var chart = this;
          setTimeout(function() { chart.reflow(); }, 100);
        }")
      )
    ) %>%
    hc_title(text = "Zunanja sredstva") %>%
    hc_xAxis(
      categories = data$leto,
      title = list(text = "Leto")
    ) %>%
    hc_yAxis(
      title = list(text = "Sredstva (mio €)"),
      labels = list(
        format = "{value:.1f}",
        style = list(fontSize = "11px")
      )
    ) %>%
    hc_add_series(
      name = "Zunanja sredstva",
      data = lapply(1:nrow(data), function(i) {
        list(
          y = round(data$zunanja_mio[i], 1),
          dataLabels = list(
            enabled = ifelse(i == nrow(data), TRUE, FALSE)  # Samo zadnji stolpič
          )
        )
      }),
      color = barva_zunanja,
      dataLabels = list(
        format = "{point.y:.1f}",
        style = list(
          fontWeight = "bold",
          fontSize = "11px"
        )
      )
    ) %>%
    hc_tooltip(
      pointFormat = "<b>{point.y:.1f} mio €</b>",
      headerFormat = "<span style='font-size:10px'>{point.key}</span><br/>"
    ) %>%
    hc_legend(enabled = FALSE) %>%
    hc_exporting(
      enabled = TRUE,
      chartOptions = list(
        chart = list(backgroundColor = "#FFFFFF")
      )
    )
  
  # Graf 2: Terciarni projekti
  graf_terciarni <- highchart() %>%
    hc_chart(
      type = "column",
      height = 400,
      backgroundColor = "#FFFFFF",
      events = list(
        load = htmlwidgets::JS("function() { 
          var chart = this;
          setTimeout(function() { chart.reflow(); }, 100);
        }")
      )
    ) %>%
    hc_title(text = "Terciarni projekti") %>%
    hc_xAxis(
      categories = data$leto,
      title = list(text = "Leto")
    ) %>%
    hc_yAxis(
      title = list(text = "Sredstva (mio €)"),
      labels = list(
        format = "{value:.1f}",
        style = list(fontSize = "11px")
      )
    ) %>%
    hc_add_series(
      name = "Terciarni projekti",
      data = lapply(1:nrow(data), function(i) {
        list(
          y = round(data$terciarni_mio[i], 1),
          dataLabels = list(
            enabled = ifelse(i == nrow(data), TRUE, FALSE)  # Samo zadnji stolpič
          )
        )
      }),
      color = barva_terciarni,
      dataLabels = list(
        format = "{point.y:.1f}",
        style = list(
          fontWeight = "bold",
          fontSize = "11px"
        )
      )
    ) %>%
    hc_tooltip(
      pointFormat = "<b>{point.y:.1f} mio €</b>",
      headerFormat = "<span style='font-size:10px'>{point.key}</span><br/>"
    ) %>%
    hc_legend(enabled = FALSE) %>%
    hc_exporting(
      enabled = TRUE,
      chartOptions = list(
        chart = list(backgroundColor = "#FFFFFF")
      )
    )
  
  # Ustvari HTML panel z dvema grafoma
  browsable(
    div(
        # Dva grafa eden zraven drugega
      div(
        style = "display: flex; gap: 20px; flex-wrap: wrap;",
        # Graf 1: Zunanja sredstva (levo)
        div(
          style = "flex: 1 1 45%; min-width: 400px; max-width: 50%;",
          graf_zunanja
        ),
        # Graf 2: Terciarni projekti (desno)
        div(
          style = "flex: 1 1 45%; min-width: 400px; max-width: 50%;",
          graf_terciarni
        )
      )
    )
  )
}

# UPORABA
# -------

# Ustvari panel
panel_financna <- narisi_panel_financna_sredstva(zun_sredstva)


