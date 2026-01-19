

library(highcharter)
library(htmltools)

# Priprava podatkov
podatki <- tabela_sredstva
# Funkcija za formatiranje številk (slovensko)
format_slo <- function(x) {
  format(x, big.mark = ".", decimal.mark = ",", scientific = FALSE)
}

# Funkcija za ustvarjanje posameznega grafa
create_chart <- function(data, column_name, title, color) {
  highchart() %>%
    hc_chart(type = "column", height = 320) %>%
    hc_title(text = title, style = list(fontSize = "16px", fontWeight = "bold")) %>%
    hc_xAxis(categories = data$leto, 
             title = list(text = "Leto"),
             reversed = TRUE) %>%
    hc_yAxis(title = list(text = "Znesek (mio €)"),
             min = 0,
             startOnTick = TRUE,
             endOnTick = TRUE,
             labels = list(
               formatter = JS("function() { 
                 var val = this.value / 1000000;
                 var formatted = val.toFixed(1);
                 return formatted.replace('.', ',');
               }")
             )) %>%
    hc_add_series(name = title, 
                  data = data[[column_name]], 
                  color = color,
                  dataLabels = list(
                    enabled = FALSE
                  )) %>%
    hc_tooltip(
      useHTML = TRUE,
      formatter = JS("function() { 
        return '<b>' + Highcharts.numberFormat(this.y / 1000000, 2, ',', '.') + ' mio €</b>';
      }")
    ) %>%
    hc_legend(enabled = FALSE) %>%
    hc_plotOptions(
      column = list(
        borderRadius = 3,
        minPointLength = 3
      )
    )
}

# Ustvarjanje grafov
graf1 <- create_chart(podatki, "UKCL", "UKCL - Skupaj", "#3660a0")           # navbar barva - temno modra
graf2 <- create_chart(podatki, "ARIS programi", "ARIS programi", "#4a7bc8")  # srednje modra
graf3 <- create_chart(podatki, "ARIS projekti", "ARIS projekti", "#5d8dd3")  # svetlejša modra
graf4 <- create_chart(podatki, "Klinična preskušanja", "Klinična preskušanja", "#7099db") # še svetlejša
graf5 <- create_chart(podatki, "Mednarodni projekti", "EU in mednarodni projekti", "#2c4d7c") # temnejša modra
graf6 <- create_chart(podatki, "Terciarni projekti", "Terciarni projekti", "#4e6fa6") # srednje temna


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
