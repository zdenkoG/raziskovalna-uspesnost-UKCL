
# ==========================
# Funkcije za izpis TABEL
# ==========================


# -----------------------
# Tabela: Osnovni podatki
# -----------------------


colnames(tabela_osn_podatki) <- c(
  "org",
  "Raziskovalci",
  "Status raziskovalca",
  "FTE*",
  "Strokovni sodelavci",
  "Programske skupine",
  "Raziskovalne skupine",
  "Št. ARIS projektov**"
)

# Kreiranje tabele
tbl_01 <- reactable(
  tabela_osn_podatki,
  columns = list(
    org = colDef(align = "left", name = "",
                 minWidth = 120)
  ),
  defaultColDef = colDef(
    align = "center",
    cell = format_numeric_column,
    headerStyle = list(fontSize = "14px")
  )
)



# ----------------------------------
# Tabela: Vrednotenje in citiranost
# ----------------------------------


colnames(tabela_vrednot_ukc) <- c(
  "org",
  "Upoš. tč.",
  'A"',
  "A'",
  "A1/2",
  "CI10",
  "CImax",
  "h-indeks"
)

tbl_02 <- tabela_vrednot_ukc %>% 
  reactable(
    columns = list(
      org = colDef(align = "left", name = "",
                   minWidth = 120)
    ),
    defaultColDef = colDef(
      cell = format_numeric_column,
      align = "center"
  )
  )



# ---------------------------------
# Tabela osnovni kazalniki 
# ---------------------------------

colnames(tabela_osn_kaz) <- c(
  "kaj",
  "Točke",
  "Točke/Razisk*",
  "Točke/FTE**"
)
 

tbl_03 <- reactable(
  tabela_osn_kaz,
  columns = list(
    kaj = colDef(name = "", align = "left")
  ),
  defaultColDef = colDef(
    cell = format_numeric_column,
    align = "center"
  )
) 



# ----------------------------------
# Tabela finančna sredstva
# ----------------------------------


# Izračunaj min/max za gradienta
values_2_5 <- unlist(tabela_sredstva[, 2:5])
global_min_2_5 <- min(values_2_5, na.rm = TRUE)
global_max_2_5 <- max(values_2_5, na.rm = TRUE)

terciarni_min <- min(tabela_sredstva$`Terciarni projekti`, na.rm = TRUE)
terciarni_max <- max(tabela_sredstva$`Terciarni projekti`, na.rm = TRUE)



tbl_04 <- reactable(
  tabela_sredstva,
  defaultColDef = colDef(
    cell = format_numeric_eur,
    minWidth = 120,
    align = "center",
    headerStyle = list(
      background = "#3660a0",
      color = "white",
      fontWeight = "bold",
      fontSize = "12px"
    )
  ),
  columns = list(
    leto = colDef(
      name = "Leto",
      align = "center",
      minWidth = 80,
      cell = function(value) {
        as.character(value)
      }
    ),
    `ARIS programi` = colDef(
      name = "ARIS programi",
      style = function(value) {
        bg_color <- heatmap_color(value, global_min_2_5, global_max_2_5)
        list(background = bg_color, fontWeight = "500")
      }
    ),
    `ARIS projekti` = colDef(
      name = "ARIS projekti",
      style = function(value) {
        bg_color <- heatmap_color(value, global_min_2_5, global_max_2_5)
        list(background = bg_color, fontWeight = "500")
      }
    ),
    `Klinična preskušanja` = colDef(
      name = "Klinična preskušanja",
      style = function(value) {
        bg_color <- heatmap_color(value, global_min_2_5, global_max_2_5)
        list(background = bg_color, fontWeight = "500")
      }
    ),
    `Mednarodni projekti` = colDef(
      name = "Mednarodni projekti",
      style = function(value) {
        bg_color <- heatmap_color(value, global_min_2_5, global_max_2_5)
        list(background = bg_color, fontWeight = "500")
      }
    ),
    `Terciarni projekti` = colDef(
      name = "Terciarni projekti",
      style = function(value) {
        # Uporabi svoj gradient za Terciarne projekte
        bg_color <- heatmap_color(value, terciarni_min, terciarni_max)
        list(background = bg_color, fontWeight = "500",
             borderLeft = "2.5px solid #3660a0")
      }
    ),
    UKCL = colDef(
      name = "UKCL (SKUPAJ)",
      style = list(
        fontWeight = "bold",
        color = "#3660a0",
        fontSize = "16px",
        borderLeft = "2.5px solid #3660a0"
      )
    )
  ),
  bordered = TRUE,
  striped = FALSE,
  highlight = TRUE,
  compact = TRUE
)



# ----------------------------------------
# Tekoči projeki na 31.12. - po sklopih
# ---------------------------------------


# Definiraj numerične stolpce
numeric_cols <- setdiff(names(rezultat), "sklop")

# Ustvari reactable tabelo z heatmap po vrsticah
tab_05 <- reactable(rezultat,
                    columns = list(
                      sklop = colDef(
                        align = "right", 
                        name = "Sklop",
                        minWidth = 120,
                        style = list(
                          color = "black"
                        )
                      )
                    ),
                    defaultColDef = colDef(
                      minWidth = 80,
                      # Heatmap styling - izračunaj min/max za vsako vrstico posebej
                      style = function(value, index) {
                        # Preveri če je vrednost NA ali ni numerična
                        if (is.na(value) || !is.numeric(value)) {
                          return(list(
                            background = "#f8f9fa",  # Svetlo siva za NA
                            color = "#adb5bd",       # Siva barva teksta
                            fontStyle = "italic"
                          ))
                        }
                        
                        # Pridobi vrednosti za trenutno vrstico (samo numerični stolpci)
                        row_values <- unlist(rezultat[index, numeric_cols])
                        # Odstrani NA vrednosti
                        row_values <- row_values[!is.na(row_values)]
                        
                        # Izračunaj min in max za to vrstico
                        min_val <- min(row_values, na.rm = TRUE)
                        max_val <- max(row_values, na.rm = TRUE)
                        
                        # Dodaj 20% pribitek na max vrednost za svetlejši gradient
                        max_val_adjusted <- max_val * 1.20
                        
                        # Za veljavne numerične vrednosti uporabi heatmap
                        list(
                          background = heatmap_color(value, min_val, max_val_adjusted),
                          fontWeight = "500"
                        )
                      },
                      # Formatiranje prikaza NA vrednosti
                      cell = function(value) {
                        if (is.na(value)) return("—")  # Em dash za NA
                        value
                      }
                    ),
                    striped = FALSE,
                    bordered = TRUE,
                    highlight = TRUE,
                    compact = TRUE,
                    searchable = TRUE,
                    theme = reactableTheme(
                      cellStyle = list(borderBottom = "1px solid #ddd")
                    )
)


# --------------------------------
# Tabela Raziskovalne skupine UKCL
# --------------------------------

colnames(tabela_raz_sk) <- c(
  "Šifra",
  "Naziv",
  "Vodja",
  "Št. članov",
  "Št. raz.",
  "FTE*",
  "Program**"
)

# Dodaj prazen stolpec za številke
kc_skup_num <- tabela_raz_sk %>%
  mutate(` ` = "", .before = 1)  # Prazen stolpec

tbl_06 <- reactable(
  kc_skup_num %>%  arrange(`Šifra`),
  columns = list(
    ` ` = colDef(
      name = "#",
      width = 60,
      align = "center",
      sortable = FALSE,
      cell = JS("function(cellInfo) { return cellInfo.viewIndex + 1; }"),
      style = list(color = "#666")
    ),
    Naziv = colDef(align = "left", minWidth = 150,
                   sortable = FALSE),
    Vodja = colDef(align = "left", minWidth = 80,
                   sortable = FALSE)
  ),
  striped = FALSE,
  bordered = TRUE,
  highlight = TRUE,
  compact = TRUE,
  searchable = TRUE,
  defaultColDef = colDef(align = "center", minWidth = 50,
                         style = list(fontSize = "14px"),
                         headerStyle = list(fontSize = "14px")),
  pagination = FALSE,
  height = 500
)



# ----------------------------------------------------
# Tabela: Programske skupine - UKCL vodilina ustanova
# ----------------------------------------------------


colnames(tabela_programi_ukcl) <- c(
  "Šifra",
  "Naziv",
  "Vodja",
  "Začetek",
  "Konec",
  "Št. članov",
  "Št. raz.",
  "FTE*",
  "Št. org**"
)



# Dodaj prazen stolpec za številke
kc_prog_num <- tabela_programi_ukcl %>%
  mutate(` ` = "", .before = 1)  # Prazen stolpec

tbl_07 <- reactable(
  kc_prog_num %>%  arrange(desc(`Začetek`)),
  columns = list(
    ` ` = colDef(
      name = "#",
      width = 60,
      align = "center",
      sortable = FALSE,
      cell = JS("function(cellInfo) { return cellInfo.viewIndex + 1; }"),
      style = list(color = "#666")
    ),
    Naziv = colDef(align = "left", minWidth = 150,
                   sortable = FALSE),
    Vodja = colDef(align = "left", minWidth = 80,
                   sortable = FALSE),
    `Začetek` = colDef(cell = function(value) {
      format(value, "%b %Y")
    }),
    Konec = colDef(cell = function(value) {
      format(value, "%b %Y")
    })
  ),
  striped = FALSE,
  bordered = TRUE,
  highlight = TRUE,
  compact = TRUE,
  searchable = TRUE,
  defaultColDef = colDef(align = "center", minWidth = 50,
                         style = list(fontSize = "14px"),
                         headerStyle = list(fontSize = "14px")),
  pagination = FALSE,
  height = 500
)


# -----------------------------------
# Programske skupine - UKCL sodeluje
# -----------------------------------

colnames(tabela_programi_zunanji) <- c(
  "Šifra",
  "Naziv",
  "Vodja",
  "Vodilna org.",
  "Začetek",
  "Konec"
)

# Dodaj prazen stolpec za številke
kc_prog_num <- tabela_programi_zunanji %>%
  mutate(` ` = "", .before = 1)  # Prazen stolpec

tbl_08 <- reactable(
  tabela_programi_zunanji,
  columns = list(
    ` ` = colDef(
      name = "#",
      width = 60,
      align = "center",
      sortable = FALSE,
      cell = JS("function(cellInfo) { return cellInfo.viewIndex + 1; }"),
      style = list(color = "#666")
    ),
    Naziv = colDef(align = "left", minWidth = 150,
                   sortable = FALSE),
    Vodja = colDef(align = "left", minWidth = 80,
                   sortable = FALSE),
    `Vodilna org.` = colDef(align = "left", minWidth = 80),
    `Začetek` = colDef(cell = function(value) {
      format(value, "%b %Y")
    }),
    Konec = colDef(cell = function(value) {
      format(value, "%b %Y")
    })
  ),
  striped = FALSE,
  bordered = TRUE,
  highlight = TRUE,
  compact = TRUE,
  searchable = TRUE,
  defaultColDef = colDef(align = "center", minWidth = 50,
                         style = list(fontSize = "14px"),
                         headerStyle = list(fontSize = "14px")),
  pagination = FALSE,
  height = 500
)
