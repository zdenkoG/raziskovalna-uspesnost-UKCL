

## Upoštevane točke

# na FTE

panel1_fte_leto <- create_panel_2x2(
  tabela_kaz_ukcl_leto, 
  "Upoštevane točke", 
  "Upoštevane točke", 
  "#3660a0",
  yaxis_label = "Točke",
  title_label = "Točke po letih",
  kazalnik = "kaz_fte",
  kazalnik_label = "Kazalnik (točke/FTE)"
)

# na RAZISKOVALCA

panel1_raz_leto <- create_panel_2x2(
  tabela_kaz_ukcl_leto, 
  "Upoštevane točke", 
  "Upoštevane točke", 
  "#b0b0b0",
  yaxis_label = "Točke",
  title_label = "Točke po letih",
  kazalnik = "kaz_raz",
  kazalnik_label = "Kazalnik (točke/raziskovalec)"
)


#-------------------------------------

## A'' - izjemni dosežki

# na FTE

panel2_fte_leto <- create_panel_2x2(
  tabela_kaz_ukcl_leto, 
  "A'' - izjemni dosežki", 
  "A'' - izjemni dosežki", 
  "#4a7bc8",
  yaxis_label = "Točke",
  title_label = "Točke po letih",
  kazalnik = "kaz_fte",
  kazalnik_label = "Kazalnik (točke/FTE)"
)


# na RAZISKOVALCA

panel2_raz_leto <- create_panel_2x2(
  tabela_kaz_ukcl_leto, 
  "A'' - izjemni dosežki", 
  "A'' - izjemni dosežki", 
  "#7a7a7a",
  yaxis_label = "Točke",
  title_label = "Točke po letih",
  kazalnik = "kaz_raz",
  kazalnik_label = "Kazalnik (točke/raziskovalec)"
)



## A' - zelo kvalitetni dosežki

# na FTE

panel3_fte_leto <- create_panel_2x2(
  tabela_kaz_ukcl_leto, 
  "A' - zelo kvalitetni dosežki", 
  "A' - zelo kvalitetni dosežki", 
  "#4a7bc8",
  yaxis_label = "Točke",
  title_label = "Točke po letih",
  kazalnik = "kaz_fte",
  kazalnik_label = "Kazalnik (točke/FTE)"
)


# na RAZISKOVALCA

panel3_raz_leto <- create_panel_2x2(
  tabela_kaz_ukcl_leto, 
  "A' - zelo kvalitetni dosežki", 
  "A' - zelo kvalitetni dosežki", 
  "#7a7a7a",
  yaxis_label = "Točke",
  title_label = "Točke po letih",
  kazalnik = "kaz_raz",
  kazalnik_label = "Kazalnik (točke/raziskovalec)"
)



## A1/2 - pomembni dosežki

# na FTE

panel4_fte_leto <- create_panel_2x2(
  tabela_kaz_ukcl_leto, 
  "A1/2 - pomembni dosežki", 
  "A1/2 - pomembni dosežki",  
  "#4a7bc8",
  yaxis_label = "Točke",
  title_label = "Točke po letih",
  kazalnik = "kaz_fte",
  kazalnik_label = "Kazalnik (točke/FTE)"
)


# na RAZISKOVALCA

panel4_raz_leto <- create_panel_2x2(
  tabela_kaz_ukcl_leto, 
  "A1/2 - pomembni dosežki", 
  "A1/2 - pomembni dosežki", 
  "#7a7a7a",
  yaxis_label = "Točke",
  title_label = "Točke po letih",
  kazalnik = "kaz_raz",
  kazalnik_label = "Kazalnik (točke/raziskovalec)"
)




