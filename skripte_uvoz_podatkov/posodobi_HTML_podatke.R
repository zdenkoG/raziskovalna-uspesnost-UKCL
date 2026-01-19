

# ================================================
# Pridobivanje podatkov iz HTML datotek
# ================================================

# HTML se izvozi iz sicrisa -> Vrednotenje bibliografskih kazalcev raziskovalne uspešnosti po metodologiji ARIS
# Izbere se, da se izpiše seznam avtorjev
# Seznam del NI potreben za: Programe in Projekte
# Seznam del JE potreben za UKCL (seznam z znanstvenimi deli + seznam z A" deli)
# HTML-ji se shranijo v ustrezne mape
# Podatki se posodobijo 1x letno!
# Podatki se posodobijo ROČNO


# -----------------------------------------
# Izvoz 1. table (raziskovalci) iz HTML
# -----------------------------------------


# Define folder path with HTML files
html_folder <- "./UKCL_HTML/"                                 ## POPRAVI POT ODVISNO KATERE HTML-je SE POSODABLJA!

# Get all HTML files in folder
html_files <- list.files(html_folder, pattern = "\\.html$", full.names = TRUE)

print(paste("Najdenih HTML datotek:", length(html_files)))

# Function to extract table1 from one HTML file
extract_table1_from_html <- function(html_file) {
  
  # Read HTML
  html <- read_html(html_file)
  
  # Extract group code from H1
  h1_text <- html %>% 
    html_node("body h1") %>% 
    html_text()
  
  sifra <- str_extract(h1_text, "(?<=\\[).+?(?=\\])")
  
  # Extract year from H2
  h2_text <- html %>% 
    html_node("body h2") %>% 
    html_text()
  
  leto <- str_extract(h2_text, "(?<=\\().+?(?=\\))")
  
  # Extract first table
  table1 <- html %>% 
    html_table(fill = TRUE) %>% 
    .[[1]] %>% 
    mutate(
      sifra_skupine = sifra,
      leto = leto,
      source_file = basename(html_file),  # Add filename for tracking
      .before = 1
    )
  
  return(table1)
}


# Apply function to all HTML files and combine into one table
# Use map_dfr from purrr package to combine rows automatically
all_tables <- map_dfr(html_files, extract_table1_from_html)


# ---------------------------------------------
# Izvoz 4. tabele (točke in citiranost) iz HTML
# ---------------------------------------------


# Define folder path with HTML files
html_folder <- "./UKCL_HTML/"

# Get all HTML files in folder
html_files <- list.files(html_folder, pattern = "\\.html$", full.names = TRUE)

print(paste("Najdenih HTML datotek:", length(html_files)))

# Function to use first row as header
use_first_row_as_header <- function(df) {
  if (nrow(df) > 1) {
    # Extract column names from first row
    new_names <- as.character(df[1, ])
    
    # Remove first row and set new column names
    df <- df[-1, ]
    names(df) <- new_names
    
    # Check for duplicate names and fix them
    if (any(duplicated(names(df)))) {
      names(df) <- make.unique(names(df), sep = "_")
    }
  }
  return(df)
}

# Function to extract table4a (rows 1-6) from one HTML file
extract_table4a_from_html <- function(html_file) {
  
  # Read HTML
  html <- read_html(html_file)
  
  # Extract group code from H1
  h1_text <- html %>% 
    html_node("body h1") %>% 
    html_text()
  
  sifra <- str_extract(h1_text, "(?<=\\[).+?(?=\\])")
  
  # Extract year from H2
  h2_text <- html %>% 
    html_node("body h2") %>% 
    html_text()
  
  leto <- str_extract(h2_text, "(?<=\\().+?(?=\\))")
  
  # Extract fourth table
  table4_raw <- html %>% 
    html_table(fill = TRUE) %>% 
    .[[4]]
  
  # Fix duplicate column names in raw table
  if (any(duplicated(names(table4_raw)))) {
    names(table4_raw) <- make.unique(names(table4_raw), sep = "_")
  }
  
  # Create first slice (rows 1-6) and use row 1 as header
  table4a <- table4_raw %>% 
    slice(1:6) %>% 
    use_first_row_as_header() %>% 
    mutate(
      sifra_skupine = sifra,
      leto = leto,
      source_file = basename(html_file),
      .before = 1
    )
  
  return(table4a)
}



# Function to extract table4b (rows 7-10) from one HTML file
extract_table4b_from_html <- function(html_file) {
  
  # Read HTML
  html <- read_html(html_file)
  
  # Extract group code from H1
  h1_text <- html %>% 
    html_node("body h1") %>% 
    html_text()
  
  sifra <- str_extract(h1_text, "(?<=\\[).+?(?=\\])")
  
  # Extract year from H2
  h2_text <- html %>% 
    html_node("body h2") %>% 
    html_text()
  
  leto <- str_extract(h2_text, "(?<=\\().+?(?=\\))")
  
  # Extract fourth table
  table4_raw <- html %>% 
    html_table(fill = TRUE) %>% 
    .[[4]]
  
  # Fix duplicate column names in raw table
  if (any(duplicated(names(table4_raw)))) {
    names(table4_raw) <- make.unique(names(table4_raw), sep = "_")
  }
  
  # Create second slice (rows 7-10) and use row 7 as header
  table4b <- table4_raw %>% 
    slice(7:10) %>% 
    use_first_row_as_header() %>% 
    mutate(
      sifra_skupine = sifra,
      leto = leto,
      source_file = basename(html_file),
      .before = 1
    ) %>% 
    select(-5)
  
  return(table4b)
}

# Apply functions to all HTML files
# table4a: rows 1-6 with row 1 as header
all_tables_4a <- map_dfr(html_files, extract_table4a_from_html)    ## Upoštevane točke

# table4b: rows 7-10 with row 7 as header
all_tables_4b <- map_dfr(html_files, extract_table4b_from_html)    ## citiranost

