# Raziskovalna uspešnost - Quarto spletna stran

Spletna stran za prikaz raziskovalne uspešnosti bolnišnice.

## Struktura projekta

```
raziskovalna-uspesnost/
├── _quarto.yml               # Glavna konfiguracija Quarto
├── index.qmd                 # Domača stran
├── ukcl.qmd                  # UKCL - celotna bolnišnica
├── raziskovalne-skupine.qmd  # Raziskovalne skupine
├── programske-skupine.qmd    # Programske skupine
├── raziskovalci.qmd          # Posamezni raziskovalci
├── styles.css                # Dodatni CSS stili
├── custom.scss               # SCSS prilagoditve teme
├── images/                   # Mapa za slike (logo, favicon)
├── data/                     # MAPA S PODATKI
│   ├── ukcl_letni.csv
│   ├── projekti_tipi.csv
│   ├── raziskovalne_skupine.csv
│   ├── programske_skupine.csv
│   └── raziskovalci.csv
└── README.md
```

## Potrebni R paketi

```r
install.packages(c(
  "readr",       # Branje CSV datotek
  "dplyr",       # Manipulacija podatkov
  "highcharter", # Interaktivni grafi z izvozom
  "DT",          # Interaktivne tabele z izvozom
  "bslib",       # Bootstrap komponente
  "bsicons",     # Bootstrap ikone
  "htmltools",   # HTML elementi
  "crosstalk"    # Povezovanje filtrov s tabelami
))
```

## Zagon strani

```bash
quarto render
```

Ali za predogled:

```bash
quarto preview
```

## Izvoz podatkov

### Iz grafov (Highcharter)

1. Premaknite miško nad graf
2. V zgornjem desnem kotu se prikaže meni (☰)
3. Kliknite in izberite:
   - **Download PNG** - slika PNG
   - **Download JPEG** - slika JPEG
   - **Download PDF** - PDF dokument
   - **Download SVG** - vektorska slika
   - **View data table** - prikaz podatkov
   - **Download CSV** - izvoz v CSV
   - **Download XLS** - izvoz v Excel

### Iz tabel (DT)

Nad vsako tabelo so gumbi:
- **Copy** - kopiraj v odložišče
- **CSV** - izvozi v CSV
- **Excel** - izvozi v Excel
- **PDF** - izvozi v PDF

## Filtriranje

Filter v spustnem seznamu (crosstalk) deluje **samo na tabele**, ne na grafe.
Za popolno interaktivnost z grafi bi potrebovali Shiny server.

## Posodabljanje podatkov

1. Zamenjajte CSV datoteke v mapi `data/`
2. Zaženite `quarto render`

## Prilagoditve

### Barve
Uredite `custom.scss`:
```scss
$primary: #1e3a5f;
$success: #28a745;
$info: #17a2b8;
$warning: #ffc107;
```

### Logo
Dodajte `logo.png` in `favicon.png` v mapo `images/`
