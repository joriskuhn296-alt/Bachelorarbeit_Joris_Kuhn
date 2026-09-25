# Baut das Quartalspanel 2005-Q1 bis 2019-Q4 fuer Deutschland und sieben Donorlaender. Es wird nicht imputiert.
# Eingabe: Rohdaten in data/raw (Quellen siehe README). Ausgabe: data/processed und output.

source("00_setup.R")
library(tidyverse)

RAW   <- "data/raw"
UNITS <- c(CFG$treated, CFG$donors)

# Quartalsangaben wie "2015-Q1" in Dezimaljahre umrechnen.
parse_qdate <- function(tp) {
  m <- str_match(as.character(tp), "(\\d{4})[-]?Q([1-4])")
  yr <- as.integer(m[, 2]); q <- as.integer(m[, 3])
  yr + (q - 1) / 4
}

# OECD-Quartalsdaten einlesen, filtern und auf Eindeutigkeit pruefen.
read_oecd_q <- function(path, varname, filters = list()) {
  raw <- read_csv(path, show_col_types = FALSE, guess_max = 20000,
                  name_repair = "unique_quiet")
  for (col in names(filters)) {
    if (!col %in% names(raw))
      stop(basename(path), ": Filterspalte '", col, "' fehlt.")
    raw <- raw |> filter(.data[[col]] %in% filters[[col]])
  }
  out <- raw |>
    transmute(country  = REF_AREA,
              date     = parse_qdate(TIME_PERIOD),
              variable = varname,
              value    = as.numeric(OBS_VALUE),
              src_file = basename(path)) |>
    filter(!is.na(date), !is.na(value))
  if (nrow(out) == 0)
    stop(basename(path), ": keine Zeilen nach dem Filtern.")
  dups <- out |> count(country, date) |> filter(n > 1)
  if (nrow(dups) > 0)
    stop(basename(path), ": ", nrow(dups),
         " Land-Quartal-Duplikate, weitere Dimension filtern.")
  out
}

# Dateinamen und Filter der OECD-Quartalsreihen festlegen.
q_specs <- list(
  list(file = "EmploymentRateQuarterly.csv", var = "emp_rate",
       filters = list(ADJUSTMENT = "Y")),
  list(file = "LabourForceParticipationQuarterly.csv", var = "lfpr",
       filters = list(AGE = "Y15T64", ADJUSTMENT = "Y")),
  list(file = "CPIQuarterly.csv", var = "inflation",
       filters = list())
)

# Alle Quartalsreihen einlesen und bei fehlender Datei abbrechen.
panel_q_raw <- q_specs |>
  map(\(s) {
    path <- file.path(RAW, s$file)
    if (!file.exists(path)) stop("Nicht gefunden: ", path)
    read_oecd_q(path, s$var, s$filters)
  }) |>
  list_rbind()

# Vierteljaehrliche VGR der OECD (QNA, Tabelle T0102) aus der mitgelieferten Rohdatei einlesen.
QNA_FILE <- file.path(RAW, "QNA_T0102.csv")
if (!file.exists(QNA_FILE)) stop("Nicht gefunden: ", QNA_FILE)

# Abbrechen, wenn die Datei nicht der in der Arbeit verwendeten Fassung entspricht.
if (unname(tools::md5sum(QNA_FILE)) != "03911d631a7032a82dc2866e5551bb11")
  stop(QNA_FILE, " weicht von der verwendeten Fassung ab (Abruf 24.09.2026).")



# Investitionsquote und reales BIP-Wachstum aus den VGR-Rohdaten berechnen.
read_qna <- function(path) {
  raw <- read_csv(path, show_col_types = FALSE, guess_max = 50000,
                  name_repair = "unique_quiet")
  
  # Pruefen, ob alle benoetigten Dimensionen vorhanden sind.
  need <- c("REF_AREA", "SECTOR", "TRANSACTION", "ACTIVITY", "EXPENDITURE",
            "PRICE_BASE", "TRANSFORMATION", "ADJUSTMENT", "TIME_PERIOD", "OBS_VALUE")
  miss <- setdiff(need, names(raw))
  if (length(miss) > 0)
    stop("QNA-CSV: Spalten fehlen: ", paste(miss, collapse = ", "))
  
  # Saisonbereinigte Werte behalten und Datumsangaben umrechnen.
  base <- raw |>
    filter(ADJUSTMENT == "Y") |>
    transmute(country = REF_AREA, date = parse_qdate(TIME_PERIOD),
              SECTOR, TRANSACTION, ACTIVITY, EXPENDITURE,
              PRICE_BASE, TRANSFORMATION, value = as.numeric(OBS_VALUE)) |>
    filter(!is.na(date), !is.na(value))
  
  # Genau eine Reihe ueber alle Dimensionen auswaehlen und auf Eindeutigkeit pruefen.
  pick <- function(txn, sec, act, exp, pb, tf) {
    s <- base |>
      filter(TRANSACTION == txn, SECTOR == sec, ACTIVITY == act,
             EXPENDITURE == exp, PRICE_BASE == pb, TRANSFORMATION == tf) |>
      select(country, date, value)
    id <- paste(txn, sec, act, exp, pb, tf, sep = "/")
    if (nrow(s) == 0)
      stop("QNA ", id, ": keine Zeilen.")
    if (nrow(s |> count(country, date) |> filter(n > 1)) > 0)
      stop("QNA ", id, ": Duplikate pro Land und Quartal.")
    s
  }
  
  # Investitionsquote als nominale Bruttoanlageinvestitionen in Prozent des nominalen BIP berechnen.
  gdp_v  <- pick("B1GQ", "S1", "_Z", "_Z", "V", "N")
  gfcf_v <- pick("P51G", "S1", "_T", "_Z", "V", "N")
  inv <- inner_join(gfcf_v |> rename(gfcf = value),
                    gdp_v  |> rename(gdp  = value), by = c("country", "date")) |>
    mutate(value = gfcf / gdp * 100, variable = "inv_rate")
  
  # Unplausible Investitionsquoten abfangen.
  bad <- inv |> filter(value < 10 | value > 45)
  if (nrow(bad) > 0)
    stop("inv_rate unplausibel: ", nrow(bad), " Werte ausserhalb 10-45 %.")
  
  # Preisbasis fuer das reale BIP waehlen und protokollieren.
  pb_real <- if (nrow(base |> filter(TRANSACTION == "B1GQ", SECTOR == "S1",
                                     PRICE_BASE == "L")) > 0) "L" else "LR"
  message("Preisbasis reales BIP: ", pb_real)
  
  # Reales BIP-Wachstum gegenueber dem Vorjahresquartal berechnen.
  gdp_r <- pick("B1GQ", "S1", "_Z", "_Z", pb_real, "N") |> arrange(country, date)
  grow <- gdp_r |> group_by(country) |>
    mutate(value = (value / lag(value, 4) - 1) * 100, variable = "gdp_growth") |>
    ungroup() |> filter(!is.na(value))
  
  bind_rows(inv  |> transmute(country, date, variable, value),
            grow |> transmute(country, date, variable, value)) |>
    mutate(src_file = basename(path))
}

panel_qna <- read_qna(QNA_FILE)

# Jahreswerte auf alle vier Quartale des jeweiligen Jahres uebertragen.
replicate_to_q <- function(df_annual) {
  tidyr::crossing(df_annual, q = 1:4) |>
    mutate(date = year + (q - 1) / 4) |>
    select(country, date, variable, value) |>
    mutate(src_file = "annual_repliziert")
}

# Kaufkraftbereinigtes BIP pro Kopf aus der Weltbank-Datei einlesen.
read_worldbank_annual <- function(path, varname) {
  read_csv(path, skip = 4, show_col_types = FALSE, name_repair = "unique_quiet") |>
    select(country = `Country Code`, matches("^\\d{4}$")) |>
    pivot_longer(-country, names_to = "year", values_to = "value") |>
    mutate(year = as.integer(year), value = as.numeric(value),
           variable = varname) |>
    filter(!is.na(year), !is.na(value))
}
gdp_pc_annual <- read_worldbank_annual(file.path(RAW, "GDPperCapita.csv"), "gdp_pc")

# Gewerkschaftsdichte aus ICTWSS einlesen und Fehlcodes als fehlend setzen.
read_ictwss_ud <- function(path) {
  raw <- read_csv(path, show_col_types = FALSE, guess_max = 5000)
  clean <- \(x) { x <- as.numeric(x); if_else(x %in% c(-88, -99), NA_real_, x) }
  raw |>
    transmute(country = iso3, year = as.integer(year),
              value = coalesce(clean(UD), clean(UD_s)), variable = "ud") |>
    filter(!is.na(year), !is.na(value))
}
ud_annual <- read_ictwss_ud(file.path(RAW, "ICTWSS_v2.csv"))

# Jahresreihen auf Quartale uebertragen und zusammenfuehren.
panel_repl <- bind_rows(
  replicate_to_q(gdp_pc_annual),
  replicate_to_q(ud_annual)
)

# Alle Quellen zusammenfuehren, auf Fenster und acht Einheiten beschraenken und t setzen.
panel_long <- bind_rows(panel_q_raw, panel_qna, panel_repl) |>
  filter(country %in% UNITS,
         date >= CFG$date_min, date <= CFG$date_max) |>
  mutate(t = date_to_t(date)) |>
  relocate(country, date, t)

# Doppelte Beobachtungen nach dem Zusammenfuehren abfangen.
dup <- panel_long |> count(country, date, variable) |> filter(n > 1)
if (nrow(dup) > 0) { print(dup); stop("Duplikate nach Merge.") }

# Panel auf das volle Raster erweitern, ins Breitformat bringen und als data.frame fuer Synth ablegen.
t_date <- panel_long |> distinct(t, date) |> arrange(t)
panel_scm <- panel_long |>
  select(country, t, variable, value) |>
  complete(country, t = CFG$t_min:CFG$t_max, variable) |>
  left_join(t_date, by = "t") |>
  select(country, t, date, variable, value) |>
  pivot_wider(names_from = variable, values_from = value) |>
  arrange(country, t) |>
  mutate(unit_id = as.integer(factor(country))) |>
  relocate(unit_id, country, t, date) |>
  as.data.frame()

# Panel speichern
saveRDS(panel_scm,  "data/processed/panel_scm.rds")
write_csv(panel_scm, "data/processed/panel_scm.csv")

# Quellenprotokoll mit Abdeckung je Variable schreiben
panel_long |>
  group_by(variable, src_file) |>
  summarise(n_laender = n_distinct(country),
            von = min(date), bis = max(date), n_obs = n(), .groups = "drop") |>
  write_csv("output/quellen_log.csv")

message("Panel gebaut: ", n_distinct(panel_scm$country), " Laender x ",
        n_distinct(panel_scm$t), " Quartale, ", ncol(panel_scm) - 4, " Variablen.")