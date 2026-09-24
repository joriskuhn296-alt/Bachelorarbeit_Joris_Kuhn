# Gemeinsame Konfiguration und Hilfsfunktionen. Wird von allen Skripten zu Beginn geladen.

# Abbrechen, wenn das Arbeitsverzeichnis nicht das Projektverzeichnis ist.
if (!file.exists("00_setup.R"))
  stop("Arbeitsverzeichnis muss das Projektverzeichnis sein (dort liegt 00_setup.R).")

# Abbrechen, wenn benötigte Pakete fehlen.
pkgs <- c("tidyverse", "Synth", "kableExtra", "augsynth")
fehlt <- pkgs[!vapply(pkgs, requireNamespace, logical(1), quietly = TRUE)]
if (length(fehlt) > 0)
  stop("Fehlende Pakete: ", paste(fehlt, collapse = ", "), ". Mit renv::restore() installieren.")

# Designparameter der Hauptspezifikation festlegen.
CFG <- list(
  treated    = "DEU",
  donors     = c("AUT", "DNK", "FIN", "ISL", "ITA", "NOR", "SWE"),
  outcome    = "emp_rate",
  predictors = c("gdp_pc", "gdp_growth", "inv_rate", "inflation", "lfpr", "ud"),
  date_min   = 2005.00,
  date_max   = 2019.75,
  treat_date = 2015.00,
  t_min      = 1L,
  t_max      = 60L,
  t_treat    = 41L,
  pre_t      = 1:40,
  seed       = 20150101L
)

# Zufallszahlengenerator fuer alle Skripte festlegen.
set.seed(CFG$seed)

# Ordnerstruktur fuer Zwischen- und Ergebnisdateien anlegen.
dir.create("data/processed", recursive = TRUE, showWarnings = FALSE)
dir.create("output",         recursive = TRUE, showWarnings = FALSE)

# Dezimaljahre in den Periodenindex t umrechnen und umgekehrt (2005-Q1 = 1).
date_to_t <- function(d) as.integer(round((d - 2005) * 4) + 1)
to_date   <- function(tt) 2005 + (as.integer(tt) - 1) / 4

# Periodenindex, Datum und Vorperiode auf Widerspruchsfreiheit pruefen.
stopifnot(CFG$t_treat == date_to_t(CFG$treat_date),
          CFG$t_max   == date_to_t(CFG$date_max),
          identical(CFG$pre_t, CFG$t_min:(CFG$t_treat - 1L)))

# Wurzel des mittleren quadrierten Prognosefehlers berechnen.
rmspe <- function(x) sqrt(mean(x^2, na.rm = TRUE))

# Outcome-Stuetzstellen der Hauptspezifikation festlegen.
SPECIAL_PRED <- lapply(c(8, 16, 24, 32, 40), \(p) list(CFG$outcome, p, "mean"))

# Synthetische Kontrolle mit Synth schaetzen.
run_scm <- function(panel, predictors, treated_id, donor_ids,
                    special_predictors = SPECIAL_PRED,
                    pre_t = CFG$pre_t, plot_t = CFG$t_min:CFG$t_max,
                    dependent = CFG$outcome) {
  dp <- Synth::dataprep(
    foo = panel, predictors = predictors, predictors.op = "mean",
    special.predictors = special_predictors, dependent = dependent,
    unit.variable = "unit_id", unit.names.variable = "country",
    time.variable = "t",
    treatment.identifier = treated_id, controls.identifier = donor_ids,
    time.predictors.prior = pre_t, time.optimize.ssr = pre_t, time.plot = plot_t)
  list(dataprep = dp, synth = Synth::synth(dp, trace = FALSE))
}

# Verlauf der behandelten Einheit, der synthetischen Kontrolle und die Luecke auslesen.
extract_paths <- function(res) {
  dp <- res$dataprep
  tt <- as.integer(rownames(dp$Y1plot))
  tibble::tibble(t = tt, date = to_date(tt),
                 deutschland = as.numeric(dp$Y1plot),
                 synthetisch = as.numeric(dp$Y0plot %*% res$synth$solution.w)) |>
    dplyr::mutate(gap = deutschland - synthetisch)
}