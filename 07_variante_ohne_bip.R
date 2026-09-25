# Eingabe: panel_scm.rds und donorpool.rds. Ausgabe: output.

source("00_setup.R")
library(tidyverse)
library(Synth)

# Analysepanel und Einheiten-IDs laden.
panel_scm  <- readRDS("data/processed/panel_scm.rds")
dp_obj     <- readRDS("data/processed/donorpool.rds")
id_treated <- dp_obj$id_treated
id_donors  <- dp_obj$id_donors

# Hauptspezifikation und Variante ohne beide BIP-Größen festlegen.
varianten <- list(
  list(v = "V0 Haupt",    predictors = CFG$predictors),
  list(v = "V1 ohne BIP", predictors = setdiff(CFG$predictors, c("gdp_pc", "gdp_growth"))))

# Beide Varianten schätzen und Gewichte, Pfade und Kennzahlen sammeln.
ergebnisse <- list(); pfade_all <- list()
for (spec in varianten) {
  res <- tryCatch(run_scm(panel_scm, spec$predictors, id_treated, id_donors),
                  error = \(e) { message(spec$v, " fehlgeschlagen: ", conditionMessage(e)); NULL })
  if (is.null(res)) next
  pf <- extract_paths(res)
  pfade_all[[spec$v]] <- pf |> mutate(variante = spec$v)
  w <- synth.tab(dataprep.res = res$dataprep, synth.res = res$synth)$tab.w |>
    as_tibble() |> arrange(desc(w.weights))
  write_csv(w, paste0("output/weights_", gsub("[^A-Za-z0-9]+", "_", spec$v), ".csv"))
  ergebnisse[[spec$v]] <- tibble(
    variante  = spec$v,
    n_donoren = length(id_donors),
    n_praed   = length(spec$predictors) + length(SPECIAL_PRED),
    rmspe_pre = rmspe(pf$gap[pf$t <  CFG$t_treat]),
    att_post  = mean(pf$gap[pf$t >= CFG$t_treat]),
    top_donor = paste0(w$unit.names[1], " (", round(w$w.weights[1], 3), ")"))
}

# Vergleichstabelle speichern.
vergleich <- bind_rows(ergebnisse)
print(vergleich)
write_csv(vergleich, "output/praediktorvariante_ohne_bip.csv")
