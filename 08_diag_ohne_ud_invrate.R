# Diagnoselauf ohne die huellenverletzenden Praediktoren ud und inv_rate. Kein Robustheitstest.
# Eingabe: panel_scm.rds, donorpool.rds sowie Pfade und Gewichte aus 03_estimation.R. Ausgabe: output.

source("00_setup.R")
library(tidyverse)
library(Synth)

# Analysepanel und Einheiten-IDs laden.
panel_scm  <- readRDS("data/processed/panel_scm.rds")
dp_obj     <- readRDS("data/processed/donorpool.rds")
id_treated <- dp_obj$id_treated
id_donors  <- dp_obj$id_donors

# Reduzierten Praediktorsatz festlegen.
preds_red <- setdiff(CFG$predictors, c("ud", "inv_rate"))
v_name    <- "R1 ohne UD & INV (J=7)"

# Reduzierte Spezifikation schaetzen und Gewichte speichern.
res <- run_scm(panel_scm, preds_red, id_treated, id_donors)
pf  <- extract_paths(res) |> mutate(variante = v_name)
w   <- synth.tab(dataprep.res = res$dataprep, synth.res = res$synth)$tab.w |>
  as_tibble() |> arrange(desc(w.weights))
write_csv(w, paste0("output/diag_weights_", gsub("[^A-Za-z0-9]+", "_", v_name), ".csv"))

# Kennzahlen einer Spezifikation aus Pfaden und Gewichten zusammenstellen.
kennzahlen <- function(name, pfade, gewichte, n_praed) {
  tibble(variante  = name,
         n_donoren = length(id_donors),
         n_praed   = n_praed + length(SPECIAL_PRED),
         rmspe_pre = rmspe(pfade$gap[pfade$t <  CFG$t_treat]),
         att_post  = mean(pfade$gap[pfade$t >= CFG$t_treat]),
         top_donor = paste0(gewichte$unit.names[1], " (", round(gewichte$w.weights[1], 3), ")"),
         top2      = paste0(gewichte$unit.names[2], " (", round(gewichte$w.weights[2], 3), ")"))
}

# Hauptspezifikation aus 04_estimation.R als Referenzzeile einlesen.
pf_h <- read_csv("output/synth_pfade_haupt.csv", show_col_types = FALSE) |> mutate(t = as.integer(t))
w_h  <- read_csv("output/synth_weights_haupt.csv", show_col_types = FALSE) |> arrange(desc(w.weights))

# Referenz und reduzierte Spezifikation vergleichen und speichern.
vergleich <- bind_rows(kennzahlen("V0 Haupt", pf_h, w_h, length(CFG$predictors)),
                       kennzahlen(v_name, pf, w, length(preds_red)))
print(vergleich)
write_csv(vergleich, "output/diag_ohne_ud_invrate.csv")