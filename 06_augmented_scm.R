# Augmented SCM mit Ridge-Outcome-Modell nach Ben-Michael, Feller und Rothstein (2021).
# Eingabe: panel_scm.rds sowie Pfade und Gewichte aus 04_estimation.R. Ausgabe: output.

source("00_setup.R")
library(tidyverse)
library(augsynth)

# Analysepanel laden und Treatmentindikator fuer Deutschland ab 2015-Q1 setzen.
df <- readRDS("data/processed/panel_scm.rds") |>
  as_tibble() |>
  mutate(trt = as.integer(country == CFG$treated & t >= CFG$t_treat))

# Ridge-ASCM mit datengesteuertem lambda schaetzen und zusammenfassen.
ascm <- augsynth(emp_rate ~ trt, unit = country, time = t, data = df,
                 progfunc = "Ridge", scm = TRUE)
summ <- summary(ascm)
print(summ)

# Donorgewichte speichern.
w <- setNames(as.numeric(ascm$weights), rownames(ascm$weights))
tibble(country = names(w), gewicht = w) |>
  arrange(desc(gewicht)) |>
  write_csv("output/ascm_weights_ridge.csv")

# Konzentration, negative Gewichte und Gewichtssumme ausgeben.
cat("\nASCM-Gewichte (Ridge):\n"); print(round(sort(w, decreasing = TRUE), 3))
cat("Groesstes Einzelgewicht :", round(max(w), 3), "(", names(which.max(w)), ")\n")
cat("Anzahl |w| > 0.05       :", sum(abs(w) > 0.05), "\n")
cat("Negative Gewichte       :", sum(w < -1e-4), "\n")
cat("Summe der Gewichte      :", round(sum(w), 3), "\n")

# Luecken des klassischen Schaetzers und der ASCM auf gleicher Skala einlesen.
pfade_klassisch <- read_csv("output/synth_pfade_haupt.csv", show_col_types = FALSE) |>
  mutate(t = as.integer(t))
gap_ascm_alle <- summ$att |> as_tibble() |>
  transmute(t = as.integer(Time), gap = Estimate) |>
  filter(!is.na(t))
w_klass <- read_csv("output/synth_weights_haupt.csv", show_col_types = FALSE)

# ATT, Vorperioden-RMSPE und Gewichtsspanne beider Schaetzer vergleichen und speichern.
vergleich <- tibble(
  Schaetzer   = c("Klassisch (Synth)", "Augmented SCM (Ridge)"),
  ATT_post    = c(mean(pfade_klassisch$gap[pfade_klassisch$t >= CFG$t_treat]),
                  summ$average_att$Estimate),
  RMSPE_pre   = c(rmspe(pfade_klassisch$gap[pfade_klassisch$t < CFG$t_treat]),
                  rmspe(gap_ascm_alle$gap[gap_ascm_alle$t < CFG$t_treat])),
  max_Gewicht = c(max(w_klass$w.weights), max(w)),
  min_Gewicht = c(min(w_klass$w.weights), min(w))
)
print(vergleich)
write_csv(vergleich, "output/ascm_vergleich.csv")

# Luecken beider Schaetzer zeichnen.
pdf("output/ascm_gap_vergleich.pdf", width = 9, height = 5.5)
print(
  bind_rows(
    pfade_klassisch |> transmute(date = to_date(t), gap, Schaetzer = "Klassisch (Synth)"),
    gap_ascm_alle   |> transmute(date = to_date(t), gap, Schaetzer = "Augmented SCM (Ridge)")
  ) |>
    ggplot(aes(date, gap, color = Schaetzer)) +
    geom_line(linewidth = 0.9) +
    geom_hline(yintercept = 0, linewidth = 0.3) +
    geom_vline(xintercept = CFG$treat_date, linetype = "dotted") +
    labs(x = "Jahr", y = "Differenz Erwerbstätigenquote", color = NULL) +
    theme_minimal(base_size = 12) +
    theme(legend.position = "bottom")
)
dev.off()