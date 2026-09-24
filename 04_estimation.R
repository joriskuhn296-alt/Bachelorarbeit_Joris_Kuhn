# Schaetzt die Hauptspezifikation und fuehrt In-Space-Placebo, Leave-One-Out und Rueckdatierung durch.
# Positive Luecke: Erwerbstaetigenquote Deutschlands liegt ueber der synthetischen Kontrolle.
# Eingabe: panel_scm.rds und donorpool.rds. Ausgabe: output.

source("00_setup.R")
library(tidyverse)
library(Synth)
library(kableExtra)

# Analysepanel und Einheiten-IDs laden.
panel_scm  <- readRDS("data/processed/panel_scm.rds")
dp_obj     <- readRDS("data/processed/donorpool.rds")
id_treated <- dp_obj$id_treated
id_donors  <- dp_obj$id_donors

# ---------------------------------------------------------------------------
# A Hauptspezifikation
# ---------------------------------------------------------------------------

# Hauptspezifikation schaetzen und Pfade speichern.
res_haupt   <- run_scm(panel_scm, CFG$predictors, id_treated, id_donors)
pfade_haupt <- extract_paths(res_haupt)
write_csv(pfade_haupt, "output/synth_pfade_haupt.csv")

# Donorgewichte und Praediktorbalance speichern.
tabs        <- synth.tab(dataprep.res = res_haupt$dataprep, synth.res = res_haupt$synth)
weights_tab <- tabs$tab.w |> as_tibble() |> arrange(desc(w.weights))
print(weights_tab)
write_csv(weights_tab, "output/synth_weights_haupt.csv")
bal <- tabs$tab.pred |> as_tibble(rownames = "Praediktor")
write_csv(bal, "output/synth_praediktorbalance_haupt.csv")

# Praediktorbalance als LaTeX-Tabelle ausgeben.
lab <- c(
  gdp_pc              = "BIP pro Kopf",
  gdp_growth          = "BIP-Wachstum",
  inv_rate            = "Investitionsquote",
  inflation           = "Inflation",
  lfpr                = "Erwerbsquote",
  ud                  = "Gewerkschaftsdichte",
  special.emp_rate.8  = "Beschäftigungsquote ~2006-Q4",
  special.emp_rate.16 = "Beschäftigungsquote ~2008-Q4",
  special.emp_rate.24 = "Beschäftigungsquote ~2010-Q4",
  special.emp_rate.32 = "Beschäftigungsquote ~2012-Q4",
  special.emp_rate.40 = "Beschäftigungsquote 2014-Q4"
)
bal |>
  mutate(Praediktor = lab[Praediktor],
         across(c(Treated, Synthetic, `Sample Mean`), ~ formatC(.x, format = "f", digits = 2))) |>
  kbl(format = "latex", booktabs = TRUE,
      align = c("l", "r", "r", "r"),
      col.names = c("Prädiktor", "Deutschland", "Synth. DEU", "Donor-Mittel"),
      caption = "Prädiktorbalance der Hauptspezifikation.",
      label = "praediktorbalance") |>
  kable_styling(latex_options = "hold_position") |>
  writeLines("output/praediktorbalance.tex")

# Verlauf Deutschlands und der synthetischen Kontrolle zeichnen.
pdf("output/synth_haupt_pfade.pdf", width = 9, height = 5.5)
print(
  pfade_haupt |>
    pivot_longer(c(deutschland, synthetisch), names_to = "serie", values_to = "wert") |>
    ggplot(aes(date, wert, color = serie)) +
    geom_line(linewidth = 0.9) +
    geom_vline(xintercept = CFG$treat_date, linetype = "dotted") +
    scale_color_manual(values = c(deutschland = "black", synthetisch = "firebrick")) +
    labs(x = "Jahr", y = "Erwerbstätigenquote in %", color = NULL) +
    theme_minimal(base_size = 12) +
    theme(legend.position = "bottom", legend.justification = "center")
)
dev.off()

# Luecke der Hauptspezifikation zeichnen.
pdf("output/synth_haupt_gap.pdf", width = 9, height = 5.5)
print(
  pfade_haupt |>
    ggplot(aes(date, gap)) +
    geom_line(linewidth = 0.9) +
    geom_hline(yintercept = 0, linewidth = 0.3) +
    geom_vline(xintercept = CFG$treat_date, linetype = "dotted") +
    labs(x = "Jahr", y = "Differenz Erwerbstätigenquote") +
    theme_minimal(base_size = 12)
)
dev.off()

# ---------------------------------------------------------------------------
# B In-Space-Placebo
# ---------------------------------------------------------------------------

# Jeden Donor einmal als behandelte Einheit schaetzen.
placebo_pfade <- map_dfr(id_donors, \(pid) {
  res <- tryCatch(run_scm(panel_scm, CFG$predictors, pid, setdiff(id_donors, pid)),
                  error = \(e) NULL)
  if (is.null(res)) return(NULL)
  extract_paths(res) |> mutate(country = panel_scm$country[panel_scm$unit_id == pid][1])
})

# RMSPE vor und nach dem Treatment vergleichen und den Placebo-p-Wert bestimmen.
placebo_alle <- bind_rows(pfade_haupt |> mutate(country = CFG$treated), placebo_pfade) |>
  group_by(country) |>
  summarise(rmspe_pre  = rmspe(gap[t <  CFG$t_treat]),
            rmspe_post = rmspe(gap[t >= CFG$t_treat]), .groups = "drop") |>
  mutate(ratio = rmspe_post / rmspe_pre) |>
  arrange(desc(ratio)) |>
  mutate(rang = row_number(), p_wert = rang / n())
print(placebo_alle)
write_csv(placebo_alle, "output/placebo_rmspe_ranking.csv")
message("Placebo: DEU Rang ", placebo_alle$rang[placebo_alle$country == CFG$treated],
        "/", nrow(placebo_alle),
        " (p=", round(placebo_alle$p_wert[placebo_alle$country == CFG$treated], 3), ")")

# Luecken aller Placebo-Laeufe gegen Deutschland zeichnen.
pdf("output/placebo_plot.pdf", width = 9, height = 5.5)
print(
  bind_rows(placebo_pfade, pfade_haupt |> mutate(country = CFG$treated)) |>
    mutate(ist_deu = country == CFG$treated) |>
    ggplot(aes(date, gap, group = country)) +
    geom_line(data = \(d) filter(d, !ist_deu), color = "grey65", linewidth = 0.4) +
    geom_line(data = \(d) filter(d,  ist_deu), color = "black", linewidth = 1.2) +
    geom_hline(yintercept = 0, linewidth = 0.3) +
    geom_vline(xintercept = CFG$treat_date, linetype = "dotted") +
    labs(x = "Jahr", y = "Differenz Erwerbstätigenquote") +
    theme_minimal(base_size = 12)
)
dev.off()

# ---------------------------------------------------------------------------
# C Leave-One-Out ueber alle Donoren mit positivem Gewicht (Abadie 2021)
# ---------------------------------------------------------------------------

# Kennzahlen der Hauptspezifikation als Referenz berechnen.
att_haupt       <- mean(pfade_haupt$gap[pfade_haupt$t >= CFG$t_treat])
rmspe_haupt_pre <- rmspe(pfade_haupt$gap[pfade_haupt$t <  CFG$t_treat])

# Donoren mit positivem Gewicht bestimmen, numerische Nullgewichte ignorieren.
W_TOL       <- 1e-4
loo_laender <- weights_tab$unit.names[weights_tab$w.weights > W_TOL]
message("Leave-One-Out: ", paste(loo_laender, collapse = ", "))

# Hauptspezifikation je einmal ohne einen dieser Donoren schaetzen.
loo_paths <- map_dfr(loo_laender, function(land) {
  pid <- panel_scm$unit_id[panel_scm$country == land][1]
  res <- tryCatch(run_scm(panel_scm, CFG$predictors, id_treated,
                          setdiff(id_donors, pid)), error = \(e) NULL)
  if (is.null(res)) return(NULL)
  extract_paths(res) |> mutate(ausgeschlossen = land)
})
write_csv(loo_paths, "output/leave_one_out_pfade.csv")

# ATT, Vorperioden-RMSPE und Abweichung von der Hauptspezifikation je Lauf berechnen.
loo_summary <- loo_paths |>
  group_by(ausgeschlossen) |>
  summarise(att_post  = mean(gap[t >= CFG$t_treat]),
            rmspe_pre = rmspe(gap[t <  CFG$t_treat]), .groups = "drop") |>
  left_join(
    loo_paths |> filter(t >= CFG$t_treat) |>
      left_join(pfade_haupt |> filter(t >= CFG$t_treat) |>
                  select(t, gap_h = gap), by = "t") |>
      group_by(ausgeschlossen) |>
      summarise(max_abw_vs_haupt  = max(abs(gap - gap_h)),
                mean_abw_vs_haupt = mean(abs(gap - gap_h)), .groups = "drop"),
    by = "ausgeschlossen") |>
  mutate(att_haupt = att_haupt, rmspe_haupt_pre = rmspe_haupt_pre,
         vorzeichen_wie_haupt = sign(att_post) == sign(att_haupt)) |>
  arrange(desc(max_abw_vs_haupt))
print(loo_summary, n = Inf)
write_csv(loo_summary, "output/leave_one_out_summary.csv")

# Vorzeichenwechsel melden.
if (any(!loo_summary$vorzeichen_wie_haupt))
  warning("Vorzeichenwechsel bei Ausschluss von: ",
          paste(loo_summary$ausgeschlossen[!loo_summary$vorzeichen_wie_haupt],
                collapse = ", "))

# Luecken der Leave-One-Out-Laeufe gegen die Hauptspezifikation zeichnen.
pdf("output/leave_one_out_plot.pdf", width = 9, height = 5.5)
print(
  ggplot(mapping = aes(date, gap)) +
    geom_line(data = loo_paths, aes(color = ausgeschlossen), linewidth = 0.6) +
    geom_line(data = pfade_haupt, color = "black", linewidth = 1.2) +
    geom_hline(yintercept = 0, linewidth = 0.3) +
    geom_vline(xintercept = CFG$treat_date, linetype = "dotted") +
    labs(x = "Jahr", y = "Differenz Erwerbstätigenquote", color = "ausgeschlossen") +
    theme_minimal(base_size = 12)
)
dev.off()

# ---------------------------------------------------------------------------
# D Rueckdatierung auf den Gesetzesbeschluss (Antizipation nach Schmitz 2017)
# ---------------------------------------------------------------------------

# Fiktives Treatment auf 2014-Q3 legen und nur bis 2014-Q2 anpassen.
T_ANTIZIP   <- 39L
pre_antizip <- 1:(T_ANTIZIP - 1L)

# Outcome-Stuetzstellen auf das verkuerzte Anpassungsfenster beschraenken.
LAGS_ANTIZIP <- lapply(c(8, 16, 24, 32), \(p) list(CFG$outcome, p, "mean"))

# Rueckdatierten Lauf schaetzen und Pfade speichern.
res_antizip   <- run_scm(panel_scm, CFG$predictors, id_treated, id_donors,
                         special_predictors = LAGS_ANTIZIP, pre_t = pre_antizip)
pfade_antizip <- extract_paths(res_antizip)
write_csv(pfade_antizip, "output/synth_pfade_antizipation.csv")

# Luecke im Fenster 2014-H2 und ab 2015 berechnen und speichern.
kennz_antizip <- tibble(
  fit_bis         = "2014-Q2 (t=38)",
  gap_2014Q3      = pfade_antizip$gap[pfade_antizip$t == 39],
  gap_2014Q4      = pfade_antizip$gap[pfade_antizip$t == 40],
  mean_gap_2014H2 = mean(pfade_antizip$gap[pfade_antizip$t %in% 39:40]),
  rmspe_pre_bis38 = rmspe(pfade_antizip$gap[pfade_antizip$t <= 38]),
  att_ab_2015     = mean(pfade_antizip$gap[pfade_antizip$t %in% 41:60]))
print(kennz_antizip)
write_csv(kennz_antizip, "output/antizipation_schmitz_kennzahlen.csv")

# Luecke des rueckdatierten Laufs zeichnen.
pdf("output/antizipation_schmitz_gap.pdf", width = 9, height = 5.5)
print(
  pfade_antizip |>
    ggplot(aes(date, gap)) +
    geom_line(linewidth = 0.9) +
    geom_hline(yintercept = 0, linewidth = 0.3) +
    geom_vline(xintercept = to_date(T_ANTIZIP), linetype = "dashed") +
    geom_vline(xintercept = CFG$treat_date, linetype = "dotted") +
    labs(title = "Antizipations-Check: Fit bis 2014-Q2, Gap danach",
         subtitle = "gestrichelt 2014-Q3 (Gesetz beschlossen), gepunktet 2015-Q1 (Lohn greift)",
         x = NULL, y = paste0("Differenz ", CFG$outcome)) +
    theme_minimal(base_size = 11)
)
dev.off()