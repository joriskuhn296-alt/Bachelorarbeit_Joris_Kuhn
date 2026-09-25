# Beschreibt den vorab festgelegten Donorpool: Datenbasis, konvexe Huelle und Outcome-Verlaeufe.
# Eingabe: data/processed/panel_scm.rds. Ausgabe: data/processed/donorpool.rds und output.

source("00_setup.R")
library(tidyverse)

# Schwellen fuer die Pruefung der Datenbasis festlegen.
THRESH <- list(outcome_complete = 1.00, pooled_missing_max = 0.30,
               single_pred_min = 0.50, corr_min = 0.00)

# Analysepanel laden und auf vollstaendige Einheiten und Quartale pruefen.
panel_scm <- readRDS("data/processed/panel_scm.rds")
units_all <- c(CFG$treated, CFG$donors)
n_t       <- CFG$t_max - CFG$t_min + 1
fehlend   <- setdiff(units_all, unique(panel_scm$country))
if (length(fehlend) > 0) stop("Nicht im Panel: ", paste(fehlend, collapse = ", "))
stopifnot(nrow(panel_scm) == length(units_all) * n_t)
if (any(is.na(panel_scm[[CFG$outcome]])))
  warning("Outcome enthaelt NA, dataprep schlaegt fehl.")

panel <- as_tibble(panel_scm)
pre   <- panel |> filter(t %in% CFG$pre_t)

# A1: Vollstaendigkeit des Outcomes je Land bestimmen.
a1 <- panel |>
  group_by(country) |>
  summarise(A1_n = sum(!is.na(.data[[CFG$outcome]])),
            A1_cov = A1_n / n_t, .groups = "drop") |>
  mutate(A1_fail = A1_cov < THRESH$outcome_complete)

# A2: Anteil fehlender Praediktorwerte vor dem Treatment je Land bestimmen.
pred_long <- pre |>
  select(country, t, all_of(CFG$predictors)) |>
  pivot_longer(-c(country, t), names_to = "variable", values_to = "value") |>
  mutate(betroffen = is.na(value))
a2 <- pred_long |> group_by(country) |>
  summarise(A2_anteil = mean(betroffen), .groups = "drop") |>
  mutate(A2_fail = A2_anteil > THRESH$pooled_missing_max)

# A3: Praediktor mit der geringsten Abdeckung je Land bestimmen.
a3 <- pred_long |> group_by(country, variable) |>
  summarise(cov = mean(!is.na(value)), .groups = "drop") |>
  group_by(country) |> slice_min(cov, n = 1, with_ties = FALSE) |> ungroup() |>
  transmute(country, A3_var = variable, A3_cov = cov,
            A3_fail = cov < THRESH$single_pred_min)

# A4: Korrelation des Outcomes jedes Donors mit Deutschland vor dem Treatment berechnen.
outcome_wide <- pre |>
  select(country, t, y = all_of(CFG$outcome)) |>
  pivot_wider(names_from = country, values_from = y) |> arrange(t)
a4 <- tibble(country = CFG$donors) |>
  mutate(A4_corr = map_dbl(country, \(c)
                           suppressWarnings(cor(outcome_wide[[CFG$treated]], outcome_wide[[c]],
                                                use = "pairwise.complete.obs"))),
         A4_flag = !is.na(A4_corr) & A4_corr < THRESH$corr_min)

# Kriterien A1 bis A4 zur Tabelle der Datenbasis zusammenfuehren und speichern.
datenbasis <- tibble(country = units_all) |>
  left_join(a1, "country") |> left_join(a2, "country") |>
  left_join(a3, "country") |> left_join(a4, "country") |>
  mutate(rolle = if_else(country == CFG$treated, "behandelt", "Donor"))
tab_datenbasis <- datenbasis |>
  transmute(Land = country, Rolle = rolle,
            Outcome = sprintf("%d/%d", A1_n, n_t),
            Praed_fehlend = sprintf("%.0f%%", 100 * A2_anteil),
            Schwaechster = sprintf("%s (%.0f%%)", A3_var, 100 * A3_cov),
            Korr_DEU = if_else(is.na(A4_corr), "-", sprintf("%.2f", A4_corr)))
print(tab_datenbasis, n = Inf)

# Verletzte Kriterien melden.
if (any(datenbasis$A1_fail))
  warning("A1 Outcome-Luecken bei: ",
          paste(datenbasis$country[datenbasis$A1_fail], collapse = ", "))
if (any(datenbasis$A4_flag, na.rm = TRUE))
  warning("A4 nicht-positive Outcome-Korrelation: ",
          paste(datenbasis$country[replace_na(datenbasis$A4_flag, FALSE)], collapse = ", "))

# Anzahl der Donoren, Praediktoren und den kleinstmoeglichen Placebo-p-Wert berechnen.
J <- length(CFG$donors); k <- length(CFG$predictors); p_min <- 1 / (J + 1)
message("J=", J, " Donoren | k=", k, " Praediktoren | T0=", length(CFG$pre_t),
        " | kleinster Placebo-p=", round(p_min, 3))

# Mittelwerte der Praediktoren vor dem Treatment je Land berechnen.
pred_means <- pre |>
  group_by(country) |>
  summarise(across(all_of(CFG$predictors), \(x) mean(x, na.rm = TRUE)), .groups = "drop") |>
  pivot_longer(-country, names_to = "variable", values_to = "m")

# Lage Deutschlands relativ zur Spannweite der Donoren je Praediktor bestimmen.
hull_tab <- pred_means |>
  group_by(variable) |>
  summarise(
    DEU      = m[country == CFG$treated],
    lo       = min(m[country %in% CFG$donors]),
    hi       = max(m[country %in% CFG$donors]),
    donor_lo = country[country %in% CFG$donors][which.min(m[country %in% CFG$donors])],
    donor_hi = country[country %in% CFG$donors][which.max(m[country %in% CFG$donors])],
    .groups  = "drop") |>
  mutate(status  = if_else(DEU >= lo & DEU <= hi, "ok", "AUSSERHALB"),
         abstand = case_when(DEU < lo ~ lo - DEU, DEU > hi ~ DEU - hi, TRUE ~ 0))
print(hull_tab)
write_csv(hull_tab |> select(variable, DEU, lo, hi, status), "output/tab_konvexe_huelle.csv")
if (any(hull_tab$status == "AUSSERHALB"))
  warning("Konvexe Huelle verletzt: ",
          paste(hull_tab$variable[hull_tab$status == "AUSSERHALB"], collapse = ", "))

# Praediktoren in Donor-Standardabweichungen umrechnen und die Huelle grafisch darstellen.
lab <- c(gdp_pc = "BIP pro Kopf", gdp_growth = "BIP-Wachstum",
         inv_rate = "Investitionsquote", inflation = "Inflation",
         lfpr = "Erwerbsquote", ud = "Gewerkschaftsdichte")
plot_df <- pred_means |>
  group_by(variable) |>
  mutate(z = (m - mean(m[country %in% CFG$donors])) / sd(m[country %in% CFG$donors])) |>
  summarise(DEU_z = z[country == CFG$treated],
            lo_z  = min(z[country %in% CFG$donors]),
            hi_z  = max(z[country %in% CFG$donors]), .groups = "drop") |>
  mutate(status = if_else(DEU_z >= lo_z & DEU_z <= hi_z, "innerhalb", "außerhalb"),
         name   = lab[variable]) |>
  arrange(DEU_z) |>
  mutate(name = factor(name, levels = name))
p_hull <- ggplot(plot_df, aes(y = name)) +
  geom_segment(aes(x = lo_z, xend = hi_z, y = name, yend = name),
               linewidth = 4, colour = "grey80", lineend = "round") +
  geom_point(aes(x = DEU_z, colour = status), size = 3.2) +
  geom_vline(xintercept = 0, linetype = "dotted") +
  scale_colour_manual(values = c(innerhalb = "black", "außerhalb" = "firebrick")) +
  labs(x = "Standardisierte Abweichung (in Donor-SD)", y = NULL, colour = NULL) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom")
ggsave("output/konvexe_huelle.pdf", p_hull, width = 8, height = 4.5, device = pdf)

# Einheiten-IDs fuer Synth bestimmen und die Donorpool-Definition speichern.
id_treated <- unique(panel_scm$unit_id[panel_scm$country == CFG$treated])
id_donors  <- sort(setdiff(unique(panel_scm$unit_id), id_treated))
saveRDS(list(cfg = CFG, thresh = THRESH, id_treated = id_treated,
             id_donors = id_donors, J = J, k = k, p_min = p_min),
        "data/processed/donorpool.rds")
