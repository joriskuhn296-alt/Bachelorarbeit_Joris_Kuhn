# Prueft das Quartalspanel auf Vollstaendigkeit und beschreibt es deskriptiv.
# Eingabe: data/processed/panel_long.rds. Ausgabe: output.

source("00_setup.R")
library(tidyverse)

# Panel im Langformat laden.
panel_long <- readRDS("data/processed/panel_long.rds")

# Vollstaendiges Raster aus Land, Quartal und Variable mit den vorhandenen Werten abgleichen.
grid_full <- expand_grid(country  = c(CFG$treated, CFG$donors),
                         t        = CFG$t_min:CFG$t_max,
                         variable = unique(panel_long$variable))
coverage <- grid_full |>
  left_join(panel_long |> select(country, t, variable, value),
            by = c("country", "t", "variable")) |>
  mutate(available = !is.na(value))

# Abdeckung in Prozent je Land und Variable speichern.
coverage |>
  group_by(country, variable) |>
  summarise(pct = mean(available) * 100, .groups = "drop") |>
  pivot_wider(names_from = variable, values_from = pct) |>
  write_csv("output/abdeckung_land_x_variable.csv")

# Je Variable eine Heatmap der Datenverfuegbarkeit zeichnen.
pdf("output/vollstaendigkeit_heatmaps.pdf", width = 10, height = 6)
for (v in unique(coverage$variable)) {
  p <- coverage |> filter(variable == v) |> mutate(date = to_date(t)) |>
    ggplot(aes(date, fct_rev(country), fill = available)) +
    geom_tile(color = "white", linewidth = 0.15) +
    scale_fill_manual(values = c(`TRUE` = "grey25", `FALSE` = "firebrick"),
                      labels = c(`TRUE` = "vorhanden", `FALSE` = "fehlt"), name = NULL) +
    labs(title = paste("Datenverfuegbarkeit:", v),
         subtitle = "Fenster 2005-Q1 .. 2019-Q4", x = NULL, y = NULL) +
    theme_minimal(base_size = 10)
  print(p)
}
dev.off()

# Fehlende Quartale im Outcome je Land zaehlen und speichern.
outcome_gaps <- coverage |>
  filter(variable == CFG$outcome, !available) |>
  count(country, name = "fehlende_quartale") |>
  arrange(desc(fehlende_quartale))
write_csv(outcome_gaps, "output/outcome_luecken.csv")

# Luecken im Outcome melden.
if (nrow(outcome_gaps) > 0) {
  message("Outcome-Luecken (", CFG$outcome, ") bei: ",
          paste(outcome_gaps$country, collapse = ", "))
} else {
  message("Outcome (", CFG$outcome, ") vollstaendig fuer alle Einheiten.")
}

# Mittelwert, Standardabweichung und Fallzahl vor dem Treatment fuer Deutschland und Donorpool berechnen.
panel_long |>
  filter(t < CFG$t_treat) |>
  mutate(group = if_else(country == CFG$treated, "Deutschland", "Donorpool")) |>
  group_by(group, variable) |>
  summarise(mean = mean(value, na.rm = TRUE), sd = sd(value, na.rm = TRUE),
            n = sum(!is.na(value)), .groups = "drop") |>
  pivot_wider(names_from = group, values_from = c(mean, sd, n)) |>
  write_csv("output/deskriptiv_praetreatment.csv")

# Je Variable den Verlauf Deutschlands gegen die Donoren zeichnen.
pdf("output/zeitreihen_alle_variablen.pdf", width = 10, height = 6)
for (v in unique(panel_long$variable)) {
  p <- panel_long |> filter(variable == v) |>
    ggplot(aes(date, value, group = country)) +
    geom_line(data = \(d) filter(d, country != CFG$treated),
              color = "grey70", linewidth = 0.35) +
    geom_line(data = \(d) filter(d, country == CFG$treated),
              color = "black", linewidth = 1.0) +
    geom_vline(xintercept = CFG$treat_date, linetype = "dashed") +
    coord_cartesian(xlim = c(CFG$date_min, CFG$date_max)) +
    labs(title = v, x = NULL, y = NULL,
         subtitle = "DEU schwarz vs. Donorpool grau; gestrichelt 2015-Q1") +
    theme_minimal(base_size = 11)
  print(p)
}
dev.off()