# Sensitivitaetsschranke nach Zeitler, Vlontzos und Gilligan-Lee (2023, Gl. 3).
# Eingabe: panel_scm.rds und synth_pfade_haupt.csv aus 04_estimation.R. Ausgabe: output.

source("00_setup.R")
library(tidyverse)

# Analysepanel und Pfade der Hauptspezifikation laden.
panel_scm <- readRDS("data/processed/panel_scm.rds") |> as_tibble()
pfade     <- read_csv("output/synth_pfade_haupt.csv", show_col_types = FALSE) |>
  mutate(t = as.integer(t), date = to_date(t))
post_t    <- CFG$t_treat:CFG$t_max

# Outcome Deutschlands und der Donoren als Matrix nach Quartalen anordnen.
y_wide  <- panel_scm |> select(country, t, y = all_of(CFG$outcome)) |>
  pivot_wider(names_from = country, values_from = y) |> arrange(t)
y_deu   <- y_wide[[CFG$treated]]
donors  <- setdiff(names(y_wide), c("t", CFG$treated))
X       <- y_wide |> select(all_of(donors)) |> as.matrix()
is_pre  <- y_wide$t %in% CFG$pre_t
is_post <- y_wide$t %in% post_t

# Deutschland vor dem Treatment per OLS ohne Konstante auf die Donoren regressieren.
betas <- coef(lm(y_deu[is_pre] ~ X[is_pre, ] - 1)); names(betas) <- donors
bnz   <- betas[abs(betas) > 1e-6 & !is.na(betas)]
N_z   <- length(bnz); max_b <- max(abs(bnz)); land_b <- names(bnz)[which.max(abs(bnz))]

# Groesste Verschiebung des Donor-Mittelwerts zwischen Vor- und Nachperiode bestimmen.
proxy_change <- setNames(map_dbl(donors, \(d)
                                 abs(mean(X[is_pre, d], na.rm = TRUE) - mean(X[is_post, d], na.rm = TRUE))), donors)
max_c  <- max(proxy_change); land_c <- names(proxy_change)[which.max(proxy_change)]

# Schranke berechnen und dem ATT gegenueberstellen.
bound_ols <- N_z * max_b * max_c
att_avg   <- mean(pfade$gap[pfade$t %in% post_t])
zeitler_tab <- tibble(
  Variante         = "Zeitler (OLS)",
  N                = N_z,
  max_Koeff        = max_b,   Koeff_Land  = land_b,
  max_Proxy_Change = max_c,   Change_Land = land_c,
  Bound            = bound_ols,
  ATT_avg          = att_avg,
  `|Bound|<|ATT|`  = abs(bound_ols) < abs(att_avg)
)
write_csv(zeitler_tab, "output/zeitler_bound.csv")

# Nachperiodenluecke mit Schrankenband und die Verschiebung je Donor zeichnen.
pdf("output/zeitler_bound_plot.pdf", width = 9, height = 5.5)
print(
  pfade |> mutate(lo = gap - bound_ols, hi = gap + bound_ols) |>
    filter(t >= CFG$t_treat) |>
    ggplot(aes(date, gap)) +
    geom_ribbon(aes(ymin = lo, ymax = hi), fill = "seagreen", alpha = 0.18) +
    geom_line(linewidth = 1) + geom_hline(yintercept = 0, linewidth = 0.35) +
    labs(title = "Post-Treatment-Gap mit Zeitler-Schranken (Quartale)",
         subtitle = paste0("Bound = +-", round(bound_ols, 2), " pp"),
         x = NULL, y = paste0("Differenz ", CFG$outcome, " (DEU - synth.)")) +
    theme_minimal(base_size = 11))
print(
  tibble(country = donors, change = proxy_change) |>
    ggplot(aes(change, fct_reorder(country, change))) +
    geom_col(fill = "grey45") +
    geom_vline(xintercept = max_c, linetype = "dashed", color = "firebrick") +
    labs(title = "Proxy-Change je Donor (|Prae- minus Post-Mittel|)",
         x = "abs. Aenderung Outcome-Mittel (pp)", y = NULL) +
    theme_minimal(base_size = 11))
dev.off()