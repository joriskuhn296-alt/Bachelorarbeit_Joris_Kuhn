# In-Time-Placebo: verlegt das Treatment fiktiv in die Vorperiode und misst die Luecke bis 2014-Q4.
# Platzierungen mit der Finanzkrise im Auswertungsfenster (2009 bis 2011) werden als kontaminiert gekennzeichnet.
# Eingabe: panel_scm.rds und donorpool.rds. Ausgabe: output.

source("00_setup.R")
library(tidyverse)
library(Synth)

# Analysepanel und Einheiten-IDs laden.
panel_scm  <- readRDS("data/processed/panel_scm.rds")
dp_obj     <- readRDS("data/processed/donorpool.rds")
id_treated <- dp_obj$id_treated
id_donors  <- dp_obj$id_donors

# Fiktive Treatmentzeitpunkte 2009-Q1 bis 2013-Q1 und kontaminierte Platzierungen festlegen.
T_PRE_END    <- CFG$t_treat - 1L
FAKE_TREATS  <- c(17L, 21L, 25L, 29L, 33L)
KONTAMINIERT <- c(17L, 21L, 25L)

# Fuenf Outcome-Stuetzstellen gleichmaessig ueber das jeweilige Anpassungsfenster legen.
make_lags <- function(t_fake, n_lags = 5L, outcome = CFG$outcome) {
  L   <- t_fake - 1L
  pos <- unique(round(seq(from = max(1L, floor(L / n_lags)), to = L, length.out = n_lags)))
  pos <- pos[pos >= 1L & pos <= L]
  lapply(pos, \(p) list(outcome, as.integer(p), "mean"))
}

# Je fiktivem Treatment schaetzen und Pfade, Gewichte und Kennzahlen sammeln.
kennzahlen <- list(); pfade_all <- list(); gewichte_all <- list()
for (tf in FAKE_TREATS) {
  res <- tryCatch(run_scm(panel_scm, CFG$predictors, id_treated, id_donors,
                          special_predictors = make_lags(tf), pre_t = 1:(tf - 1L)),
                  error = \(e) { message("t=", tf, " fehlgeschlagen: ", conditionMessage(e)); NULL })
  if (is.null(res)) next
  pf  <- extract_paths(res)
  key <- as.character(tf)
  pfade_all[[key]] <- pf |> mutate(spez = "Haupt (J=7)", t_fake = tf, date_fake = to_date(tf))
  gewichte_all[[key]] <- synth.tab(dataprep.res = res$dataprep, synth.res = res$synth)$tab.w |>
    as_tibble() |>
    mutate(spez = "Haupt (J=7)", t_fake = tf, date_fake = to_date(tf)) |>
    arrange(desc(w.weights))
  g_fit  <- pf$gap[pf$t <  tf]
  g_plac <- pf$gap[pf$t >= tf & pf$t <= T_PRE_END]
  kennzahlen[[key]] <- tibble(
    spez = "Haupt (J=7)", fake_treat = to_date(tf),
    kontaminiert = tf %in% KONTAMINIERT,
    n_fit = length(g_fit), n_placebo = length(g_plac),
    rmspe_fit = rmspe(g_fit),
    mean_gap_plac = mean(g_plac),
    rmspe_plac = rmspe(g_plac),
    ratio = rmspe(g_plac) / rmspe(g_fit))
}

# Kennzahlen und Pfade speichern.
tab <- bind_rows(kennzahlen) |> arrange(fake_treat)
print(tab, n = Inf)
write_csv(tab, "output/in_time_placebo_kennzahlen.csv")
write_csv(bind_rows(pfade_all), "output/in_time_placebo_pfade.csv")

# Donorgewichte je Platzierung speichern.
gew <- bind_rows(gewichte_all) |>
  select(spez, fake_treat = date_fake, country = unit.names, gewicht = w.weights) |>
  arrange(fake_treat, desc(gewicht))
write_csv(gew, "output/in_time_placebo_gewichte.csv")
print(gew |> filter(gewicht > 0.001), n = Inf)

# Luecken aller Platzierungen zeichnen.
pdf("output/in_time_placebo_plot.pdf", width = 9, height = 5.5)
print(
  bind_rows(pfade_all) |>
    mutate(`Fiktives Treatment` = factor(paste0(as.integer(date_fake), "-Q1"))) |>
    ggplot(aes(date, gap, color = `Fiktives Treatment`)) +
    geom_hline(yintercept = 0, linewidth = 0.3) +
    geom_line(linewidth = 0.7) +
    geom_vline(xintercept = CFG$treat_date, linetype = "dotted") +
    labs(x = NULL, y = "Differenz Erwerbstätigenquote", color = "Fiktives Treatment") +
    theme_minimal(base_size = 11)
)
dev.off()