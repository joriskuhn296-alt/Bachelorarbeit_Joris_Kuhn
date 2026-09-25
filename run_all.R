# Fuehrt alle Skripte in der richtigen Reihenfolge aus und protokolliert die Softwareumgebung.

scripts <- c(
  "01_build_panel.R",
  "02_donorpool.R",
  "03_estimation.R",
  "04_in_time_placebo.R",
  "05_augmented_scm.R",
  "06_sensitivity_zeitler.R",
  "07_variante_ohne_bip.R",
  "08_diag_ohne_ud_invrate.R"
)

# Jedes Skript in einer eigenen Umgebung ausführen.
for (f in scripts) {
  message("\n=== ", f, " ===")
  source(f, local = new.env(), encoding = "UTF-8")
}

# R-Version und Paketversionen festhalten.
writeLines(capture.output(sessionInfo()), "output/sessionInfo.txt")