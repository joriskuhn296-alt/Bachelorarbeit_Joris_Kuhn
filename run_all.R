# Fuehrt alle Skripte in der richtigen Reihenfolge aus und protokolliert die Softwareumgebung.

scripts <- c(
  "01_build_panel.R",
  "02_diagnostics.R",
  "03_donorpool.R",
  "04_estimation.R",
  "05_in_time_placebo.R",
  "06_augmented_scm.R",
  "07_sensitivity_zeitler.R",
  "08_variante_ohne_bip.R",
  "09_diag_ohne_ud_invrate.R"
)

# Jedes Skript in einer eigenen Umgebung ausfuehren, damit keines von Objekten eines anderen abhaengt.
for (f in scripts) {
  message("\n=== ", f, " ===")
  source(f, local = new.env(), encoding = "UTF-8")
}

# R-Version und Paketversionen festhalten.
writeLines(capture.output(sessionInfo()), "output/sessionInfo.txt")