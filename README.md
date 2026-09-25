# Beschäftigungseffekt des gesetzlichen Mindestlohns 2015: Synthetic Control Method

RCode und Rohdaten zur Bachelorarbeit von Joris Kuhn, TU Braunschweig, 2026.

Titel der Arbeit: Strukturelles kausales Modell zur Bewertung der Beschäftigungseffekte des Mindestlohns: Eine SCM-basierte Panelanalyse mit begrenzten Regionaldaten

Die Arbeit schätzt mit der Synthetic Control Method (SCM) den aggregierten Effekt des 2015 eingeführten gesetzlichen Mindestlohns auf die Erwerbstätigenquote der 15- bis 64-Jährigen in Deutschland. Die synthetische Kontrolle wird aus sieben OECD-Staaten ohne gesetzlichen Mindestlohn gebildet (Österreich, Dänemark, Finnland, Island, Italien, Norwegen, Schweden). Die Analyse nutzt Quartalsdaten von 2005-Q1 bis 2019-Q4, das Treatment liegt in 2015-Q1. Ergänzt wird sie um Placebo-Tests, Leave-One-Out, eine Augmented SCM, eine Rückdatierung des Treatments und eine Sensitivitätsschranke nach Zeitler et al. (2023).

Spezifikation und Robustheitsläufe wurden vor Kenntnis der Ergebnisse festgelegt. Sie sind zentral in `00_setup.R` hinterlegt.

## Voraussetzungen

- R 4.4.2 (getestet unter macOS 26, Apple Silicon)
- Paket `renv` zur Wiederherstellung der verwendeten Paketversionen

Die verwendeten Pakete und ihre Versionen sind in `renv.lock` festgehalten. Das Paket `augsynth` stammt von GitHub. `renv.lock` hält den genauen Stand fest.

## Ausführung

1. Repository klonen und `RSKRIPT.Rproj` in RStudio öffnen. Das setzt das Arbeitsverzeichnis auf den Projektordner.
2. Pakete installieren:
   ```r
   renv::restore()
   ```
3. Gesamte Analyse ausführen:
   ```r
   source("run_all.R")
   ```

`run_all.R` führt alle Skripte in der richtigen Reihenfolge aus, jedes in einer eigenen Umgebung. Zwischendateien entstehen in `data/processed/`, alle Ergebnisse in `output/`. Am Ende wird `output/sessionInfo.txt` mit R- und Paketversionen geschrieben.

## Aufbau

```
.
├── 00_setup.R                  Konfiguration, Seed, gemeinsame Hilfsfunktionen
├── 01_build_panel.R            Quartalspanel aus den Rohdaten
├── 02_donorpool.R              Konvexe Hülle, Einheiten-IDs
├── 03_estimation.R             Hauptspezifikation, In-Space-Placebo, Leave-One-Out, Rückdatierung
├── 04_in_time_placebo.R        In-Time-Placebo
├── 05_augmented_scm.R          Augmented SCM (Ridge)
├── 06_sensitivity_zeitler.R    Sensitivitätsschranke nach Zeitler et al. (2023)
├── 07_variante_ohne_bip.R      Robustheitsvariante ohne BIP-Prädiktoren
├── 08_diag_ohne_ud_invrate.R   Diagnoselauf ohne hüllenverletzende Prädiktoren
├── run_all.R                   Führt alle Skripte aus
├── data/raw/                   Rohdaten (siehe unten)
├── data/processed/             Zwischendateien (werden erzeugt, nicht versioniert)
├── output/                     Tabellen und Abbildungen der Arbeit
├── renv.lock, renv/            Paketversionen
└── RSKRIPT.Rproj
```

## Designparameter

| Parameter | Wert |
|---|---|
| Behandelte Einheit | Deutschland |
| Donorpool | AUT, DNK, FIN, ISL, ITA, NOR, SWE |
| Outcome | Erwerbstätigenquote 15–64, saisonbereinigt |
| Beobachtungsfenster | 2005-Q1 bis 2019-Q4 (t = 1, …, 60) |
| Treatment | 2015-Q1 (t = 41) |
| Prädiktoren (Vorperiodenmittel) | BIP pro Kopf (KKP), BIP-Wachstum, Investitionsquote, Inflation, Erwerbsquote, Gewerkschaftsdichte |
| Outcome-Stützstellen | t = 8, 16, 24, 32, 40 |
| Seed | 20150101 |

## Ausgaben und ihre Verwendung in der Arbeit

| Datei | Skript | Verwendung |
|---|---|---|
| `synth_haupt_pfade.pdf` | 03 | Abbildung: Verlauf Deutschland und synthetische Kontrolle |
| `synth_haupt_gap.pdf` | 03 | Abbildung: geschätzte Lücke |
| `synth_pfade_haupt.csv` | 03 | Pfade, ATT und RMSPE der Hauptspezifikation |
| `synth_weights_haupt.csv` | 03 | Tabelle: Donorgewichte |
| `praediktorbalance.tex` | 03 | Tabelle: Prädiktorbalance (per `\input` eingebunden) |
| `placebo_rmspe_ranking.csv` | 03 | Tabelle: In-Space-Placebo |
| `placebo_plot.pdf` | 03 | Abbildung: In-Space-Placebo |
| `leave_one_out_summary.csv` | 03 | Robustheitstabelle: Leave-One-Out |
| `leave_one_out_plot.pdf` | 03 | Abbildung: Leave-One-Out |
| `antizipation_schmitz_kennzahlen.csv` | 03 | Rückdatierung auf 2014-Q3 |
| `tab_konvexe_huelle.csv` | 02 | Lage Deutschlands zur konvexen Hülle |
| `konvexe_huelle.pdf` | 02 | Abbildung: konvexe Hülle |
| `in_time_placebo_kennzahlen.csv` | 04 | Tabelle: In-Time-Placebo |
| `in_time_placebo_plot.pdf` | 04 | Abbildung: In-Time-Placebo |
| `ascm_vergleich.csv` | 05 | Robustheitstabelle: ASCM |
| `ascm_weights_ridge.csv` | 05 | Tabelle: ASCM-Gewichte |
| `ascm_gap_vergleich.pdf` | 05 | Abbildung: Lücke SCM und ASCM |
| `zeitler_bound.csv` | 06 | Sensitivitätsschranke und ihre Komponenten |
| `praediktorvariante_ohne_bip.csv` | 07 | Robustheitstabelle: Variante ohne BIP |
| `weights_V1_ohne_BIP.csv` | 07 | Gewichte der Variante ohne BIP |
| `diag_ohne_ud_invrate.csv` | 08 | Diagnoselauf ohne Investitionsquote und Gewerkschaftsdichte |
| `diag_weights_R1_ohne_UD_INV_J_7_.csv` | 08 | Gewichte des Diagnoselaufs |
| `quellen_log.csv` | 01 | Dokumentation: Quelle und Abdeckung je Variable |
| `sessionInfo.txt` | run_all | Dokumentation: R- und Paketversionen |

## Daten

Alle Rohdaten liegen in `data/raw/`. Das Panel ist balanciert (8 Länder × 60 Quartale), es fehlen keine Werte und es wird nichts imputiert. Jahreswerte (BIP pro Kopf, Gewerkschaftsdichte) werden auf die vier Quartale des jeweiligen Jahres übertragen.

| Datei | Variable | Quelle | Abruf |
|---|---|---|---|
| `EmploymentRateQuarterly.csv` | Erwerbstätigenquote 15–64 (Outcome) | OECD Data Explorer, `OECD.SDD.TPS:DSD_LFS@DF_IALFS_EMP_WAP_Q(1.0)`, saisonbereinigt | 17.08.2026 |
| `LabourForceParticipationQuarterly.csv` | Erwerbsquote 15–64 | OECD Data Explorer, `OECD.SDD.TPS:DSD_LFS@DF_IALFS_LF_WAP_Q(1.0)`, saisonbereinigt | 18.08.2026 |
| `CPIQuarterly.csv` | Inflation (VPI, Vorjahresrate) | OECD Data Explorer, `OECD.SDD.TPS:DSD_PRICES@DF_PRICES_ALL(1.0)` | 17.08.2026 |
| `QNA_T0102.csv` | Investitionsquote, BIP-Wachstum | OECD Quarterly National Accounts, Tabelle T0102, SDMX-API | 24.09.2026 |
| `GDPperCapita.csv` | BIP pro Kopf, KKP, konstante internationale Dollar von 2021 | Weltbank, World Development Indicators, `NY.GDP.PCAP.PP.KD` (Stand 13.07.2026) | 14.07.2026 |
| `ICTWSS_v2.csv` | Gewerkschaftsdichte (`UD`, ersatzweise `UD_s`) | OECD/AIAS ICTWSS-Datenbank | 28.07.2026 |

### QNA-Abfrage

`QNA_T0102.csv` wurde über folgende Abfrage bezogen:

```
https://sdmx.oecd.org/public/rest/data/OECD.SDD.NAD,DSD_NAMAIN1@DF_QNA,1.1/Q..DEU+AUT+DNK+FIN+ISL+ITA+NOR+SWE...._Z......T0102?startPeriod=2004-Q1&endPeriod=2019-Q4&dimensionAtObservation=AllDimensions&format=csv
```

MD5-Prüfsumme: `03911d631a7032a82dc2866e5551bb11`

Die OECD revidiert die Volkswirtschaftlichen Gesamtrechnungen laufend. Ein erneuter Abruf liefert deshalb in der Regel abweichende Werte. `01_build_panel.R` prüft die Prüfsumme und bricht ab, wenn die Datei nicht der in der Arbeit verwendeten Fassung entspricht. Um die Ergebnisse der Arbeit zu reproduzieren, ist die mitgelieferte Datei zu verwenden.

## Reproduzierbarkeit

- **Paketversionen:** `renv.lock`, wiederherstellbar mit `renv::restore()`.
- **Daten:** Alle Rohdaten liegen im Repository. Die QNA-Datei ist per Prüfsumme gesichert.
- **Zufall:** `00_setup.R` setzt einen festen Seed. Synth und die Ridge-ASCM sind mit den verwendeten Einstellungen deterministisch, der Seed dient als Absicherung.
- **Konfiguration:** Alle Designparameter stehen einmalig in `00_setup.R`. Ein `stopifnot` prüft Treatmentzeitpunkt, Fenster und Vorperiode auf Widerspruchsfreiheit.
- **Stand der Arbeit:** Der eingereichte Stand ist mit dem Git-Tag `abgabe` markiert.
