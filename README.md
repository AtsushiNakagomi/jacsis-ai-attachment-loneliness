# AI social use → perceived AI-attachment → loneliness & social-network displacement

Analysis code for a two-wave (2024 → 2025) panel study testing whether **social/emotional
generative-AI use** shifts **perceived AI-attachment** and **perceived anthropomorphism**, and
whether either mediates change in **subjective loneliness** and **objective social-network
connection**, among 2025 generative-AI initiators.

This repository contains the analysis scripts and a variable/estimator codebook. **No data are
included** — the survey data are restricted-access (see *Data*).

---

## Design in one paragraph

Among 2025 generative-AI initiators (n ≈ 2,490), the association between **AI-use purpose**
(categorical: None / Low / Mid / High user-tertiles; focal = social/emotional use **A_SE**, with
productivity/creative **A_PC** and daily/information **A_DI** as specificity checks) and three
outcomes — **UCLA-3 loneliness** and **LSNS-6 friends / family** network subscales at 2025 — is
decomposed through two **parallel single-mediator** analyses: **perceived AI-attachment**
(`W_attach`) and **perceived anthropomorphism** (`W_anthrop`). Each is estimated with the
VanderWeele 4-way effect decomposition (CMAverse regression-based estimator, with an
exposure–mediator interaction), adjusted for 44 baseline-2024 covariates (including the baseline
outcomes and Big Five personality). Exposure, mediators and outcomes are all measured in the 2025
wave, so the components are reported as an associational decomposition under an assumed ordering,
not as causal mediation. The two mediators are run separately (not jointly) because they
correlate r ≈ 0.65; a randomized-interventional-analogue sensitivity (g-formula) cross-checks the
attachment estimate net of anthropomorphism. Primary inference is a 6-test (2 mediators × 3
outcomes) Benjamini–Hochberg family on the A_SE cat4 omnibus indirect effect. Robustness of the six
cells to baseline timing, attrition weighting and the composition of the attachment composite, and
measurement analyses of the attachment construct, are produced by two further scripts.

See **`CODEBOOK.md`** for full variable definitions, recoding, and estimator math.

---

## Data

The analyses use the JACSIS 2024 + 2025 two-wave panel, a **restricted-access** survey. **No data
are distributed in this repository** — `r_code/data/` is an empty placeholder. To run the pipeline,
place the analytic CSV at `r_code/data/jacsis_2wave2425.csv` (or point the environment variable
`JACSIS_2WAVE_2425_PATH` at it). The attrition-weighting analysis additionally reads the file of all
valid 2024 respondents, `r_code/data/jacsis_2024_all.csv` (or `JACSIS_2024_ALL_PATH`), which carries
the same 2024 variables plus the respondent identifier `Monitor_ID`, which is matched against the
two-wave file to derive the follow-up flag (a 0/1 column `followed_2025` is used directly if
present). Variable provenance is documented in `CODEBOOK.md`.

Mode is auto-detected from data presence (override with `AI_MOD_MODE=real|dummy`). The scripts also
recognize a schema-matched synthetic file at `r_code/data/dummy_24_25.csv` for an end-to-end dry run
(it cannot reproduce the substantive results); supply your own if you want one.

---

## Requirements

- R ≥ 4.5
- CRAN packages: `here`, `readr`, `psych`, `parallel`, `ggplot2`, `scales`, `dplyr`, `tidyr`,
  `GPArotation`, `car`
- **CMAverse** (regression-based / g-formula causal-mediation estimators), installed from GitHub:

  ```r
  # install.packages("remotes")
  remotes::install_github("BS1125/CMAverse")
  ```

  If installation fails (a lazy-loading error under some R builds), place an unmodified source
  checkout of CMAverse at `r_code/CMAverse_src/` (so that `r_code/CMAverse_src/DESCRIPTION`
  exists), or set the environment variable `CMAVERSE_SRC` to the path of such a checkout, and
  install `pkgload`; the scripts then load the package from source with `pkgload::load_all()`.
  The header line of every outcome-script log reports which route was used
  (`CMAverse: TRUE (package)` / `(pkgload_in_repo)` / `(pkgload_external)`) or the load error. The closed-form sections
  still run if CMAverse is unavailable; the `cmest`-based sections skip and the log says so.

---

## Layout

```
r_code/
  ai_attach_loneliness.R        # W_attach mediator → UCLA-3 loneliness
  ai_attach_lsns_friends.R      # W_attach → LSNS-friends 2025
  ai_attach_lsns_family.R       # W_attach → LSNS-family 2025
  ai_anthrop_loneliness.R       # W_anthrop mediator → UCLA-3
  ai_anthrop_lsns_friends.R     # W_anthrop → LSNS-friends
  ai_anthrop_lsns_family.R      # W_anthrop → LSNS-family
  ai_measurement_validity.R     # follow-up descriptives, construct distinctness (correlations, HTMT,
                                #   EFAs with UCLA-3 and problematic-use items), selection models,
                                #   baseline characteristics by AI status
  ai_robustness.R               # six primary cells under alternative specifications (baseline timing,
                                #   attrition IPW, two-item attachment); closed-form engine + bootstrap
  run_all_mediation.R           # orchestrator: runs the 6 outcome scripts + the 2 above + the packager
  ai_mediation_tables_figures.R # manuscript tables + figures (pure CSV reader)
  diagnosis_mediation.R         # pre-analysis diagnostics + EFA + collinearity/VIF check
  data/                         # placeholder — no data shipped; place jacsis_2wave2425.csv (and
                                #   jacsis_2024_all.csv for the attrition weights) here
  CMAverse_src/                 # optional — unmodified CMAverse source checkout, loaded with pkgload
                                #   when the package cannot be installed (not distributed here)
CODEBOOK.md                     # variable definitions + estimator math
README.md
LICENSE                         # MIT
output/                         # created at runtime (tables, figures, logs)
```

Each outcome script is **standalone** (it rebuilds the analytic dataset itself, by design — there
is intentional duplication of the build/§0 block across the six scripts so each can be run alone).

---

## How to run

From the repository root (with the working directory set there):

```r
# Full pipeline (6 outcome scripts + measurement-validity + robustness + manuscript packager,
# sequential, fresh R process per stage). The joint-mediator interventional sensitivity runs as
# section 6 of the three ai_attach_* scripts.
Rscript r_code/run_all_mediation.R

# Individual stages can also be run alone, e.g.
Rscript r_code/ai_measurement_validity.R
Rscript r_code/ai_robustness.R

# Pre-analysis diagnostics + EFA (writes KMO/Bartlett/RMSEA/TLI + loadings):
Rscript r_code/diagnosis_mediation.R

# Manuscript tables + figures (after the outcome scripts have run):
Rscript r_code/ai_mediation_tables_figures.R
```

Bootstrap is R = 1,000 in real mode (R = 100 on dummy), 8 parallel socket workers, fixed seed.
Real-data runtime on an 8-core laptop: about 3–7 h per outcome script (the g-formula interventional
section of the three `ai_attach_*` scripts and the bootstrap CMAverse calls dominate; roughly 26 h
for the six scripts in sequence), ~1 min for the measurement script, ~10–30 min for the robustness
script (27 cells × 1,000 bootstrap replicates, closed-form engine), ~1 min for the packager.

Optional environment overrides:

```sh
AI_MOD_MODE=real|dummy                 # force a mode
JACSIS_2WAVE_2425_PATH=/path/to.csv    # real CSV outside r_code/data/
JACSIS_2024_ALL_PATH=/path/to.csv      # full 2024 respondent file for the attrition weights
INTERV_SMOKE=1                         # interventional runner: High contrast only, nboot=50 preview
```

---

## Outputs

Written under `output/ai_mod/` (real) or `output/ai_mod/dummy/` (dummy):

- `mediation_<construct>_<outcome>/` — per-(mediator × outcome) analytic CSVs.
- `diagnosis_mediation/` — diagnostics + EFA (loadings, factor correlations, fit indices) + the
  centered-VIF collinearity table (`12_vif_outcome_model.csv`).
- `validity/` — follow-up descriptives, outcome SDs, correlations, reliability, HTMT, EFAs,
  selection models, baseline characteristics by AI status.
- `robustness/` — the six primary cells under the reference specification and robustness analyses
  S6–S9 (S1–S5 are the sensitivity analyses of the outcome scripts; numbering as in the manuscript,
  Section 3.3), the baseline-timing cross-tabulation, and the attrition model and weight summary.
- `manuscript/{tables,figures}/` — Tables 1, 2, 2b, 3, 3b + Supplementary Tables 1–17 + Figures.

Reproducibility: every reported number traces to a logged `write_table()` call; do not hand-edit
the published artifacts — fix the script/CSV and regenerate.

---

## License & citation

Released under the **MIT License** (see `LICENSE`). Please cite the accompanying manuscript; the
JACSIS data are governed by their own access terms (the code is MIT; the data are not redistributed).
