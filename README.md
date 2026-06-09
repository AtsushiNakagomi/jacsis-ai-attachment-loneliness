# AI social use → perceived AI-attachment → loneliness & social-network displacement

Analysis code for a two-wave (2024 → 2025) panel study testing whether **social/emotional
generative-AI use** shifts **perceived AI-attachment** and **perceived anthropomorphism**, and
whether either mediates change in **subjective loneliness** and **objective social-network
connection**, among 2025 generative-AI initiators.

This repository contains the analysis scripts, a variable/estimator codebook, and a synthetic
(schema-matched) dataset for a runnable smoke test. **The real survey data are not included**
(restricted access — see *Data*).

---

## Design in one paragraph

Among 2025 generative-AI initiators (n ≈ 2,490), the association between **AI-use purpose**
(categorical: None / Low / Mid / High user-tertiles; focal = social/emotional use **A_SE**, with
productivity/creative **A_PC** and daily/information **A_DI** as specificity checks) and three
outcomes — **UCLA-3 loneliness** and **LSNS-6 friends / family** network subscales at 2025 — is
decomposed through two **parallel single-mediator** analyses: **perceived AI-attachment**
(`W_attach`) and **perceived anthropomorphism** (`W_anthrop`). Each is estimated with the
VanderWeele 4-way natural-effect decomposition (CMAverse regression-based estimator, with an
exposure–mediator interaction). The two mediators are run separately (not jointly) because they
correlate r ≈ 0.65; a randomized-interventional-analogue sensitivity (g-formula) cross-checks the
attachment pathway net of anthropomorphism. Primary inference is a 6-test (2 mediators × 3
outcomes) Benjamini–Hochberg family on the A_SE cat4 omnibus indirect effect.

See **`CODEBOOK.md`** for full variable definitions, recoding, and estimator math.

---

## Data

The analyses use the JACSIS 2024 + 2025 two-wave panel, a **restricted-access** survey. **No data
are distributed in this repository** — `r_code/data/` is an empty placeholder. To run the pipeline,
place the analytic CSV at `r_code/data/jacsis_2wave2425.csv` (or point the environment variable
`JACSIS_2WAVE_2425_PATH` at it). Variable provenance is documented in `CODEBOOK.md`.

Mode is auto-detected from data presence (override with `AI_MOD_MODE=real|dummy`). The scripts also
recognize a schema-matched synthetic file at `r_code/data/dummy_24_25.csv` for an end-to-end dry run
(it cannot reproduce the substantive results); supply your own if you want one.

---

## Requirements

- R ≥ 4.5
- CRAN packages: `here`, `readr`, `psych`, `parallel`, `ggplot2`, `scales`, `dplyr`, `tidyr`,
  `GPArotation`, `car`, `EValue`
- **CMAverse** (regression-based / g-formula causal-mediation estimators), installed from GitHub:

  ```r
  # install.packages("remotes")
  remotes::install_github("BS1125/CMAverse")
  ```

  The closed-form sections still run if CMAverse is unavailable; the `cmest`-based sections skip.

---

## Layout

```
r_code/
  ai_attach_loneliness.R        # W_attach mediator → UCLA-3 loneliness
  ai_attach_lsns_friends.R      # W_attach → LSNS-friends
  ai_attach_lsns_family.R       # W_attach → LSNS-family
  ai_anthrop_loneliness.R       # W_anthrop mediator → UCLA-3
  ai_anthrop_lsns_friends.R     # W_anthrop → LSNS-friends
  ai_anthrop_lsns_family.R      # W_anthrop → LSNS-family
  run_all_mediation.R           # orchestrator: runs the 6 above + the packager
  ai_mediation_tables_figures.R # manuscript tables + figures (pure CSV reader)
  diagnosis_mediation.R         # pre-analysis diagnostics + EFA + collinearity/VIF check
  data/                         # placeholder — no data shipped; place jacsis_2wave2425.csv here
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
# Full pipeline (6 outcome scripts + manuscript packager, sequential, fresh R process per stage).
# The joint-mediator interventional sensitivity runs as section 6 of the three ai_attach_* scripts.
Rscript r_code/run_all_mediation.R

# Pre-analysis diagnostics + EFA (writes KMO/Bartlett/RMSEA/TLI + loadings):
Rscript r_code/diagnosis_mediation.R

# Manuscript tables + figures (after the outcome scripts have run):
Rscript r_code/ai_mediation_tables_figures.R
```

Bootstrap is R = 1,000 in real mode (R = 100 on dummy), 8 parallel socket workers, fixed seed.
Expected real-data runtime: ~70–100 min for the six outcome scripts + ~1 min for the packager.

Optional environment overrides:

```sh
AI_MOD_MODE=real|dummy                 # force a mode
JACSIS_2WAVE_2425_PATH=/path/to.csv    # real CSV outside r_code/data/
INTERV_SMOKE=1                         # interventional runner: High contrast only, nboot=50 preview
```

---

## Outputs

Written under `output/ai_mod/` (real) or `output/ai_mod/dummy/` (dummy):

- `mediation_<construct>_<outcome>/` — per-(mediator × outcome) analytic CSVs.
- `diagnosis_mediation/` — diagnostics + EFA (loadings, factor correlations, fit indices) + the
  centered-VIF collinearity table (`12_vif_outcome_model.csv`).
- `manuscript/{tables,figures}/` — Table 1–3 + Supplementary Tables 1–11 + Figures.

Reproducibility: every reported number traces to a logged `write_table()` call; do not hand-edit
the published artifacts — fix the script/CSV and regenerate.

---

## License & citation

Released under the **MIT License** (see `LICENSE`). Please cite the accompanying manuscript; the
JACSIS data are governed by their own access terms (the code is MIT; the data are not redistributed).
