# Files to push for the revised manuscript (not part of the repository content — delete after use)

Repository: https://github.com/AtsushiNakagomi/jacsis-ai-attachment-loneliness (branch `main`).
The repository already contains `LICENSE` (MIT) and a `.gitignore`; keep `LICENSE` as it is and
replace `.gitignore` with the one in this folder (the GitHub R template plus the three project rules).

## Changed files (replace)

| file | what changed |
|---|---|
| `README.md` | 44 covariates incl. Big Five; associational framing; two new scripts; runtime; data statement (no data shipped, `Monitor_ID` for the attrition file); `CMAVERSE_SRC` fallback |
| `CODEBOOK.md` | Big Five covariates (§ covariates); §7.3 sensitivity analyses S1–S5; §7.8 robustness S6–S9; §7.9 measurement analyses; table map renumbered to the final Supplementary Data (Sup 1–17) |
| `r_code/ai_attach_loneliness.R`, `ai_attach_lsns_friends.R`, `ai_attach_lsns_family.R`, `ai_anthrop_loneliness.R`, `ai_anthrop_lsns_friends.R`, `ai_anthrop_lsns_family.R` | Big Five block (44 covariates, `stopifnot(length(C_VARS) == 44L)`); CMAverse loader (installed package → `r_code/CMAverse_src` → `CMAVERSE_SRC`); section banners labelled with manuscript analyses S1–S5 |
| `r_code/diagnosis_mediation.R` | 44 covariates; EFA cumulative-variance fix |
| `r_code/ai_mediation_tables_figures.R` | Table 1 Big Five rows and follow-up block; Table 3b; supplementary-table file names renumbered to the final Supplementary Data (Sup 1 AI-status groups … Sup 17 EFA); S6 rows written to `sup_table_13_two_item_attachment_S6`, REF + S7–S9 to `sup_table_14_robustness_S7_S9` |
| `r_code/run_all_mediation.R` | nine stages (adds `ai_measurement_validity.R` and `ai_robustness.R`) |
| `r_code/data/README.md` | `jacsis_2024_all.csv` with `Monitor_ID` |
| `.gitignore` | adds `r_code/output/`, `r_code/data/*.csv`, `r_code/CMAverse_src/` |

## New files (add)

| file | purpose |
|---|---|
| `r_code/ai_measurement_validity.R` | follow-up descriptives, outcome SDs, correlations, reliability, HTMT, EFAs (attachment + UCLA-3; attachment + anthropomorphism + problematic use), selection models, baseline characteristics by AI status |
| `r_code/ai_robustness.R` | six primary cells under REF and S6–S9 (two-item attachment; January-2025 baseline responders excluded; July–December 2025 initiators; attrition IPW), closed-form engine + bootstrap + joint Wald test |

## Do not push

`r_code/output/`, `r_code/CMAverse_src/`, this file, and `RUN_GUIDE_R1.md`. (`r_code/data/` now contains only `README.md`; the dummy data and codebooks were removed from the package and are excluded by `.gitignore` in any case.)

## Suggested commit message

    Revision 1: Big Five covariates (44), measurement-validity and robustness scripts, S1–S9 numbering, packager renumbered to the final Supplementary Data
