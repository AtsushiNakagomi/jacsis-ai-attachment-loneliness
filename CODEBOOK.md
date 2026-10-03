# Codebook — variable definitions and estimators

AI social use → perceived AI-attachment / anthropomorphism → subjective loneliness and behavioral
social-network displacement. Two-wave (2024 → 2025) panel, 2025 generative-AI initiators; 44 baseline-2024 covariates.

The design uses a **categorical exposure** (None + within-user tertiles) and **two parallel
single-mediator analyses** (perceived AI-attachment; perceived anthropomorphism), each applied to
**three outcomes** spanning two domains (subjective loneliness; objective network connection).

---

## 1. Sample

**Cohort filter.** `Q37S1_2025 ∈ {5, 6}` (initiated generative-AI use in 2025) AND present in both
waves. Never-users and earlier initiators are excluded: the perception mediators are asked of users
only, and 2025 initiation preserves the temporal ordering **covariates @ 2024 → use initiated 2025
→ mediators & outcomes @ 2025**. Analytic n ≈ 2,490.

| `Q37S1_2025` | Meaning | Action |
|---|---|---|
| 1 | Never used | exclude (mediator undefined) |
| 2 | Used before, not now | exclude |
| 3 | Started Nov 2022 – Dec 2023 | exclude (pre-baseline use) |
| 4 | Started Jan – Dec 2024 | exclude (initiation during baseline year) |
| 5 | Started Jan – Jun 2025 | **include** |
| 6 | Started Jul – Dec 2025 | **include** |

**Recode helpers.**

```r
ucla_recode <- function(M) pmax(0, pmin(3, 4 - M))   # UCLA-3 raw 1..4 -> 3..0 (higher = lonelier)
k6_recode   <- function(M) pmax(0, pmin(4, 5 - M))   # K6     raw 1..5 -> 4..0
lsns_recode <- function(M) pmax(0, pmin(5, M - 1L))  # LSNS-6 raw 1..6 -> 0..5 (higher = connected)
```

A real-mode sample-size assertion (`n >= 2000`) guards against silent data problems; a degeneracy
guard lets the synthetic file run to descriptives only.

---

## 2. Exposure (A) — three AI-use purposes; categorical (cat4) is primary

Three composites at the **2025** wave, each the row-mean of its items (raw 1–5 frequency) minus 1
(→ 0–4 scale). Each purpose is rotated as the focal exposure in turn; the other two enter the
covariate set per run.

### 2.1 Item → composite map (Q37S3, 2025)

| Q-code | Item | Composite |
|---|---|---|
| `Q37S3.1` | Document drafting / writing | A_PC |
| `Q37S3.2` | Translation / summarization | A_PC |
| `Q37S3.3` | Information search / lookup | A_DI |
| `Q37S3.4` | Image / video generation | A_PC |
| `Q37S3.5` | Learning support | A_PC |
| `Q37S3.6` | Daily-life planning | A_DI |
| `Q37S3.7` | Health advice / information | A_DI |
| `Q37S3.8` | Casual conversation / chat | A_SE |
| `Q37S3.9` | Emotional support / venting | A_SE |

A_SE = social/emotional (focal/headline), A_PC = productivity/creative, A_DI = daily/information.

### 2.2 Primary parameterization — cat4 (None + user-tertiles)

A_SE is heavily right-skewed (median 0; a large share of users do not use AI socially), so a
categorical exposure is more honest than a linear one. The **cat4** factor is None (= 0) plus
tertiles cut from each purpose's own *user* (> 0) distribution:

```r
make_cat4 <- function(x) {
  f <- rep(NA_character_, length(x)); f[x == 0] <- "None"; pos <- x > 0
  if (any(pos)) {
    qq  <- quantile(x[pos], c(1/3, 2/3), names = FALSE)
    br  <- unique(c(-Inf, qq, Inf))
    lab <- c("Low", "Mid", "High")[seq_len(length(br) - 1L)]
    f[pos] <- as.character(cut(x[pos], breaks = br, labels = lab, include.lowest = TRUE))
  }
  factor(f, levels = intersect(c("None", "Low", "Mid", "High"), unique(f)))
}
```

### 2.3 Sensitivity parameterizations

- **Binary** `any vs none` (robust to the mass at zero; the most-powered displacement contrast).
- **Continuous** symmetric ∓0.5-SD contrast (mean-centered `A_*_c`). Demoted from primary because
  of the skew.

---

## 3. Mediators (M) — two co-equal candidate mediators

Two **parallel single-mediator** analyses with the same exposure, outcomes, and pipeline. They are
tested **separately** (not in one joint model) because the two facets correlate r ≈ 0.65: at that
correlation, natural path-specific effects in a joint two-mediator model are not identified, so each
mediator is analyzed alone. The price is that each single-mediator estimate captures the effect
*transmitted through* that mediator (possibly relaying variance from the other); the interventional
sensitivity (§7.4) probes this directly.

### 3.1 Perceived AI-attachment (`W_attach`)

3-item mean of **Q40.22, Q40.23, Q40.24** (2025), 1–7 agreement; mean-centered → `W_attach_c`.

| Q-code | Item gloss | Flavor |
|---|---|---|
| `Q40.22` | Would like to become friends with gen AI | affiliation / attachment |
| `Q40.23` | More comfortable talking to gen AI than to real people | preference / substitution |
| `Q40.24` | Gen AI feels like a safe and secure place | safe-haven / attachment |

Cronbach's α ≈ 0.87. Construct label "perceived AI-attachment": two items use attachment-theory
phrasing (friendship, secure base), one uses preference-comparison phrasing.

### 3.2 Perceived anthropomorphism (`W_anthrop`)

3-item mean of **Q40.1, Q40.2, Q40.3** (2025), 1–7; mean-centered → `W_anthrop_c`.

| Q-code | Item gloss |
|---|---|
| `Q40.1` | Gen AI feels like it has human emotions/intentions |
| `Q40.2` | Talking to gen AI feels like conversation with a person |
| `Q40.3` | Gen AI seems like a living being rather than just a tool |

Cronbach's α ≈ 0.89; r with `W_attach` ≈ 0.65. A co-equal a priori candidate mediator (the dominant
mediator in the chatbot literature), carried alongside `W_attach` in the primary inference family
(§7) so the data adjudicate which facet (if any) mediates.

### 3.3 Exploratory factor analysis (measurement validation)

Two separate oblique EFAs validate the measurement model: one on the **9 AI-use purpose items**
(Q37S3.1–9), one on the **6 mediator items** (Q40.1–3, 22–24), on the analytic sample.

| Aspect | Choice |
|---|---|
| Input correlation | Pearson (Likert items with ≥ 5 categories treated as continuous) |
| Sampling adequacy | Kaiser–Meyer–Olkin (overall + per-item MSA); Bartlett's test of sphericity |
| Factor retention | Horn's parallel analysis (factor-analysis based, 20 iterations), cross-checked vs the hypothesized block count, capped at items⁄3 |
| Extraction | **minimum residual (MINRES / OLS)** |
| Rotation | **direct oblimin (oblique)** — factors are expected to correlate |
| Reported | pattern loadings, communality (h²), uniqueness, item complexity, inter-factor (oblique) correlation, variance explained, and fit indices (RMSEA + 90% CI, TLI, RMSR, BIC, model χ²) |

Caveat: a 6-item / 2-factor model has few degrees of freedom; KMO, Bartlett, communalities, and the
factor correlation are the informative adequacy checks there, with RMSEA/TLI more meaningful for the
9-item purposes EFA.

---

## 4. Outcomes (Y) — two domains, three outcomes

| Domain | Outcome | Construction | Scale / direction |
|---|---|---|---|
| Subjective loneliness | `Y_ucla3` | `rowSums(ucla_recode(Q66.1–3_2025))` | 0–9, **higher = lonelier** |
| Behavioral displacement (friends) | `Y_lsns_friends_2025` | `rowSums(lsns_recode(Q20.4–6_2025))` | 0–15, **higher = more connected** |
| Behavioral displacement (family) | `Y_lsns_family_2025` | `rowSums(lsns_recode(Q20.1–3_2025))` | 0–15, **higher = more connected** |

**Sign convention.** A *worsening* path is **positive** on UCLA-3 (more loneliness) but **negative**
on LSNS (less connection / network displacement).

**Change-from-baseline.** Baseline 2024 LSNS (`Q17.*`) and baseline UCLA-3 (`Q66.*_2024`) are in the
covariate set, so each 2025 outcome model is effectively change-from-baseline; the LSNS outcomes
operationalize behavioral social-network displacement.

---

## 5. Covariates (C) — 44 baseline-2024

All at the **2024** wave (strictly pre-exposure). Per focal run, the two non-focal purpose
composites (`A_*_c`) are appended.

- **Demographic / SES (15):** age; female; education (university, graduate; ref below);
  employment (executive, self-employed, non-regular, student, not-working; ref regular);
  income (2–6 m, 6–10 m, 10 m+, unknown; ref < 2 m JPY); married; living alone.
- **Psychological / social-network (8):** baseline LSNS-family, baseline LSNS-friends, baseline
  UCLA-3, baseline K6, ACE category (1, 2–3, 4+; ref 0), mental & physical health composite.
- **Lifestyle / time-use (16):** smartphone, PC/tablet, sitting, walking — each four dummies
  (1–2 h, 3–4 h, 5+ h, unknown) against a `0–<1 h/day` reference.
- **Big Five personality (5):** Ten-Item Personality Inventory, Japanese version (TIPI-J;
  `Q79.1–10`, 1–7 agreement). Each domain is the mean of its two items after reversing the
  reverse-keyed item (8 − x): extraversion (Q79.1, Q79.6R), agreeableness (Q79.2R, Q79.7),
  conscientiousness (Q79.3, Q79.8R), neuroticism (Q79.4, Q79.9R), openness (Q79.5, Q79.10R).

`stopifnot(length(C_VARS) == 44L)`.

---

## 6. Identification & timing

A two-wave panel with covariates (including the baseline outcomes) strictly at 2024 preceding the
2025 measurements (baseline-adjusted / lagged). However, exposure, mediators, and outcomes are all
measured at the **2025** wave, so the ordering "A precedes M precedes Y" and the absence of
exposure-induced M–Y confounding cannot be verified by timing. Report as an **associational effect
decomposition under an assumed ordering**, not causal mediation. The behavioral (LSNS)
displacement outcomes are the most timing-sensitive (a network outcome plausibly needs more than a
one-year lag).

**Baseline timing.** The 2024 wave was fielded December 2024 – January 2025 and `Q37S1_2025 = 5`
denotes initiation in January 2025 or later; only respondents who completed the baseline in
January 2025 could have initiated before it. `ai_robustness.R` re-estimates the six cells
excluding those respondents (specification S7; completion timestamp `回答完了日時_2024`, UTC ISO,
converted to Asia/Tokyo calendar month) and, separately, restricting to July–December 2025
initiators (S8, `Q37S1_2025 = 6`).

**Attrition.** All items are mandatory in the survey, so item non-response does not occur (income
and time-use carry explicit "unknown" categories). Loss to follow-up between 2024 and 2025 is
addressed by stabilized inverse-probability-of-response weights (specification S9): a logistic
model of 2025 response on the 44 baseline covariates among all valid 2024 respondents
(`data/jacsis_2024_all.csv`), predicted for the analytic cohort from its own baseline covariates,
stabilized by the marginal response rate, truncated at the 1st/99th percentiles, and applied to the
mediator and outcome models (weights held fixed across bootstrap replicates).

---

## 7. Estimators

### 7.1 Primary — cat4 mediation (CMAverse regression-based, 4-way)

For one focal purpose and each non-None level L:

```r
CMAverse::cmest(data = d, model = "rb", outcome = OUTCOME,
                exposure = paste0(focal, "_cat4"), mediator = "<W>_c",
                basec = c(C_VARS, paste0(setdiff(A_VARS, focal), "_c")),
                yreg = "linear", mreg = list("linear"),
                astar = "None", a = L, EMint = TRUE, mval = list(0),
                estimation = "imputation", inference = "bootstrap",
                nboot = R_BOOT, boot.ci.type = "per")
```

Yields the VanderWeele 4-way decomposition (CDE, INT_ref, INT_med, PNIE, PNDE, TNDE, TNIE, TE, PM).
`EMint = TRUE` (exposure–mediator interaction) is essential — the indirect effect is interaction-
dominated (INT_med carries it; PNIE ≈ 0). A_SE focal is the headline; A_PC / A_DI give specificity.

### 7.2 Closed-form natural effects (continuous exposure)

For continuous A with a symmetric ∓0.5-SD contrast, the linear-linear-with-interaction model
(mediator model coefficient β₁; outcome model θ₁ on A, θ₂ on M, θ₃ on A:M) gives, for `delta = SD`:

```
TNIE = β₁·delta·(θ₂ + θ₃·a)    PNIE = β₁·delta·(θ₂ + θ₃·a*)    INT_med = TNIE − PNIE
CDE  = delta·θ₁                PNDE = delta·(θ₁ + θ₃·β₁·a*)     TE = PNDE + TNIE
```

These match the CMAverse `rb` decomposition to floating-point precision; the closed form is used for
the parallelized continuous-exposure bootstrap (with a CMAverse cross-check).

### 7.3 Sensitivity analyses S1–S5 (outside the primary family, descriptive)

The manuscript (Section 3.3, "Sensitivity and robustness analyses") numbers nine analyses S1–S9 in
three groups: model and variable specification (S1–S4), measurement of the mediators (S5–S6), and
composition of the sample (S7–S9). S1–S5 run in the outcome scripts and are described here; S6–S9
run in `ai_robustness.R` (§7.8). S1–S3 and S5 run in **every** outcome script (after the §1 primary +
§1b omnibus); S4 runs in the three W_attach outcome scripts.

- **S1 Tertile mediator** (script §2; Supplementary Table 9) — the mediator coarsened to within-user tertiles
  (`cmest` rb with a multinomial mediator model), across all 3 purposes. A coarsening-robustness
  lens; categorical M carries a known precision/bias cost relative to continuous M.
- **S2 Binary exposure** (script §3; Supplementary Table 10) — `any vs none`, across all 3 purposes. Robust to the mass at
  zero; the most-powered behavioral-displacement contrast.
- **S3 Continuous exposure** (script §4; Supplementary Table 11) — symmetric ∓0.5-SD contrast on the mean-centered exposure,
  via the closed-form engine (§7.2) with a CMAverse `cmest` rb cross-check, across all 3 purposes.
  Demoted from primary because of the A_SE right-skew.
- **S5 Per-item mediator** (script §5; Supplementary Table 13) — each of the three composite items used individually as a
  single-item mediator (A_SE focal only; cat4 + binary), to check whether the composite signal
  rides on a single item.
- **S4 Joint-mediator interventional** (script §6; Supplementary Table 12; g-formula detail in §7.4) — because the two
  mediators correlate r ≈ 0.65, natural path-specific effects are unidentified; this estimates the
  **randomized interventional analogue** of each mediator's effect *net of the other* (treating the
  other mediator as a post-exposure confounder). A_SE focal, all 3 outcomes, both directions.

### 7.4 S4 — joint-mediator interventional (g-formula detail)

Sensitivity analysis S4 (§7.3). Because the two mediators correlate r ≈ 0.65, natural path-specific
effects are unidentified; the **randomized interventional analogue** is identified by treating the
*other* mediator as an exposure-induced (post-treatment) confounder. Two symmetric g-formula runs
(A_SE focal, per outcome): (a) mediator = attachment, post-confounder = anthropomorphism;
(b) the reverse.

```r
CMAverse::cmest(data = d, model = "gformula", outcome = OUTCOME, exposure = "A_SE_cat4",
                mediator = "<M>_c", basec = c(C_VARS, "A_PC_c", "A_DI_c"), postc = "<other M>_c",
                yreg = "linear", mreg = list("linear"), postcreg = list("linear"),
                astar = "None", a = L, EMint = TRUE, mval = list(0),
                estimation = "imputation", inference = "bootstrap", nboot = R_BOOT)
```

`model = "gformula"` returns **randomized interventional analogue** effects (named with an `r-`
prefix: `rtnie`, `rpnie`, `rintmed`, …); these are reported as such and are **not** relabeled as
natural effects.

### 7.5 Bootstrap

R = 1,000 in real mode (100 on the synthetic file). Eight parallel socket workers (L'Ecuyer-CMRG
stream); a fixed seed is set at script start and reset before each bootstrap section.

### 7.6 Exposure-contrast convention

- **Categorical (primary, §1):** factor level vs `None` (reference) — a separate `cmest` call per
  non-None level (Low/Mid/High vs None).
- **Binary (§3):** `any vs none`, one call per purpose.
- **Continuous (§4):** symmetric ∓0.5 SD of the focal exposure (effects per 1 SD).
- **Per-item (§5):** the §1 cat4 and §3 binary contrasts, with each single item substituted in as
  the mediator.

### 7.7 Primary inference family & multiplicity

The primary inference family (defined before estimation; the study was not preregistered) is **6 tests** = {attachment, anthropomorphism} × {UCLA-3, LSNS-friends,
LSNS-family}, focal exposure A_SE. Within a cell, the three cat4 dose contrasts (Low/Mid/High) are
**levels of one relationship, not separate hypotheses**; the cell test is an **omnibus joint
indirect-effect test** — a Wald χ² on the closed-form per-level TNIE vector with bootstrap
covariance. The 6 omnibus p-values are Benjamini–Hochberg corrected. A_PC/A_DI and all §7.3–7.4
sensitivities are descriptive (outside the family). Output: each outcome script writes its cell's
omnibus to `<construct>_omnibus_primary.csv`; the packager assembles the 6 and writes
`table_2_primary_omnibus_bh.csv` (manuscript Table 2) with the BH `q_BH`.

### 7.8 Robustness specifications S6–S9 (`ai_robustness.R`)

The six primary cells (A_SE cat4 focal) are re-estimated under: REF, the primary specification
(reference row); S6, a two-item attachment composite (Q40.22 + Q40.24; the three attachment cells);
S7, January-2025 baseline responders excluded; S8, July–December 2025 initiators only; S9, attrition
IPW. The estimator is the closed-form linear natural-effects engine (§7.2 generalized to a
categorical exposure: for level L vs None with mediator-model coefficient bL, outcome-model
coefficients t1L on A, t2 on M, t3L on A×M, and m0 the mean predicted mediator under None,
TNIE = (t2 + t3L)·bL, PNIE = t2·bL, INT_med = t3L·bL, CDE = t1L, INT_ref = t3L·m0,
PNDE = CDE + INT_ref, TE = PNDE + TNIE), with bootstrap percentile CIs (R = 1,000; 8 workers) and
the same joint Wald χ²(3) test on the TNIE vector as §7.7. User-tertile cut-points are those of the
full analytic cohort in every specification. Output: `robustness_cells.csv` (long),
`robustness_joint.csv`, `robustness_specs.csv`, `timing_baseline_month.csv`,
`ipw_attrition_model.csv`, `ipw_weights_summary.csv` → Supplementary Table 14 (reference and S7–S9); the S6
two-item attachment rows are reported with the S5 per-item results in Supplementary Table 13.

### 7.9 Measurement analyses (`ai_measurement_validity.R`)

- Follow-up descriptives: 2025 means (SD) of both mediators and the three outcomes, and change
  from the 2024 baseline, overall and by A_SE cat4 (Table 2b); 2025 outcome SDs used to express the
  primary TNIE, INT_med and TE per SD of the outcome (Table 3b).
- Construct distinctness: Pearson correlations among `W_attach`, the two-item `W_attach2`,
  `W_anthrop`, problematic generative-AI use (`PCUS`, mean of `Q38.1–11`, 2025, 1–7), UCLA-3 2025
  and 2024, LSNS-6 2025 and `A_SE`; item-level correlations of each attachment item with UCLA-3;
  Cronbach's α; HTMT ratios (Henseler, Ringle & Sarstedt 2015; < 0.85 criterion) for attachment
  vs UCLA-3, anthropomorphism and problematic use (Supplementary Table 15).
- EFAs (MINRES, oblimin, Horn's parallel analysis recorded; theoretical factor number fitted and the
  parallel-analysis solution written alongside when it differs): (a) attachment + UCLA-3 items
  (2025; UCLA items recoded so higher = lonelier), two factors expected; (b) attachment +
  anthropomorphism + problematic-use items, three factors expected (Supplementary Tables 16–17).
- Selection: any social/emotional use (logistic), social/emotional intensity, `W_attach` and
  `W_anthrop` (linear) regressed on the 44 baseline covariates; baseline UCLA-3, K6, LSNS and Big
  Five coefficients are the quantities of interest (Supplementary Table 6).
- Baseline characteristics of all two-wave respondents by generative-AI status at 2025 (never;
  used before, not now; 2022–23, 2024 and 2025 initiators) (Supplementary Table 1).

### 7.10 Collinearity / centered-VIF diagnostic

A model-side collinearity check (`diagnosis_mediation.R`, Diagnostic 12; requires `car`) guards the
exposure–mediator interaction against being inflated by near-collinear predictors. Per mediator it
fits the **mean-centered** outcome model `Y ~ A_*_c + W_c + A_*_c:W_c + C` and computes term-wise
`car::vif` through an alias-safe wrapper (centering makes the interaction VIFs interpretable; an
uncentered product is mechanically collinear with its main effects). It is outcome-independent (the
predictor matrix is the same across outcomes), so it is computed once per mediator → table
`12_vif_outcome_model.csv` (`mediator`, `term`, `vif`, `flag`, `n`); any term with VIF > 5 flags
`high`. On the analytic sample all predictor terms stay below 5 for both mediators (the A×W
interactions ≈ 1.3–1.8), confirming the interaction is identifiable with no harmful collinearity.

---

## 8. Pipeline

| Script | Role |
|---|---|
| `ai_attach_loneliness.R`, `ai_attach_lsns_friends.R`, `ai_attach_lsns_family.R` | attachment mediator × 3 outcomes |
| `ai_anthrop_loneliness.R`, `ai_anthrop_lsns_friends.R`, `ai_anthrop_lsns_family.R` | anthropomorphism mediator × 3 outcomes |
| `run_all_mediation.R` | orchestrator — runs the 6 outcome scripts, the validity and robustness scripts, and the packager in fresh R processes |
| `ai_measurement_validity.R` | follow-up descriptives, construct distinctness (correlations, HTMT, EFAs), selection models, AI-status groups (§7.9) |
| `ai_robustness.R` | six primary cells under the reference specification and S6–S9 (§7.8) |
| `ai_mediation_tables_figures.R` | manuscript tables + figures (downstream CSV reader; no analytic logic) |
| `diagnosis_mediation.R` | pre-analysis diagnostics + EFA (measurement validation) + centered-VIF collinearity check |

Each outcome script performs: build + cat4/tertile/binary factors → §1 cat4 primary → §1b cat4
omnibus (A_SE focal) → sensitivity analyses S1, S2, S3, S5 (§7.3: tertile / binary / continuous /
per-item). The three W_attach outcome scripts additionally run **S4** (§6, the
joint-mediator interventional; the second mediator is built locally there, so the shared build block
stays identical across all six scripts).

---

## 9. Outputs

```
output/ai_mod/[dummy/]
  mediation_<construct>_<outcome>/{tables,figures,logs}/   # one folder per (mediator × outcome)
  diagnosis_mediation/                                     # diagnostics + EFA
  validity/{tables,logs}/                                  # measurement analyses (§7.9)
  robustness/{tables,logs}/                                # robustness specifications (§7.8)
  manuscript/{tables,figures,logs}/                        # packaged tables + figures
  logs/                                                    # orchestrator master log + timings
```

---

## 10. Manuscript tables & figures

| Artifact | Contents |
|---|---|
| Table 1 | Cohort characteristics, Total + by A_SE cat4 |
| Table 2 | Primary multiplicity — cat4 omnibus joint indirect-effect p per (mediator × outcome), BH-FDR over 6 |
| Table 2b | Follow-up (2025) mediators and outcomes, and change from baseline, Total + by A_SE cat4 |
| Table 3 | Primary cat4 A_SE decomposition × 3 outcomes × both mediators (full 4-way, side-by-side) |
| Table 3b | Primary TNIE / INT_med / TE per SD of the 2025 outcome |
| Sup 1 | baseline characteristics by generative-AI status at 2025 (all two-wave respondents) |
| Sup 2 / 3 | EFA — AI-use purposes / mediators: loadings (+ h²/u²/complexity/MSA) + factor correlations + fit (KMO, Bartlett, RMSEA, TLI, RMSR, BIC) |
| Sup 4 / 5 | Cohort characteristics by A_PC / A_DI cat4 |
| Sup 6 | baseline predictors of 2025 social/emotional use and of the two perceptions |
| Sup 7 / 8 | cat4 A_PC / A_DI specificity × both mediators |
| Sup 9 / 10 / 11 | S1 tertile-mediator / S2 binary / S3 continuous sensitivities × both mediators × 3 purposes |
| Sup 12 | S4 joint-mediator interventional (g-formula) × 3 outcomes, both configurations |
| Sup 13 | S5 per-item cat4 A_SE × 3 outcomes × 6 single items + S6 two-item attachment composite (`sup_table_13_two_item_attachment_S6`) |
| Sup 14 | robustness: reference and S7–S9 × six cells (+ baseline-timing cross-tabulation, attrition model, weight summary) |
| Sup 15 | correlations, item-level correlations, reliability, HTMT |
| Sup 16 / 17 | EFA attachment + UCLA-3 items / attachment + anthropomorphism + problematic-use items |
| Figure 1 | Working model and decomposition (drawn manually) |
| Sup Figure 1 | Sample flow chart |
