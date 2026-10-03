# =============================================================================
# diagnosis_mediation.R — pre-analysis data + scale diagnostics for the
# MEDIATION study (README.md / CODEBOOK.md)..
#
# Scope: ONLY the items the mediation study actually uses —
#   - 2 mediators : W_attach (perceived AI-attachment, Q40.22-24) and
#                   W_anthrop (perceived anthropomorphism, Q40.1-3).
#                   The other 7 Q40 subscales are NOT touched here.
#   - 3 exposures : A_SE / A_PC / A_DI AI-use purposes (Q37S3 items).
#   - 3 outcomes  : UCLA-3 loneliness + LSNS-friends/family @ 2025.
#   - 44 baseline-2024 covariates (C).
#
# Standalone — duplicates the §0 build of the mediation outcome scripts by
# intention (no source()); does NOT modify or read from diagnosis.R. All
# diagnostics are analytic (no bootstrap).
#
# Diagnostics:
#   1.  Cohort flow                 — n at each filter step (2025 initiators)
#   2.  Missingness (used items)    — % missing per used variable (6 mediator
#                                     items + 9 purpose items + 3 outcomes + 44C)
#   3.  Variable construction sanity — range/mean/SD/N for 3 A, 2 mediators,
#                                     3 outcomes, baselines
#   4.  Cronbach's α                — W_attach + W_anthrop + 3 A composites +
#                                     UCLA-3 baseline/outcome + K6 + LSNS subscales
#   5.  Mediator subscale structure — per mediator: inter-item r, item-total r,
#                                     α-if-item-deleted (2 mediators only)
#   6.  Construct correlation       — {W_attach, W_anthrop, A_SE, A_PC, A_DI}
#                                     matrix; flags r(W_attach, W_anthrop)
#   7.  A_purpose × Y descriptive   — 3×3 inter-A + per-purpose crude A→UCLA OLS
#   8.  EFA on the 3 AI-use purposes — parallel analysis + oblique EFA on the 9
#                                     Q37S3 items (validates A_SE/A_PC/A_DI); emits
#                                     loadings (+ h2/u2/complexity/MSA per item),
#                                     variance, factor cor, AND fit diagnostics
#                                     (KMO, Bartlett, RMSEA, TLI, RMSR, BIC, chi2)
#   9.  EFA on the 2 mediators      — parallel analysis + oblique EFA on the 6
#                                     mediator items (same fit-diagnostic outputs);
#                                     the 2-factor oblique factor correlation is the
#                                     empirical ≈0.65 that justifies the parallel
#                                     single-mediator design (codebook §3)
#       EFA outputs per block <tag> = 08_efa_purposes / 09_efa_mediators:
#         <tag>_parallel.csv (eigenvalues + suggested nfact)
#         <tag>_loadings.csv (pattern loadings + theoretical_group + h2/u2/complexity/MSA_item)
#         <tag>_variance.csv (SS loadings / prop / cumulative)
#         <tag>_factor_cor.csv (oblique Phi, >1 factor)
#         <tag>_fit.csv  (KMO_overall, Bartlett chi2/df/p, model chi2/df/p, RMSEA[+CI], TLI, RMSR, BIC, cum_var)
#   10. Straight-line responding    — identical-value prevalence across the 6
#                                     mediator items (acquiescence/halo check)
#   11. Mediation analytic readiness — LSNS-2025 construction from Q20.1-6,
#                                     per-outcome complete-case n, A_SE cat4 cells
#                                     vs locked (None=1470/Low=414/
#                                     Mid=334/High=272), W_attach & W_anthrop α + r
#   12. Centered-VIF                  — collinearity of the mean-centered outcome-model
#                                     predictors (3 A_*_c + W_c + 3 A_*_c×W_c + 44C),
#                                     one table per mediator → 12_vif_outcome_model.csv
#                                     (flags any term VIF > 5; needs the `car` package)
#
# Inputs:  r_code/data/dummy_24_25.csv (dummy) or JACSIS_2WAVE_2425_PATH (real).
#          AI_MOD_MODE ("REAL"/"DUMMY") overrides auto-detect.
# Outputs: output/ai_mod/diagnosis_mediation/  (real) or
#          output/ai_mod/dummy/diagnosis_mediation/  (dummy).
# =============================================================================

set.seed(20260524)

suppressPackageStartupMessages({
  library(here)
  library(readr)
  library(psych)
})

# -------- Paths / mode (identical convention to the mediation scripts) -------
find_proj_root <- function() {
  candidates <- character(0)
  ch <- tryCatch(here::here(), error = function(e) NA_character_); if (!is.na(ch)) candidates <- c(candidates, ch)
  args <- commandArgs(trailingOnly = FALSE); fa <- args[grepl("^--file=", args)]
  if (length(fa)) { sd <- tryCatch(normalizePath(dirname(sub("^--file=", "", fa[1])), winslash = "/"), error = function(e) NA_character_); if (!is.na(sd)) candidates <- c(candidates, sd, dirname(sd)) }
  candidates <- c(candidates, getwd(), dirname(getwd()))
  for (cand in unique(candidates)) {
    if (!nzchar(cand)) next
    if (basename(cand) == "r_code" && dir.exists(file.path(cand, "data"))) return(normalizePath(cand, winslash = "/"))
    if (dir.exists(file.path(cand, "r_code", "data"))) return(normalizePath(file.path(cand, "r_code"), winslash = "/"))
  }
  stop("Could not find r_code/ (looking for a dir whose data/ subdir exists)")
}
.proj_root <- find_proj_root()
stopifnot(dir.exists(file.path(.proj_root, "data")))
.norm <- function(p) normalizePath(p, winslash = "/", mustWork = FALSE)

DUMMY_DATA_PATH <- .norm(file.path(.proj_root, "data", "dummy_24_25.csv"))
REAL_DATA_PATH  <- Sys.getenv("JACSIS_2WAVE_2425_PATH", unset = "")
if (!nzchar(REAL_DATA_PATH)) REAL_DATA_PATH <- file.path(.proj_root, "data", "jacsis_2wave2425.csv")
REAL_DATA_PATH  <- .norm(REAL_DATA_PATH)

.env_mode <- toupper(Sys.getenv("AI_MOD_MODE", unset = ""))
RUN_MODE <- if (.env_mode == "REAL") "real" else if (.env_mode == "DUMMY") "dummy" else
            if (file.exists(REAL_DATA_PATH)) "real" else if (file.exists(DUMMY_DATA_PATH)) "dummy" else
            stop("No input CSV found. Place dummy_24_25.csv or jacsis_2wave2425.csv at r_code/data/, or set JACSIS_2WAVE_2425_PATH.")
DATA_PATH <- if (RUN_MODE == "real") REAL_DATA_PATH else DUMMY_DATA_PATH

OUT_BASE <- file.path(.proj_root, "output", "ai_mod",
                      if (RUN_MODE == "real") "diagnosis_mediation" else file.path("dummy", "diagnosis_mediation"))
dir.create(OUT_BASE, recursive = TRUE, showWarnings = FALSE)

.timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
LOG_FILE   <- file.path(OUT_BASE, sprintf("diagnosis_mediation_%s.log", .timestamp))
FLAGS_FILE <- file.path(OUT_BASE, sprintf("flags_%s.md", .timestamp))

log_msg <- function(...) { m <- paste0(format(Sys.time(), "[%H:%M:%S] "), paste(..., collapse = " ")); cat(m, "\n", sep = ""); cat(m, "\n", sep = "", file = LOG_FILE, append = TRUE); invisible(m) }
.flags_rejected <- list(); .flags_warned <- list()
add_flag <- function(level, item, rule, detail) {
  row <- data.frame(item = item, rule = rule, detail = detail, stringsAsFactors = FALSE)
  if (level == "reject") .flags_rejected[[length(.flags_rejected) + 1L]] <<- row
  else                    .flags_warned[[length(.flags_warned) + 1L]]    <<- row
}

log_msg("=== diagnosis_mediation.R START ===  mode:", RUN_MODE)
log_msg("input:", DATA_PATH); log_msg("output base:", OUT_BASE)

# -------- Helpers -----------------------------------------------------------
as_num <- function(x) suppressWarnings(as.numeric(x))
row_mean <- function(d, cols, na_rm = FALSE) { cols <- intersect(cols, names(d)); if (!length(cols)) return(rep(NA_real_, nrow(d))); rowMeans(vapply(d[cols], as_num, numeric(nrow(d))), na.rm = na_rm) }
row_sum_fn <- function(d, cols, fn = identity, na_rm = FALSE) { cols <- intersect(cols, names(d)); if (!length(cols)) return(rep(NA_real_, nrow(d))); rowSums(vapply(d[cols], function(v) fn(as_num(v)), numeric(nrow(d))), na.rm = na_rm) }
ucla_recode <- function(M) pmax(0, pmin(3, 4 - M))
k6_recode   <- function(M) pmax(0, pmin(4, 5 - M))
lsns_recode <- function(M) pmax(0, pmin(5, M - 1L))
make_timeuse_dummies <- function(d, raw_col, prefix) {
  v <- as_num(d[[raw_col]])
  d[[paste0(prefix, "_band_1_2")]]   <- as.integer(v %in% c(4L, 5L))
  d[[paste0(prefix, "_band_3_4")]]   <- as.integer(v %in% c(6L, 7L))
  d[[paste0(prefix, "_band_5plus")]] <- as.integer(v %in% c(8L, 9L, 10L, 11L))
  d[[paste0(prefix, "_unknown")]]    <- as.integer(is.na(v) | v == 12L)
  d
}

# -------- USED item specs (mediation study only) ----------------------------
# Two mediators.
MED_SPEC <- list(
  W_attach  = paste0("Q40.", 22:24, "_2025"),  # perceived AI-attachment (PRIMARY)
  W_anthrop = paste0("Q40.",  1:3,  "_2025")   # perceived anthropomorphism
)
MED_VARS <- names(MED_SPEC)
stopifnot(length(MED_VARS) == 2L)

# Three AI-use purposes (exposure composites; scale 0-4 after −1).
A_SPEC <- list(
  A_SE = paste0("Q37S3.", c(8, 9),       "_2025"),
  A_PC = paste0("Q37S3.", c(1, 2, 4, 5), "_2025"),
  A_DI = paste0("Q37S3.", c(3, 6, 7),    "_2025")
)
A_VARS <- names(A_SPEC)
stopifnot(length(A_VARS) == 3L)

# -------- Load + construct variables ----------------------------------------
df <- readr::read_csv(DATA_PATH, show_col_types = FALSE)
log_msg(sprintf("Loaded %d rows × %d cols", nrow(df), ncol(df)))
n_d <- nrow(df)
safe_col <- function(cn) if (cn %in% names(df)) as_num(df[[cn]]) else rep(NA_real_, n_d)

# Outcomes
df$Y_ucla3 <- row_sum_fn(df, paste0("Q66.", 1:3, "_2025"), fn = ucla_recode)

# Exposure composites (mean − 1 → 0-4) + mediator composites (1-7 mean)
ai_start <- safe_col("Q37S1_2025"); df$ai_start <- ai_start
for (a in A_VARS)   df[[paste0(a, "_continuous")]] <- row_mean(df, A_SPEC[[a]], na_rm = FALSE) - 1
for (m in MED_VARS) df[[m]] <- row_mean(df, MED_SPEC[[m]], na_rm = FALSE)

# Baselines + 44 covariates (identical to the mediation scripts' build)
df$baseline_ucla3 <- row_sum_fn(df, paste0("Q66.", 1:3, "_2024"), fn = ucla_recode)
df$baseline_k6    <- row_sum_fn(df, paste0("Q65.", 1:6, "_2024"), fn = k6_recode)
ace_cols <- intersect(paste0("Q77.", c(1:8, 13), "_2024"), names(df))
if (length(ace_cols)) {
  ap <- vapply(df[ace_cols], function(v) as.integer(as_num(v) == 1L), integer(nrow(df)))
  aps <- rowSums(ap, na.rm = TRUE)
  if ("Q77.9_2024" %in% names(df)) { q9 <- as_num(df$Q77.9_2024); a9 <- as.integer(q9 == 2L); a9[is.na(q9)] <- 0L } else a9 <- 0L
  df$ace_score <- aps + a9
} else df$ace_score <- NA_real_
df$ace_1 <- as.integer(df$ace_score == 1L); df$ace_2_3 <- as.integer(df$ace_score %in% 2:3); df$ace_4plus <- as.integer(df$ace_score >= 4L)
df$age_2024 <- safe_col("AGE_2024")
df$sex_female <- as.integer(safe_col("SEX_2024") == 2L)
edu <- safe_col("Q21.1_2024"); df$edu_univ <- as.integer(edu %in% 6:8); df$edu_grad <- as.integer(edu == 9L)
emp <- safe_col("Q5.1_2024")
df$emp_exec <- as.integer(emp == 1L); df$emp_self <- as.integer(emp %in% 2:4)
df$emp_nonreg <- as.integer(emp %in% 7:11); df$emp_student <- as.integer(emp %in% 12:13)
df$emp_notwork <- as.integer(emp %in% 14:16 | is.na(emp))
inc <- safe_col("Q80.1_2024")
df$income_2_6m <- as.integer(inc %in% 5:8); df$income_6_10m <- as.integer(inc %in% 9:12)
df$income_10m_plus <- as.integer(inc %in% 13:18); df$income_unknown <- as.integer(is.na(inc) | inc %in% c(19L, 20L))
df$married <- as.integer(safe_col("Q2_2024") %in% 1:3)
liv <- safe_col("Q1.1_2024"); df$living_alone <- as.integer(!is.na(liv) & liv == 1L)
df$baseline_lsns6_family  <- row_sum_fn(df, paste0("Q17.", 1:3, "_2024"), fn = lsns_recode)
df$baseline_lsns6_friends <- row_sum_fn(df, paste0("Q17.", 4:6, "_2024"), fn = lsns_recode)
df$mental_physical_health <- row_mean(df, c("Q76.3_2024", "Q76.4_2024"))
df <- make_timeuse_dummies(df, "Q28.13_2024", "smartphone"); df <- make_timeuse_dummies(df, "Q28.14_2024", "pc_tablet")
df <- make_timeuse_dummies(df, "Q28.5_2024",  "sitting");    df <- make_timeuse_dummies(df, "Q28.6_2024",  "walking")

# Big Five personality (TIPI-J, Q79.1-10 @ 2024; 1-7 agreement). Each domain = mean of its
# two items after reversing the reverse-keyed item (8 - x): Q79.1/6R extraversion,
# Q79.2R/7 agreeableness, Q79.3/8R conscientiousness, Q79.4/9R neuroticism, Q79.5/10R openness.
tipi_pair <- function(d, fwd, rev) { a <- as_num(d[[fwd]]); b <- 8 - as_num(d[[rev]]); (a + b) / 2 }
df$big5_extraversion     <- tipi_pair(df, "Q79.1_2024", "Q79.6_2024")
df$big5_agreeableness    <- tipi_pair(df, "Q79.7_2024", "Q79.2_2024")
df$big5_conscientiousness <- tipi_pair(df, "Q79.3_2024", "Q79.8_2024")
df$big5_neuroticism      <- tipi_pair(df, "Q79.4_2024", "Q79.9_2024")
df$big5_openness         <- tipi_pair(df, "Q79.5_2024", "Q79.10_2024")

C_VARS <- c(
  "age_2024", "sex_female", "edu_univ", "edu_grad",
  "emp_exec", "emp_self", "emp_nonreg", "emp_student", "emp_notwork",
  "income_2_6m", "income_6_10m", "income_10m_plus", "income_unknown", "married", "living_alone",
  "baseline_lsns6_family", "baseline_lsns6_friends", "baseline_ucla3", "baseline_k6",
  "ace_1", "ace_2_3", "ace_4plus", "mental_physical_health",
  "smartphone_band_1_2", "smartphone_band_3_4", "smartphone_band_5plus", "smartphone_unknown",
  "pc_tablet_band_1_2",  "pc_tablet_band_3_4",  "pc_tablet_band_5plus",  "pc_tablet_unknown",
  "sitting_band_1_2",    "sitting_band_3_4",    "sitting_band_5plus",    "sitting_unknown",
  "walking_band_1_2",    "walking_band_3_4",    "walking_band_5plus",    "walking_unknown",
  "big5_extraversion", "big5_agreeableness", "big5_conscientiousness", "big5_neuroticism", "big5_openness"
)
stopifnot(length(C_VARS) == 44L)

A_cont_vars <- paste0(A_VARS, "_continuous")

# Cohort filter: 2025 AI initiators only (Q37S1_2025 ∈ {5, 6})
ds <- df[ai_start %in% c(5L, 6L), , drop = FALSE]
n_cohort <- nrow(ds)
log_msg(sprintf("Cohort (initiators, codes 5+6): n = %d", n_cohort))

# Global complete-case on the mediation analytic set (Y_ucla3 + 3A + 2 mediators + 44C)
all_used <- unique(c("Y_ucla3", A_cont_vars, MED_VARS, C_VARS))
n_used_cca <- sum(complete.cases(ds[, intersect(all_used, names(ds)), drop = FALSE]))
log_msg(sprintf("Mediation analytic CCA (UCLA + 3A + 2 mediators + 44C) n = %d (%.1f%% of cohort)",
                n_used_cca, 100 * n_used_cca / max(n_cohort, 1L)))
if (n_used_cca < 100L) add_flag("warn", "mediation CCA n", "n < 100",
  sprintf("only %d survive CC on UCLA + 3A + 2 mediators + 44C; per-diagnostic CCA used (expected on dummy; real ≈ 2,490)", n_used_cca))

# =============================================================================
# Diagnostic 1 — Cohort flow
# =============================================================================
log_msg("\n[1] Cohort flow")
flow_df <- data.frame(
  step = c("raw_2wave_panel", "excluded_code1_never_user", "excluded_code2_past_users",
           "excluded_code3_pre_2024", "excluded_code4_during_2024", "excluded_NA_ai_start",
           "cohort_kept_codes_5_6_initiators", "mediation_CCA_UCLA_3A_2med_44C"),
  n = c(nrow(df), sum(ai_start == 1L, na.rm = TRUE), sum(ai_start == 2L, na.rm = TRUE),
        sum(ai_start == 3L, na.rm = TRUE), sum(ai_start == 4L, na.rm = TRUE),
        sum(is.na(ai_start)), n_cohort, n_used_cca),
  stringsAsFactors = FALSE)
write.csv(flow_df, file.path(OUT_BASE, "01_cohort_flow.csv"), row.names = FALSE, fileEncoding = "UTF-8")
for (i in seq_len(nrow(flow_df))) log_msg(sprintf("  %-35s  n = %d", flow_df$step[i], flow_df$n[i]))

# =============================================================================
# Diagnostic 2 — Missingness (USED items only, full cohort pre-CCA)
# =============================================================================
log_msg("\n[2] Missingness audit (used items only)")
miss_vars <- unique(c("Y_ucla3", A_cont_vars, MED_VARS, C_VARS,
                      unlist(A_SPEC), unlist(MED_SPEC)))
miss_vars <- intersect(miss_vars, names(ds))
miss_df <- do.call(rbind, lapply(miss_vars, function(v) {
  x <- ds[[v]]; n_total <- length(x); n_miss <- sum(is.na(x)); pct <- 100 * n_miss / n_total
  data.frame(variable = v, n_total = n_total, n_missing = n_miss, pct_missing = pct,
             flag = ifelse(pct > 10, "severe", ifelse(pct > 5, "mild", "ok")), stringsAsFactors = FALSE)
}))
miss_df <- miss_df[order(-miss_df$pct_missing), , drop = FALSE]
write.csv(miss_df, file.path(OUT_BASE, "02_missingness.csv"), row.names = FALSE, fileEncoding = "UTF-8")
n_sev <- sum(miss_df$flag == "severe")
log_msg(sprintf("  %d severe (>10%%), %d mild (5-10%%), %d ok (≤5%%)",
                n_sev, sum(miss_df$flag == "mild"), sum(miss_df$flag == "ok")))
if (n_sev > 0) for (i in seq_len(min(5, n_sev))) {
  log_msg(sprintf("    severe: %-30s %5.2f%% missing", miss_df$variable[i], miss_df$pct_missing[i]))
  add_flag("warn", miss_df$variable[i], "missingness > 10%", sprintf("%.1f%% missing in cohort", miss_df$pct_missing[i]))
}

# =============================================================================
# Diagnostic 3 — Variable construction sanity
# =============================================================================
log_msg("\n[3] Variable construction sanity (cohort; per-variable available n)")
# Build LSNS-2025 outcomes for the sanity + readiness checks
lsns25_fr <- paste0("Q20.", 4:6, "_2025"); lsns25_fa <- paste0("Q20.", 1:3, "_2025")
have_lsns25 <- all(c(lsns25_fr, lsns25_fa) %in% names(ds))
if (have_lsns25) {
  ds$Y_lsns_friends_2025 <- row_sum_fn(ds, lsns25_fr, fn = lsns_recode)
  ds$Y_lsns_family_2025  <- row_sum_fn(ds, lsns25_fa, fn = lsns_recode)
}
summarize_cont <- function(x, varname) {
  n_ok <- sum(is.finite(x))
  if (n_ok == 0L) return(data.frame(variable = varname, n = 0L, summary = "no non-missing values", stringsAsFactors = FALSE))
  data.frame(variable = varname, n = n_ok,
             summary = sprintf("mean=%.3f SD=%.3f range=[%.2f, %.2f]",
                               mean(x, na.rm = TRUE), sd(x, na.rm = TRUE), min(x, na.rm = TRUE), max(x, na.rm = TRUE)),
             stringsAsFactors = FALSE)
}
sanity_targets <- c("Y_ucla3",
                    if (have_lsns25) c("Y_lsns_friends_2025", "Y_lsns_family_2025"),
                    A_cont_vars, MED_VARS, "baseline_ucla3", "baseline_k6", "ace_score")
sanity_df <- do.call(rbind, lapply(sanity_targets, function(v) summarize_cont(ds[[v]], v)))
write.csv(sanity_df, file.path(OUT_BASE, "03_variable_sanity.csv"), row.names = FALSE, fileEncoding = "UTF-8")
for (i in seq_len(nrow(sanity_df))) log_msg(sprintf("  %-26s n=%5d  %s", sanity_df$variable[i], sanity_df$n[i], sanity_df$summary[i]))
# range checks: A on [0,4]; mediators on [1,7]; UCLA on [0,9]
range_check <- function(x, nm, lo, hi) { x <- x[is.finite(x)]; if (!length(x)) return(invisible(NULL))
  if (min(x) < lo - 1e-3 || max(x) > hi + 1e-3) add_flag("warn", nm, sprintf("out of expected [%g, %g]", lo, hi), sprintf("observed [%.2f, %.2f]", min(x), max(x))) }
for (a in A_cont_vars) range_check(ds[[a]], a, 0, 4)
for (m in MED_VARS)    range_check(ds[[m]], m, 1, 7)
range_check(ds$Y_ucla3, "Y_ucla3", 0, 9)

# =============================================================================
# Diagnostic 4 — Cronbach's α (2 mediators + 3 purposes + baselines/outcome)
# =============================================================================
log_msg("\n[4] Cronbach's α (internal consistency)")
alpha_for <- function(items, data) {
  items <- intersect(items, names(data)); if (length(items) < 2L) return(list(alpha = NA_real_, n_eff = 0L))
  m <- as.matrix(data[, items, drop = FALSE]); mode(m) <- "numeric"; m <- m[complete.cases(m), , drop = FALSE]
  if (nrow(m) < 20L) return(list(alpha = NA_real_, n_eff = nrow(m)))
  a <- tryCatch(suppressWarnings(suppressMessages(psych::alpha(m, na.rm = TRUE, warnings = FALSE)$total$raw_alpha)), error = function(e) NA_real_)
  list(alpha = a, n_eff = nrow(m))
}
alpha_specs <- c(
  setNames(lapply(MED_VARS, function(m) MED_SPEC[[m]]), MED_VARS),
  setNames(lapply(A_VARS,   function(a) A_SPEC[[a]]),   A_VARS),
  list(`UCLA-3 baseline 2024` = paste0("Q66.", 1:3, "_2024"),
       `UCLA-3 outcome 2025`  = paste0("Q66.", 1:3, "_2025"),
       `K6 baseline 2024`     = paste0("Q65.", 1:6, "_2024"),
       `LSNS-friends 2025`    = lsns25_fr,
       `LSNS-family 2025`     = lsns25_fa)
)
alpha_rows <- list()
for (label in names(alpha_specs)) {
  out <- alpha_for(alpha_specs[[label]], ds)
  flag <- if (!is.finite(out$alpha)) "n/a" else if (out$alpha < 0.60) "low" else if (out$alpha < 0.70) "marginal" else "ok"
  alpha_rows[[length(alpha_rows) + 1L]] <- data.frame(scale = label, n_items = length(alpha_specs[[label]]),
    n_subjects = out$n_eff, raw_alpha = out$alpha, flag = flag, stringsAsFactors = FALSE)
  if (label %in% MED_VARS && flag == "low")      add_flag("reject", label, "α < 0.60 (low)", sprintf("raw α = %.3f / %d subjects", out$alpha, out$n_eff))
  else if (label %in% MED_VARS && flag == "marginal") add_flag("warn", label, "α < 0.70 (marginal)", sprintf("raw α = %.3f / %d subjects", out$alpha, out$n_eff))
}
alpha_df <- do.call(rbind, alpha_rows)
write.csv(alpha_df, file.path(OUT_BASE, "04_cronbach_alpha.csv"), row.names = FALSE, fileEncoding = "UTF-8")
for (i in seq_len(nrow(alpha_df))) {
  a <- alpha_df$raw_alpha[i]
  log_msg(sprintf("  %-22s  items=%2d  n=%5d  α=%s  (%s)", alpha_df$scale[i], alpha_df$n_items[i], alpha_df$n_subjects[i],
                  if (is.finite(a)) sprintf("%.3f", a) else "  n/a", alpha_df$flag[i]))
}

# =============================================================================
# Diagnostic 5 — Mediator subscale structure (2 mediators only)
# =============================================================================
log_msg("\n[5] Mediator subscale structure (W_attach, W_anthrop)")
struct_rows <- list()
for (m in MED_VARS) {
  items <- intersect(MED_SPEC[[m]], names(ds))
  if (length(items) < 3L) { log_msg(sprintf("  %-10s fewer than 3 items present — skip", m)); next }
  mm <- as.matrix(ds[, items, drop = FALSE]); mode(mm) <- "numeric"; mm <- mm[complete.cases(mm), , drop = FALSE]
  if (nrow(mm) < 20L) { log_msg(sprintf("  %-10s n_eff < 20 — skip", m)); next }
  a_obj <- tryCatch(suppressWarnings(suppressMessages(psych::alpha(mm, na.rm = TRUE, warnings = FALSE))), error = function(e) NULL)
  if (is.null(a_obj)) next
  avg_r <- mean(cor(mm, use = "complete.obs")[upper.tri(diag(ncol(mm)))], na.rm = TRUE)
  log_msg(sprintf("  %-10s α=%5.3f  avg inter-item r=%5.3f", m, a_obj$total$raw_alpha, avg_r))
  for (k in seq_along(items)) {
    itr <- a_obj$item.stats$r.drop[k]; rdrop <- a_obj$alpha.drop$raw_alpha[k]
    flag <- if (is.finite(itr) && itr < 0.30) "low_item_total_r" else "ok"
    if (flag == "low_item_total_r") add_flag("warn", sprintf("%s :: %s", m, items[k]), "item-total r < 0.30",
      sprintf("r.drop = %.3f; α-if-deleted = %.3f", itr, rdrop))
    struct_rows[[length(struct_rows) + 1L]] <- data.frame(mediator = m, item = items[k],
      mediator_alpha = a_obj$total$raw_alpha, item_total_r = itr, alpha_if_deleted = rdrop,
      avg_interitem_r = avg_r, n = nrow(mm), flag = flag, stringsAsFactors = FALSE)
  }
}
struct_df <- if (length(struct_rows)) do.call(rbind, struct_rows) else
  data.frame(mediator = character(), item = character(), mediator_alpha = numeric(),
             item_total_r = numeric(), alpha_if_deleted = numeric(), avg_interitem_r = numeric(),
             n = integer(), flag = character(), stringsAsFactors = FALSE)
write.csv(struct_df, file.path(OUT_BASE, "05_mediator_subscale_structure.csv"), row.names = FALSE, fileEncoding = "UTF-8")

# =============================================================================
# Diagnostic 6 — Construct correlation {2 mediators + 3 purposes}
# =============================================================================
log_msg("\n[6] Construct correlation (W_attach, W_anthrop, A_SE, A_PC, A_DI)")
con_vars <- c(MED_VARS, A_cont_vars)
con_mat <- as.matrix(ds[, con_vars, drop = FALSE]); mode(con_mat) <- "numeric"
con_cor <- suppressWarnings(cor(con_mat, use = "pairwise.complete.obs"))
con_long <- as.data.frame(as.table(con_cor), stringsAsFactors = FALSE); names(con_long) <- c("var1", "var2", "r")
con_long <- con_long[con_long$var1 != con_long$var2, , drop = FALSE]
write.csv(con_long, file.path(OUT_BASE, "06_construct_correlation.csv"), row.names = FALSE, fileEncoding = "UTF-8")
r_ma <- con_cor["W_attach", "W_anthrop"]
log_msg(sprintf("  r(W_attach, W_anthrop) = %.3f  (expected ≈ 0.65 — the basis for parallel single-mediator pipelines, codebook §3)",
                r_ma))
if (is.finite(r_ma) && r_ma > 0.80) add_flag("warn", "W_attach ↔ W_anthrop", "mediator r > 0.80",
  sprintf("r = %.3f — strong overlap; parallel single-mediator design (not joint) is the more important given this", r_ma))

# =============================================================================
# Diagnostic 7 — A_purpose × Y descriptive (inter-A + crude A → UCLA)
# =============================================================================
log_msg("\n[7] A_purpose correlation + per-purpose crude A → UCLA-3 OLS")
ay_rows <- list()
for (a in A_cont_vars) {
  sub <- ds[is.finite(ds[[a]]) & is.finite(ds$Y_ucla3), c(a, "Y_ucla3"), drop = FALSE]
  if (nrow(sub) < 20L) { log_msg(sprintf("  crude %s → Y_ucla3 SKIP (n=%d<20)", a, nrow(sub)))
    ay_rows[[length(ay_rows) + 1L]] <- data.frame(purpose = a, beta = NA_real_, se = NA_real_, p = NA_real_, n = nrow(sub), stringsAsFactors = FALSE); next }
  fit <- lm(as.formula(paste0("Y_ucla3 ~ ", a)), data = sub)
  b <- unname(coef(fit)[2]); s <- sqrt(diag(vcov(fit)))[2]; p <- 2 * pnorm(-abs(b / s))
  ay_rows[[length(ay_rows) + 1L]] <- data.frame(purpose = a, beta = b, se = s, p = p, n = nrow(sub), stringsAsFactors = FALSE)
  log_msg(sprintf("  crude %s → Y_ucla3  β=%+.4f (SE=%.4f, p=%.3g, n=%d)  %s", a, b, s, p, nrow(sub), ifelse(b > 0, "lonelier", "less lonely")))
}
A_mat <- as.matrix(ds[, A_cont_vars, drop = FALSE]); mode(A_mat) <- "numeric"
A_cor <- suppressWarnings(cor(A_mat, use = "pairwise.complete.obs"))
A_cor_long <- as.data.frame(as.table(A_cor), stringsAsFactors = FALSE); names(A_cor_long) <- c("var1", "var2", "r")
write.csv(merge(A_cor_long, do.call(rbind, ay_rows), by.x = "var1", by.y = "purpose", all = TRUE),
          file.path(OUT_BASE, "07_A_purpose_correlation.csv"), row.names = FALSE, fileEncoding = "UTF-8")

# =============================================================================
# EFA helper (oblique; parallel analysis → loadings / variance / factor cor)
# =============================================================================
run_efa_block <- function(efa_items, tag, label, n_theory, item_map) {
  efa_items <- intersect(efa_items, names(ds))
  if (length(efa_items) < 3L) { log_msg(sprintf("  [%s] EFA SKIP: <3 items present (%d).", tag, length(efa_items)))
    add_flag("warn", sprintf("EFA %s", tag), "too few items", sprintf("found %d", length(efa_items))); return(invisible(FALSE)) }
  m <- as.matrix(ds[, efa_items, drop = FALSE]); mode(m) <- "numeric"; m <- m[complete.cases(m), , drop = FALSE]
  if (nrow(m) < 100L) { log_msg(sprintf("  [%s] EFA SKIP: n_eff=%d < 100 (expected on dummy; real ≈ 2,490).", tag, nrow(m)))
    add_flag("warn", sprintf("EFA %s", tag), "n_eff < 100", sprintf("only %d complete-case (dummy artefact)", nrow(m))); return(invisible(FALSE)) }
  pa <- tryCatch(suppressWarnings(suppressMessages(psych::fa.parallel(m, fa = "fa", fm = "minres", plot = FALSE, n.iter = 20))), error = function(e) NULL)
  nfact_suggest <- if (!is.null(pa) && is.finite(pa$nfact)) max(1L, as.integer(pa$nfact)) else NA_integer_
  max_fac <- max(1L, length(efa_items) %/% 3L)
  nfact_use <- if (is.na(nfact_suggest)) min(n_theory, max_fac) else min(max(nfact_suggest, 1L), max_fac)
  log_msg(sprintf("  [%s] parallel analysis suggests %s factors; fitting EFA with %d (%s).", tag,
                  ifelse(is.na(nfact_suggest), "NA", as.character(nfact_suggest)), nfact_use, label))
  if (!is.null(pa)) write.csv(data.frame(factor = seq_along(pa$fa.values), eigen_actual = pa$fa.values,
    eigen_resampled = if (!is.null(pa$fa.sim)) pa$fa.sim else NA_real_, nfact_suggested = nfact_suggest, stringsAsFactors = FALSE),
    file.path(OUT_BASE, sprintf("%s_parallel.csv", tag)), row.names = FALSE, fileEncoding = "UTF-8")
  rotate_use <- if (requireNamespace("GPArotation", quietly = TRUE)) "oblimin" else "varimax"
  fit_fa <- tryCatch(suppressWarnings(suppressMessages(psych::fa(m, nfactors = nfact_use, rotate = rotate_use, fm = "minres"))),
                     error = function(e) { log_msg(sprintf("  [%s] fa() failed: %s", tag, conditionMessage(e))); NULL })
  if (is.null(fit_fa)) { log_msg(sprintf("  [%s] EFA produced no fit.", tag)); return(invisible(FALSE)) }

  # ---- sampling-adequacy diagnostics on the same matrix ----
  kmo  <- tryCatch(suppressWarnings(suppressMessages(psych::KMO(m))), error = function(e) NULL)
  bart <- tryCatch(suppressWarnings(suppressMessages(psych::cortest.bartlett(cor(m), n = nrow(m)))), error = function(e) NULL)
  msai <- if (!is.null(kmo) && !is.null(kmo$MSAi)) kmo$MSAi else setNames(rep(NA_real_, ncol(m)), colnames(m))
  h2 <- fit_fa$communality; u2 <- fit_fa$uniquenesses; cmplx <- fit_fa$complexity

  # ---- loadings + per-item communality (h2) / uniqueness (u2) / complexity / MSA ----
  L <- unclass(fit_fa$loadings); Lm <- matrix(as.numeric(L), nrow = nrow(L), dimnames = dimnames(L))
  fac_names <- colnames(Lm)
  load_df <- data.frame(item = rownames(Lm), theoretical_group = item_map[rownames(Lm)], stringsAsFactors = FALSE)
  for (j in seq_len(ncol(Lm))) load_df[[fac_names[j]]] <- round(Lm[, j], 3)
  load_df$top_factor <- fac_names[apply(abs(Lm), 1, which.max)]
  load_df$h2         <- round(unname(h2[rownames(Lm)]), 3)      # communality
  load_df$u2         <- round(unname(u2[rownames(Lm)]), 3)      # uniqueness
  load_df$complexity <- round(unname(cmplx[rownames(Lm)]), 3)   # Hofmann item complexity
  load_df$MSA_item   <- round(unname(msai[rownames(Lm)]), 3)    # per-item KMO
  write.csv(load_df, file.path(OUT_BASE, sprintf("%s_loadings.csv", tag)), row.names = FALSE, fileEncoding = "UTF-8")
  va <- fit_fa$Vaccounted
  cum_row <- if ("Cumulative Var" %in% rownames(va)) "Cumulative Var" else "Proportion Var"
  write.csv(data.frame(factor = colnames(va), SS_loadings = va["SS loadings", ], prop_var = va["Proportion Var", ],
    cum_var = va[cum_row, ], stringsAsFactors = FALSE),
    file.path(OUT_BASE, sprintf("%s_variance.csv", tag)), row.names = FALSE, fileEncoding = "UTF-8")
  phi <- fit_fa$Phi
  if (!is.null(phi) && is.matrix(phi) && nrow(phi) > 1L) {
    phi_m <- matrix(as.numeric(phi), nrow = nrow(phi), dimnames = dimnames(phi))
    write.csv(data.frame(factor = rownames(phi_m), round(phi_m, 3), check.names = FALSE, stringsAsFactors = FALSE),
              file.path(OUT_BASE, sprintf("%s_factor_cor.csv", tag)), row.names = FALSE, fileEncoding = "UTF-8")
    off <- phi_m[upper.tri(phi_m)]
    log_msg(sprintf("  [%s] oblique factor correlations: mean |r|=%.2f, max |r|=%.2f (%s).", tag, mean(abs(off)), max(abs(off)), rotate_use))
  } else log_msg(sprintf("  [%s] no factor-correlation matrix (orthogonal rotation or single factor).", tag))
  cum_var_use <- as.numeric(va[cum_row, ncol(va)])

  # ---- model-fit + adequacy summary (KMO, Bartlett, RMSEA, TLI, RMSR, BIC, chi-square) ----
  getf <- function(x, i = 1L) if (!is.null(x) && length(x) >= i && is.finite(x[i])) as.numeric(x[i]) else NA_real_
  fit_kv <- data.frame(
    metric = c("n_obs", "n_items", "n_factors", "KMO_overall",
               "bartlett_chisq", "bartlett_df", "bartlett_p",
               "model_chisq", "model_chisq_df", "model_chisq_p",
               "RMSEA", "RMSEA_lower", "RMSEA_upper", "TLI", "RMSR", "BIC",
               "cum_var_pct", "fit_offdiag"),
    value = c(nrow(m), ncol(m), nfact_use,
              if (!is.null(kmo)) round(getf(kmo$MSA), 3) else NA_real_,
              if (!is.null(bart)) round(getf(bart$chisq), 2) else NA_real_,
              if (!is.null(bart)) getf(bart$df) else NA_real_,
              if (!is.null(bart)) signif(getf(bart$p.value), 3) else NA_real_,
              round(getf(fit_fa$STATISTIC), 2), getf(fit_fa$dof), signif(getf(fit_fa$PVAL), 3),
              round(getf(fit_fa$RMSEA, 1L), 3), round(getf(fit_fa$RMSEA, 2L), 3), round(getf(fit_fa$RMSEA, 3L), 3),
              round(getf(fit_fa$TLI), 3), round(getf(fit_fa$rms), 3), round(getf(fit_fa$BIC), 1),
              round(100 * cum_var_use, 1), round(getf(fit_fa$fit.off), 3)),
    stringsAsFactors = FALSE)
  write.csv(fit_kv, file.path(OUT_BASE, sprintf("%s_fit.csv", tag)), row.names = FALSE, fileEncoding = "UTF-8")

  log_msg(sprintf("  [%s] EFA (%d factors, %s): cum var=%.1f%%; KMO=%s; Bartlett p=%s; RMSEA=%.3f; TLI=%.3f; RMSR=%.3f; items->%d top-factors (vs %d groups).",
                  tag, nfact_use, rotate_use, 100 * cum_var_use,
                  if (!is.null(kmo)) sprintf("%.2f", getf(kmo$MSA)) else "NA",
                  if (!is.null(bart)) signif(getf(bart$p.value), 3) else "NA",
                  getf(fit_fa$RMSEA, 1L), getf(fit_fa$TLI), getf(fit_fa$rms),
                  length(unique(load_df$top_factor)), n_theory))

  # ---- flags (Kaiser/Bartlett conventions) ----
  if (!is.null(kmo) && is.finite(getf(kmo$MSA)) && getf(kmo$MSA) < 0.60)
    add_flag("warn", sprintf("EFA %s KMO", tag), "KMO < 0.60 (mediocre sampling adequacy)", sprintf("KMO = %.2f", getf(kmo$MSA)))
  if (!is.null(bart) && is.finite(getf(bart$p.value)) && getf(bart$p.value) >= 0.05)
    add_flag("warn", sprintf("EFA %s Bartlett", tag), "Bartlett p ≥ 0.05 (sphericity not rejected — items may not be factorable)", sprintf("p = %.3g", getf(bart$p.value)))
  .lowh2 <- names(h2)[is.finite(h2) & h2 < 0.30]
  if (length(.lowh2))
    add_flag("warn", sprintf("EFA %s communalities", tag), "item communality h2 < 0.30",
             paste(sprintf("%s h2=%.2f", .lowh2, h2[.lowh2]), collapse = "; "))
  invisible(TRUE)
}

# =============================================================================
# Diagnostic 8 — EFA on the 3 AI-use purposes (9 Q37S3 items)
# =============================================================================
log_msg("\n[8] EFA on the 9 AI-use purpose items (Q37S3): A_SE / A_PC / A_DI")
A_ITEM_MAP <- setNames(rep(names(A_SPEC), lengths(A_SPEC)), unlist(A_SPEC, use.names = FALSE))
run_efa_block(unlist(A_SPEC, use.names = FALSE), "08_efa_purposes", "3 AI-use purposes (9 items)",
              n_theory = length(A_SPEC), item_map = A_ITEM_MAP)

# =============================================================================
# Diagnostic 9 — EFA on the 2 mediators (6 items) — factor cor = the ≈0.65 check
# =============================================================================
log_msg("\n[9] EFA on the 6 mediator items: W_attach (Q40.22-24) + W_anthrop (Q40.1-3)")
MED_ITEM_MAP <- setNames(rep(names(MED_SPEC), lengths(MED_SPEC)), unlist(MED_SPEC, use.names = FALSE))
run_efa_block(unlist(MED_SPEC, use.names = FALSE), "09_efa_mediators", "2 mediators (6 items)",
              n_theory = length(MED_VARS), item_map = MED_ITEM_MAP)

# =============================================================================
# Diagnostic 10 — Straight-line responding on the 6 mediator items
# =============================================================================
log_msg("\n[10] Straight-line responding on the 6 mediator items")
sl_items <- intersect(unlist(MED_SPEC, use.names = FALSE), names(ds))
sl_df <- NULL
if (length(sl_items) >= 2L) {
  m <- as.matrix(ds[, sl_items, drop = FALSE]); mode(m) <- "numeric"; m <- m[complete.cases(m), , drop = FALSE]
  if (nrow(m) >= 1L) {
    is_flat <- apply(m, 1L, function(r) length(unique(r)) == 1L); n_flat <- sum(is_flat); pct <- 100 * n_flat / nrow(m)
    vals <- if (n_flat > 0L) m[is_flat, 1L] else numeric(0)
    val_tab <- if (n_flat > 0L) paste(sprintf("%s:%d", names(table(vals)), as.integer(table(vals))), collapse = ", ") else "none"
    log_msg(sprintf("  %d/%d complete-case (%.1f%%) flat across all %d mediator items; by value: %s", n_flat, nrow(m), pct, length(sl_items), val_tab))
    if (pct > 5) add_flag("warn", "straight-lining mediators", "identical-response > 5%",
      sprintf("%d/%d (%.1f%%) flat across the 6 mediator items; consider drop-straight-liners sensitivity", n_flat, nrow(m), pct))
    sl_df <- data.frame(item_set = "mediators_6items", n_items = length(sl_items), n_complete = nrow(m),
                        n_straightline = n_flat, pct_straightline = round(pct, 2), by_value = val_tab, stringsAsFactors = FALSE)
  } else log_msg("  SKIP: 0 complete-case (dummy artefact).")
} else log_msg("  SKIP: <2 mediator items present.")
if (!is.null(sl_df)) write.csv(sl_df, file.path(OUT_BASE, "10_straightlining_mediators.csv"), row.names = FALSE, fileEncoding = "UTF-8")

# =============================================================================
# Diagnostic 11 — Mediation analytic readiness
# =============================================================================
log_msg("\n[11] Mediation analytic readiness")
if (have_lsns25) {
  log_msg(sprintf("  Q20.1-6_2025 present; Y_lsns_friends_2025 (mean=%.2f sd=%.2f), Y_lsns_family_2025 (mean=%.2f sd=%.2f)",
                  mean(ds$Y_lsns_friends_2025, na.rm = TRUE), sd(ds$Y_lsns_friends_2025, na.rm = TRUE),
                  mean(ds$Y_lsns_family_2025,  na.rm = TRUE), sd(ds$Y_lsns_family_2025,  na.rm = TRUE)))
} else {
  log_msg("  WARNING: Q20.1-6_2025 missing — LSNS-2025 outcomes cannot be built")
  add_flag("reject", "LSNS-2025 outcomes", "Q20.1-6_2025 absent", "cannot build Y_lsns_friends/family_2025")
}
cca <- function(cols) sum(complete.cases(ds[, intersect(cols, names(ds)), drop = FALSE]))
n_attach_ucla <- cca(c("Y_ucla3", A_cont_vars, "W_attach", C_VARS))
n_attach_fr   <- if (have_lsns25) cca(c("Y_lsns_friends_2025", A_cont_vars, "W_attach", C_VARS)) else NA_integer_
n_attach_fa   <- if (have_lsns25) cca(c("Y_lsns_family_2025",  A_cont_vars, "W_attach", C_VARS)) else NA_integer_
n_anthrop     <- if (have_lsns25) cca(c("Y_ucla3", "Y_lsns_friends_2025", "Y_lsns_family_2025", A_cont_vars, "W_anthrop", C_VARS)) else NA_integer_
log_msg(sprintf("  CCA W_attach | UCLA n=%d %s", n_attach_ucla, if (n_attach_ucla >= 2000L) "OK" else "FAIL"))
log_msg(sprintf("  CCA W_attach | LSNS-friends n=%s | LSNS-family n=%s | W_anthrop(3oc joint) n=%s",
                ifelse(is.na(n_attach_fr), "—", as.character(n_attach_fr)),
                ifelse(is.na(n_attach_fa), "—", as.character(n_attach_fa)),
                ifelse(is.na(n_anthrop),   "—", as.character(n_anthrop))))
if (RUN_MODE == "real" && n_attach_ucla < 2000L) add_flag("reject", "W_attach UCLA CCA", "n < 2000 (stopifnot in the scripts)", sprintf("n=%d", n_attach_ucla))

# A_SE cat4 cells vs locked
xv <- ds$A_SE_continuous; xv <- xv[!is.na(xv)]; pos <- xv > 0
qq <- if (any(pos)) quantile(xv[pos], c(1/3, 2/3), names = FALSE) else c(NA_real_, NA_real_)
cat4 <- rep(NA_character_, length(xv)); cat4[xv == 0] <- "None"
if (any(pos)) { br <- unique(c(-Inf, qq, Inf)); lab <- c("Low", "Mid", "High")[seq_len(length(br) - 1L)]
  cat4[pos] <- as.character(cut(xv[pos], breaks = br, labels = lab, include.lowest = TRUE)) }
cat4 <- factor(cat4, levels = intersect(c("None", "Low", "Mid", "High"), unique(cat4)))
cells <- table(cat4); frac0 <- mean(xv == 0)
log_msg(sprintf("  A_SE cat4 cells: %s (user cuts %.3f, %.3f; frac_zero=%.3f, expected ≈ 0.59)",
                paste(sprintf("%s=%d", names(cells), as.integer(cells)), collapse = " "), qq[1], qq[2], frac0))
if (RUN_MODE == "real") {
  expected_cells <- c(None = 1470L, Low = 414L, Mid = 334L, High = 272L)
  if (all(names(expected_cells) %in% names(cells))) {
    diffs <- abs(as.integer(cells[names(expected_cells)]) - expected_cells)
    if (any(diffs > 5L)) add_flag("warn", "A_SE cat4 cells", "differ from locked by > 5", paste(sprintf("%s Δ%d", names(expected_cells), diffs), collapse = " "))
    else log_msg("  cat4 cells match locked within tolerance (±5).")
  } else add_flag("warn", "A_SE cat4 cells", "degenerate (<4 levels)", sprintf("levels: %s", paste(names(cells), collapse = ", ")))
  if (abs(frac0 - 0.59) > 0.05) add_flag("warn", "A_SE frac_zero", "deviates > 0.05 from ~0.59", sprintf("frac_zero=%.3f", frac0))
}

# α + r on the mediation cohort
a_attach  <- alpha_for(MED_SPEC$W_attach,  ds)$alpha
a_anthrop <- alpha_for(MED_SPEC$W_anthrop, ds)$alpha
r_sa <- tryCatch(cor(ds$W_attach, ds$W_anthrop, use = "complete.obs"), error = function(e) NA_real_)
log_msg(sprintf("  W_attach α=%.3f (≈0.87), W_anthrop α=%.3f (≈0.89), r=%.3f (≈0.65)", a_attach, a_anthrop, r_sa))
if (is.finite(a_attach)  && a_attach  < 0.70) add_flag("warn", "W_attach α",  "below 0.70", sprintf("α=%.3f", a_attach))
if (is.finite(a_anthrop) && a_anthrop < 0.70) add_flag("warn", "W_anthrop α", "below 0.70", sprintf("α=%.3f", a_anthrop))

write.csv(data.frame(
  metric = c("n_cohort_initiators", "n_cc_W_attach_UCLA", "n_cc_W_attach_LSNS_friends", "n_cc_W_attach_LSNS_family",
             "n_cc_W_anthrop_3outcomes_joint", "ase_cat4_None", "ase_cat4_Low", "ase_cat4_Mid", "ase_cat4_High",
             "ase_frac_zero", "ase_user_t1_cut", "ase_user_t2_cut", "w_attach_alpha", "w_anthrop_alpha", "cor_attach_anthrop"),
  value = c(n_cohort, n_attach_ucla, n_attach_fr, n_attach_fa, n_anthrop,
            as.integer(cells["None"]), as.integer(cells["Low"]), as.integer(cells["Mid"]), as.integer(cells["High"]),
            frac0, qq[1], qq[2], a_attach, a_anthrop, r_sa), stringsAsFactors = FALSE),
  file.path(OUT_BASE, "11_mediation_readiness.csv"), row.names = FALSE, fileEncoding = "UTF-8")
log_msg("  wrote 11_mediation_readiness.csv")

# =============================================================================
# Diagnostic 12 — Centered-VIF on the mediation outcome model (collinearity)
#   VIF of the mean-centered outcome-model predictors {A_SE_c, A_PC_c, A_DI_c,
#   W_c, the three A_*_c × W_c interactions, 44C}. VIF depends only on the
#   predictor matrix, so it is outcome-independent (Y_ucla3 is a nominal response).
#   Mean-centering removes the artificial interaction-term inflation (uncentered
#   A:W VIFs are large by construction, not by real collinearity). One table per
#   mediator (W_attach, W_anthrop); flag any term VIF > 5.
# =============================================================================
log_msg("\n[12] Centered-VIF on the mediation outcome model (per mediator)")
.safe_vif <- function(fit, data) tryCatch({
  al <- alias(fit)$Complete
  if (!is.null(al) && nrow(al) > 0L) {
    keep <- setdiff(attr(terms(fit), "term.labels"), rownames(al))
    lhs  <- as.character(formula(fit))[2]
    fit  <- lm(as.formula(paste0(lhs, " ~ ", paste(keep, collapse = " + "))), data = data)
  }
  suppressWarnings(car::vif(fit))   # term-wise VIF (the "higher-order terms" note is expected)
}, error = function(e) { log_msg(sprintf("  vif() failed: %s", conditionMessage(e))); NULL })

if (!requireNamespace("car", quietly = TRUE)) {
  log_msg("  car package not available — VIF skipped.")
} else {
  vif_rows <- list()
  A_c_vars <- sub("_continuous$", "_c", A_cont_vars)   # A_SE_c, A_PC_c, A_DI_c
  for (m in MED_VARS) {
    Wc  <- paste0(m, "_c")
    req <- unique(c("Y_ucla3", A_cont_vars, m, C_VARS))
    d   <- ds[complete.cases(ds[, intersect(req, names(ds)), drop = FALSE]), , drop = FALSE]
    if (nrow(d) < 50L) { log_msg(sprintf("  [%s] SKIP: n_eff=%d < 50 (expected on dummy).", m, nrow(d))); next }
    for (a in A_cont_vars) d[[sub("_continuous$", "_c", a)]] <- d[[a]] - mean(d[[a]])
    d[[Wc]] <- d[[m]] - mean(d[[m]])
    ints <- paste0(A_c_vars, ":", Wc)
    cv   <- C_VARS[vapply(C_VARS, function(v) length(unique(d[[v]])) > 1L, logical(1))]   # drop constant covs (avoid aliasing)
    rhs  <- paste(c(A_c_vars, Wc, ints, cv), collapse = " + ")
    fit  <- lm(as.formula(paste0("Y_ucla3 ~ ", rhs)), data = d)
    v    <- .safe_vif(fit, d)
    if (is.null(v)) next
    vv <- if (is.matrix(v)) v[, 1] else v                # GVIF column if factors present (none here)
    for (nm in names(vv)) vif_rows[[length(vif_rows) + 1L]] <- data.frame(
      mediator = m, term = nm, vif = round(as.numeric(vv[nm]), 3),
      flag = ifelse(vv[nm] > 10, "severe", ifelse(vv[nm] > 5, "mild", "ok")),
      n = nrow(d), stringsAsFactors = FALSE)
    for (it in ints) if (it %in% names(vv)) log_msg(sprintf("  [%s] %-22s VIF = %.2f", m, it, vv[it]))
    bad <- names(vv)[is.finite(vv) & vv > 5]
    if (length(bad)) add_flag("warn", sprintf("VIF %s", m), "term VIF > 5",
                              paste(sprintf("%s=%.2f", bad, vv[bad]), collapse = "; "))
  }
  if (length(vif_rows)) {
    write.csv(do.call(rbind, vif_rows), file.path(OUT_BASE, "12_vif_outcome_model.csv"), row.names = FALSE, fileEncoding = "UTF-8")
    log_msg("  wrote 12_vif_outcome_model.csv")
  }
}

# =============================================================================
# Flags markdown + sessionInfo
# =============================================================================
write_flags_md <- function(path, rejected, warned) {
  lines <- c(sprintf("# Flags — diagnosis_mediation run %s", .timestamp), "",
             sprintf("Total: %d rejected, %d warned.%s", length(rejected), length(warned),
                     if (length(rejected) + length(warned) == 0L) " All clear." else ""), "")
  if (length(rejected)) { lines <- c(lines, "## Rejected", "", "| Item | Reason | Detail |", "|---|---|---|")
    for (r in rejected) lines <- c(lines, sprintf("| %s | %s | %s |", r$item, r$rule, r$detail)); lines <- c(lines, "") }
  if (length(warned)) { lines <- c(lines, "## Warned", "", "| Item | Reason | Detail |", "|---|---|---|")
    for (w in warned) lines <- c(lines, sprintf("| %s | %s | %s |", w$item, w$rule, w$detail)); lines <- c(lines, "") }
  lines <- c(lines, "## What to do", "",
             sprintf("- Open `%s` for the verbose log.", basename(LOG_FILE)),
             "- `04_cronbach_alpha.csv` + `05_mediator_subscale_structure.csv` — mediator reliability/item structure.",
             "- `06_construct_correlation.csv` — r(W_attach, W_anthrop) ≈ 0.65 is the basis for parallel single-mediator pipelines.",
             "- `08_efa_purposes_*` + `09_efa_mediators_*` — measurement structure of the used items; `09_efa_mediators_factor_cor.csv` is the 2-factor (attach vs anthrop) oblique correlation.",
             "- `11_mediation_readiness.csv` — LSNS-2025 construction, per-outcome CCA n, A_SE cat4 cells, α + r. All must look as expected before locking R=1000.", "")
  writeLines(lines, path)
}
write_flags_md(FLAGS_FILE, .flags_rejected, .flags_warned)
log_msg(sprintf("\nFlags written: %d rejected, %d warned -> %s", length(.flags_rejected), length(.flags_warned), FLAGS_FILE))
writeLines(capture.output(sessionInfo()), file.path(OUT_BASE, sprintf("sessionInfo_%s.txt", .timestamp)))
log_msg("=== diagnosis_mediation.R END ===")
