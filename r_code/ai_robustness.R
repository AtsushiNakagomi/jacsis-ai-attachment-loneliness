# ============================================================
# ai_robustness.R
# Robustness of the six primary cells (2 mediators × 3 outcomes; social/emotional use focal)
# to sample definition, attrition, and the composition of the attachment composite.
# ============================================================
# Specifications, numbered as in the manuscript (Section 3.3, "Sensitivity and robustness analyses",
# where S1–S5 are the sensitivity analyses of the outcome scripts: S1 tertile mediator (§2), S2 binary
# exposure (§3), S3 continuous exposure (§4), S4 interventional analogue (§6), S5 per-item mediator (§5)).
# Each specification below re-estimates all six cells; S6 re-estimates the three attachment cells:
#   REF reference — the primary specification (full analytic cohort, unweighted), re-estimated with
#       the closed-form engine used below (should reproduce Table 3 / Table 2 to the bootstrap noise)
#   S6  attachment composite — two-item composite (Q40.22 + Q40.24), omitting Q40.23
#       ("more comfortable talking to generative AI than to real people")
#   S7  baseline timing — exclude respondents who completed the 2024 baseline in January 2025
#       (Asia/Tokyo calendar month of the completion timestamp); the 2024 wave ran Dec 2024–Jan 2025
#       and Q37S1_2025 = 5 means "started Jan 2025 or later", so only January-2025 baseline
#       responders could in principle have initiated before baseline
#   S8  baseline timing — restrict to July–December 2025 initiators (Q37S1_2025 == 6), whose
#       initiation necessarily post-dates the baseline
#   S9  attrition — stabilized inverse-probability-of-response weights (2024 → 2025 follow-up),
#       estimated on the full 2024 respondent file and applied to the mediator and outcome models
#
# Estimator: closed-form linear natural-effects engine (linear mediator model; linear outcome model
# with exposure × mediator interaction; mediator mean-centered; CDE at the mediator mean). For a
# categorical exposure with reference None and level L, with mediator model E[M|A,C] = b0 + bL·1(A=L)
# + b'C and outcome model E[Y|A,M,C] = t0 + t1L·1(A=L) + t2·M + t3L·1(A=L)·M + t'C:
#   TNIE  = (t2 + t3L)·bL        PNIE = t2·bL        INT_med = t3L·bL
#   CDE   = t1L                  PNDE = t1L + t3L·m0  INT_ref = t3L·m0      TE = PNDE + TNIE
# where m0 is the mean predicted mediator under A = None over the analytic covariate distribution.
# These are the quantities returned by the CMAverse regression-based estimator for linear models
# (the §4 continuous sections of the outcome scripts verify the agreement to floating-point precision).
# Inference: bootstrap percentile CIs (R = 1,000 real / 100 dummy; 8 workers; seed 20260524);
# one joint Wald χ²(3) test per cell on the TNIE vector (Low, Mid, High) with bootstrap covariance,
# as in the primary analysis. Weights (S9) are held fixed across bootstrap replicates.
# User-tertile cut-points for Low/Mid/High are those of the full analytic cohort in every
# specification, so the contrasts keep the same meaning across rows.
#
# Inputs : the two-wave analytic CSV (as the outcome scripts) and, for S9, the full 2024 respondent
#          file (data/jacsis_2024_all.csv, or env JACSIS_2024_ALL_PATH) with either a 0/1 column
#          `followed_2025` or an ID column shared with the two-wave file (ID_VAR below; Monitor_ID).
# Outputs: output/ai_mod/[dummy/]robustness/tables/
#   robustness_cells.csv    long: spec × mediator × outcome × contrast × estimand
#   robustness_joint.csv    spec × mediator × outcome: n, Wald χ², df, p
#   robustness_specs.csv    n per specification + notes
#   timing_baseline_month.csv   baseline completion month × initiation half-year (Q37S1 = 5 / 6)
#   ipw_attrition_model.csv / ipw_weights_summary.csv
# LLM never reads real data. Aggregated CSVs / logs are OK to read.
# ============================================================

suppressPackageStartupMessages({ library(here); library(parallel); library(readr) })
set.seed(20260524)

# ---- Constants ----
N_WORKERS <- 8L
TS_VAR    <- "回答完了日時_2024"   # 回答完了日時_2024 : baseline completion timestamp (UTC, ISO 8601)
TZ_LOCAL  <- "Asia/Tokyo"
ID_VAR    <- "Monitor_ID"                                   # respondent ID shared by the two-wave file and the full 2024 file (used to derive the follow-up flag)
FOLLOW_VAR <- "followed_2025"
IPW_TRUNC <- c(0.01, 0.99)                                  # percentile truncation of stabilized weights

# ---- Path / mode detection (as in the outcome scripts) ----
find_proj_root <- function() {
  candidates <- character(0); ch <- tryCatch(here::here(), error = function(e) NA_character_); if (!is.na(ch)) candidates <- c(candidates, ch)
  args <- commandArgs(trailingOnly = FALSE); fa <- args[grepl("^--file=", args)]
  if (length(fa)) { sd <- tryCatch(normalizePath(dirname(sub("^--file=", "", fa[1])), winslash = "/"), error = function(e) NA_character_); if (!is.na(sd)) candidates <- c(candidates, sd, dirname(sd)) }
  candidates <- c(candidates, getwd(), dirname(getwd()))
  for (cand in unique(candidates)) {
    if (!nzchar(cand)) next
    if (basename(cand) == "r_code" && dir.exists(file.path(cand, "data"))) return(normalizePath(cand, winslash = "/"))
    if (dir.exists(file.path(cand, "r_code", "data"))) return(normalizePath(file.path(cand, "r_code"), winslash = "/"))
  }
  stop("Could not find r_code/")
}
.proj_root <- find_proj_root(); stopifnot(dir.exists(file.path(.proj_root, "data")))
.norm <- function(p) normalizePath(p, winslash = "/", mustWork = FALSE)
DUMMY_DATA_PATH <- .norm(file.path(.proj_root, "data", "dummy_24_25.csv"))
REAL_DATA_PATH  <- Sys.getenv("JACSIS_2WAVE_2425_PATH", unset = "")
if (!nzchar(REAL_DATA_PATH)) REAL_DATA_PATH <- file.path(.proj_root, "data", "jacsis_2wave2425.csv")
REAL_DATA_PATH  <- .norm(REAL_DATA_PATH)
RUN_MODE <- {
  mode_env <- toupper(Sys.getenv("AI_MOD_MODE"))
  if (mode_env == "REAL")             "real"
  else if (mode_env == "DUMMY")       "dummy"
  else if (file.exists(REAL_DATA_PATH))  "real"
  else if (file.exists(DUMMY_DATA_PATH)) "dummy"
  else stop("No input CSV found (set JACSIS_2WAVE_2425_PATH or place dummy_24_25.csv).")
}
DATA_PATH <- if (RUN_MODE == "real") REAL_DATA_PATH else DUMMY_DATA_PATH
ALL2024_PATH <- Sys.getenv("JACSIS_2024_ALL_PATH", unset = "")
if (!nzchar(ALL2024_PATH)) ALL2024_PATH <- file.path(.proj_root, "data", "jacsis_2024_all.csv")
ALL2024_PATH <- .norm(ALL2024_PATH)

# ---- Output tree ----
OUT_ROOT <- file.path(.proj_root, "output", "ai_mod", if (RUN_MODE == "real") "robustness" else file.path("dummy", "robustness"))
TABLES_DIR <- file.path(OUT_ROOT, "tables"); LOGS_DIR <- file.path(OUT_ROOT, "logs")
for (d in c(TABLES_DIR, LOGS_DIR)) dir.create(d, recursive = TRUE, showWarnings = FALSE)
.timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
LOG_FILE   <- file.path(LOGS_DIR, sprintf("run_robustness_%s_%s.log", RUN_MODE, .timestamp))
log_msg     <- function(...) { m <- paste0(format(Sys.time(), "[%H:%M:%S] "), paste(..., collapse = " ")); cat(m, "\n", sep = ""); cat(m, "\n", sep = "", file = LOG_FILE, append = TRUE); invisible(m) }
write_table <- function(x, name) { fp <- file.path(TABLES_DIR, paste0(name, ".csv")); utils::write.csv(x, fp, row.names = FALSE, fileEncoding = "UTF-8"); log_msg("wrote:", fp) }
log_msg(sprintf("=== START === robustness mode=%s input=%s", RUN_MODE, DATA_PATH))

# ============================================================
# §0  Build cohort (identical recodes to the outcome scripts; both mediators, all three outcomes)
# ============================================================
.as_num     <- function(x) suppressWarnings(as.numeric(x))
.row_mean   <- function(d, cols, na_rm = FALSE) { cols <- intersect(cols, names(d)); if (!length(cols)) return(rep(NA_real_, nrow(d))); rowMeans(vapply(d[cols], .as_num, numeric(nrow(d))), na.rm = na_rm) }
.row_sum_fn <- function(d, cols, fn = identity, na_rm = FALSE) { cols <- intersect(cols, names(d)); if (!length(cols)) return(rep(NA_real_, nrow(d))); rowSums(vapply(d[cols], function(v) fn(.as_num(v)), numeric(nrow(d))), na.rm = na_rm) }
.ucla_rec   <- function(x) pmax(0, pmin(3, 4 - x))
.k6_rec     <- function(x) pmax(0, pmin(4, 5 - x))
.lsns_rec   <- function(x) pmax(0, pmin(5, x - 1L))
.timeuse    <- function(d, raw, p) { v <- .as_num(d[[raw]])
  d[[paste0(p, "_band_1_2")]]   <- as.integer(v %in% c(4L, 5L))
  d[[paste0(p, "_band_3_4")]]   <- as.integer(v %in% c(6L, 7L))
  d[[paste0(p, "_band_5plus")]] <- as.integer(v %in% c(8:11))
  d[[paste0(p, "_unknown")]]    <- as.integer(is.na(v) | v == 12L)
  d
}
.tipi_pair <- function(d, fwd, rev) { a <- .as_num(d[[fwd]]); b <- 8 - .as_num(d[[rev]]); (a + b) / 2 }
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

# Baseline (2024) covariate builder — shared by the two-wave file and the full 2024 file
build_baseline_covs <- function(df) {
  .safe <- function(cn) if (cn %in% names(df)) .as_num(df[[cn]]) else rep(NA_real_, nrow(df))
  df$baseline_ucla3 <- .row_sum_fn(df, paste0("Q66.", 1:3, "_2024"), fn = .ucla_rec)
  df$baseline_k6    <- .row_sum_fn(df, paste0("Q65.", 1:6, "_2024"), fn = .k6_rec)
  ace_cols <- intersect(paste0("Q77.", c(1:8, 13), "_2024"), names(df))
  if (length(ace_cols)) {
    ap  <- vapply(df[ace_cols], function(v) as.integer(.as_num(v) == 1L), integer(nrow(df)))
    aps <- rowSums(ap, na.rm = TRUE)
    if ("Q77.9_2024" %in% names(df)) { q9 <- .as_num(df$Q77.9_2024); a9 <- as.integer(q9 == 2L); a9[is.na(q9)] <- 0L } else a9 <- 0L
    df$ace_score <- aps + a9
  } else df$ace_score <- NA_real_
  df$ace_1     <- as.integer(df$ace_score == 1L)
  df$ace_2_3   <- as.integer(df$ace_score %in% 2:3)
  df$ace_4plus <- as.integer(df$ace_score >= 4L)
  df$age_2024  <- .safe("AGE_2024")
  df$sex_female <- as.integer(.safe("SEX_2024") == 2L)
  edu <- .safe("Q21.1_2024"); df$edu_univ <- as.integer(edu %in% 6:8); df$edu_grad <- as.integer(edu == 9L)
  emp <- .safe("Q5.1_2024")
  df$emp_exec    <- as.integer(emp == 1L)
  df$emp_self    <- as.integer(emp %in% 2:4)
  df$emp_nonreg  <- as.integer(emp %in% 7:11)
  df$emp_student <- as.integer(emp %in% 12:13)
  df$emp_notwork <- as.integer(emp %in% 14:16 | is.na(emp))
  inc <- .safe("Q80.1_2024")
  df$income_2_6m     <- as.integer(inc %in% 5:8)
  df$income_6_10m    <- as.integer(inc %in% 9:12)
  df$income_10m_plus <- as.integer(inc %in% 13:18)
  df$income_unknown  <- as.integer(is.na(inc) | inc %in% c(19L, 20L))
  df$married      <- as.integer(.safe("Q2_2024") %in% 1:3)
  liv <- .safe("Q1.1_2024"); df$living_alone <- as.integer(!is.na(liv) & liv == 1L)
  df$baseline_lsns6_family  <- .row_sum_fn(df, paste0("Q17.", 1:3, "_2024"), fn = .lsns_rec)
  df$baseline_lsns6_friends <- .row_sum_fn(df, paste0("Q17.", 4:6, "_2024"), fn = .lsns_rec)
  df$mental_physical_health <- .row_mean(df, c("Q76.3_2024", "Q76.4_2024"))
  df <- .timeuse(df, "Q28.13_2024", "smartphone"); df <- .timeuse(df, "Q28.14_2024", "pc_tablet")
  df <- .timeuse(df, "Q28.5_2024",  "sitting");    df <- .timeuse(df, "Q28.6_2024",  "walking")
  df$big5_extraversion     <- .tipi_pair(df, "Q79.1_2024", "Q79.6_2024")
  df$big5_agreeableness    <- .tipi_pair(df, "Q79.7_2024", "Q79.2_2024")
  df$big5_conscientiousness <- .tipi_pair(df, "Q79.3_2024", "Q79.8_2024")
  df$big5_neuroticism      <- .tipi_pair(df, "Q79.4_2024", "Q79.9_2024")
  df$big5_openness         <- .tipi_pair(df, "Q79.5_2024", "Q79.10_2024")
  df
}

C_VARS <- c(
  # demographic / SES (15)
  "age_2024", "sex_female", "edu_univ", "edu_grad",
  "emp_exec", "emp_self", "emp_nonreg", "emp_student", "emp_notwork",
  "income_2_6m", "income_6_10m", "income_10m_plus", "income_unknown",
  "married", "living_alone",
  # psychological / social-network (8)
  "baseline_lsns6_family", "baseline_lsns6_friends", "baseline_ucla3", "baseline_k6",
  "ace_1", "ace_2_3", "ace_4plus", "mental_physical_health",
  # time-use dummies (16)
  "smartphone_band_1_2", "smartphone_band_3_4", "smartphone_band_5plus", "smartphone_unknown",
  "pc_tablet_band_1_2", "pc_tablet_band_3_4", "pc_tablet_band_5plus", "pc_tablet_unknown",
  "sitting_band_1_2", "sitting_band_3_4", "sitting_band_5plus", "sitting_unknown",
  "walking_band_1_2", "walking_band_3_4", "walking_band_5plus", "walking_unknown",
  # Big Five personality, TIPI-J (5)
  "big5_extraversion", "big5_agreeableness", "big5_conscientiousness", "big5_neuroticism", "big5_openness"
)
stopifnot(length(C_VARS) == 44L)

A_SPEC <- list(A_SE = paste0("Q37S3.", c(8, 9), "_2025"),
               A_PC = paste0("Q37S3.", c(1, 2, 4, 5), "_2025"),
               A_DI = paste0("Q37S3.", c(3, 6, 7), "_2025"))
A_VARS <- names(A_SPEC)
MED_SPEC <- list(W_attach  = paste0("Q40.", 22:24, "_2025"),
                 W_anthrop = paste0("Q40.", 1:3, "_2025"),
                 W_attach2 = paste0("Q40.", c(22, 24), "_2025"))   # two-item composite (S6)
OUTCOMES <- c(Y_ucla3 = "loneliness", Y_lsns_friends_2025 = "lsns_friends", Y_lsns_family_2025 = "lsns_family")

df <- readr::read_csv(DATA_PATH, show_col_types = FALSE)
log_msg(sprintf("Loaded: %d rows x %d cols", nrow(df), ncol(df)))
df <- build_baseline_covs(df)
df$Y_ucla3             <- .row_sum_fn(df, paste0("Q66.", 1:3, "_2025"), fn = .ucla_rec)
df$Y_lsns_friends_2025 <- .row_sum_fn(df, paste0("Q20.", 4:6, "_2025"), fn = .lsns_rec)
df$Y_lsns_family_2025  <- .row_sum_fn(df, paste0("Q20.", 1:3, "_2025"), fn = .lsns_rec)
for (a in A_VARS) df[[paste0(a, "_continuous")]] <- .row_mean(df, A_SPEC[[a]], na_rm = FALSE) - 1
for (m in names(MED_SPEC)) df[[m]] <- .row_mean(df, MED_SPEC[[m]], na_rm = FALSE)
ai_start <- if ("Q37S1_2025" %in% names(df)) .as_num(df$Q37S1_2025) else rep(NA_real_, nrow(df)); df$ai_start <- ai_start
ds <- df[ai_start %in% c(5L, 6L), , drop = FALSE]
log_msg(sprintf("Cohort (2025 initiators, Q37S1 in {5, 6}): n = %d", nrow(ds)))

required <- unique(c(names(OUTCOMES), paste0(A_VARS, "_continuous"), "W_attach", "W_anthrop", C_VARS))
dm <- ds[complete.cases(ds[, required, drop = FALSE]), , drop = FALSE]
log_msg(sprintf("Analytic n (complete-case on all outcomes, exposures, mediators, %d covariates): %d", length(C_VARS), nrow(dm)))
if (RUN_MODE == "real") stopifnot(nrow(dm) >= 2000L)

# Centering and factors on the FULL analytic cohort (tertile cut-points fixed here)
for (a in A_VARS) dm[[paste0(a, "_c")]] <- dm[[paste0(a, "_continuous")]] - mean(dm[[paste0(a, "_continuous")]])
for (m in names(MED_SPEC)) dm[[paste0(m, "_c")]] <- dm[[m]] - mean(dm[[m]])
for (a in A_VARS) dm[[paste0(a, "_cat4")]] <- make_cat4(dm[[paste0(a, "_continuous")]])
dm$.w <- 1
.run_ok <- nrow(dm) >= 50L && sd(dm$A_SE_c) > 0 && sd(dm$W_attach_c) > 0
if (!.run_ok) log_msg(sprintf("GUARD: degenerate sample (n=%d). Skipping estimation.", nrow(dm)))
R_BOOT <- if (RUN_MODE == "dummy") 100L else 1000L
CV_ASE <- c(C_VARS, "A_PC_c", "A_DI_c")   # A_SE focal → the two non-focal purposes are covariates

# ============================================================
# §1  Baseline timing: completion month (Asia/Tokyo) × initiation half-year
# ============================================================
dm$baseline_month <- NA_character_
if (!TS_VAR %in% names(dm)) {   # fallback: any column carrying the completion-timestamp label
  .cand <- grep("\u56de\u7b54\u5b8c\u4e86\u65e5\u6642", names(dm), value = TRUE)
  if (length(.cand)) { TS_VAR <- .cand[1]; log_msg(sprintf("Timestamp column resolved to '%s'.", TS_VAR)) }
}
if (TS_VAR %in% names(dm)) {
  ts <- as.POSIXct(as.character(dm[[TS_VAR]]), tz = "UTC", tryFormats = c("%Y-%m-%dT%H:%M:%SZ", "%Y-%m-%dT%H:%M:%OSZ", "%Y-%m-%d %H:%M:%S", "%Y-%m-%dT%H:%M:%S", "%Y/%m/%d %H:%M:%S", "%Y-%m-%d"))
  dm$baseline_month <- format(ts, "%Y-%m", tz = TZ_LOCAL)
  tm <- as.data.frame(table(baseline_month = dm$baseline_month, initiation = ifelse(dm$ai_start == 5L, "Jan-Jun 2025 (code 5)", "Jul-Dec 2025 (code 6)"), useNA = "ifany"))
  write_table(tm, "timing_baseline_month")
  log_msg(sprintf("Baseline completion month (JST): %s", paste(sprintf("%s=%d", names(table(dm$baseline_month)), as.integer(table(dm$baseline_month))), collapse = " ")))
  log_msg(sprintf("Unparsed timestamps: %d", sum(is.na(ts))))
} else log_msg(sprintf("NOTE: timestamp column '%s' not found — S7 (January exclusion) will be skipped.", TS_VAR))

# ============================================================
# §2  Attrition IPW: P(followed in 2025 | baseline covariates) on the full 2024 file
# ============================================================
dm$.w_ipw <- NA_real_
ipw_ok <- FALSE
if (file.exists(ALL2024_PATH)) {
  d24 <- readr::read_csv(ALL2024_PATH, show_col_types = FALSE)
  log_msg(sprintf("2024 full file: %d rows x %d cols (%s)", nrow(d24), ncol(d24), ALL2024_PATH))
  # accept raw names without the _2024 suffix
  if (!"AGE_2024" %in% names(d24) && "AGE" %in% names(d24)) {
    keep_as_is <- c(ID_VAR, FOLLOW_VAR)
    names(d24) <- ifelse(names(d24) %in% keep_as_is | grepl("_2024$", names(d24)), names(d24), paste0(names(d24), "_2024"))
  }
  if (!FOLLOW_VAR %in% names(d24)) {
    if (ID_VAR %in% names(d24) && ID_VAR %in% names(df)) {
      id24 <- trimws(as.character(d24[[ID_VAR]])); id2w <- trimws(as.character(df[[ID_VAR]]))
      d24[[FOLLOW_VAR]] <- as.integer(id24 %in% id2w)
      log_msg(sprintf("Follow-up flag derived from %s match with the two-wave file: %d of %d 2024 respondents matched (two-wave file has %d rows, %d unique IDs).",
                      ID_VAR, sum(d24[[FOLLOW_VAR]]), nrow(d24), nrow(df), length(unique(id2w))))
      if (sum(d24[[FOLLOW_VAR]]) != length(unique(id2w)))
        log_msg("WARNING: matched count differs from the number of two-wave respondents — check ID formatting in the two files.")
    } else log_msg(sprintf("NOTE: neither '%s' nor a shared '%s' column found in the 2024 file — S9 (IPW) will be skipped.", FOLLOW_VAR, ID_VAR))
  }
  if (FOLLOW_VAR %in% names(d24)) {
    d24 <- build_baseline_covs(d24)
    d24$followed <- as.integer(.as_num(d24[[FOLLOW_VAR]]) == 1L)
    cc24 <- complete.cases(d24[, c("followed", C_VARS), drop = FALSE])
    d24m <- d24[cc24, , drop = FALSE]
    cv24 <- C_VARS[vapply(C_VARS, function(v) length(unique(d24m[[v]])) > 1L, logical(1))]
    fit24 <- glm(as.formula(paste("followed ~", paste(cv24, collapse = " + "))), data = d24m, family = binomial())
    p_hat <- fitted(fit24); p_marg <- mean(d24m$followed)
    # c-statistic (AUC) by rank statistic
    r <- rank(p_hat); n1 <- sum(d24m$followed == 1L); n0 <- sum(d24m$followed == 0L)
    auc <- (sum(r[d24m$followed == 1L]) - n1 * (n1 + 1) / 2) / (n1 * n0)
    co <- summary(fit24)$coefficients
    write_table(data.frame(term = rownames(co), estimate = co[, 1], se = co[, 2], z = co[, 3], p = co[, 4],
                           OR = exp(co[, 1]), row.names = NULL), "ipw_attrition_model")
    # predicted response probability for the analytic cohort from ITS OWN baseline covariates (no merge needed)
    p_coh <- predict(fit24, newdata = dm[, cv24, drop = FALSE], type = "response")
    sw <- p_marg / p_coh
    q <- quantile(sw, IPW_TRUNC, names = FALSE); sw_t <- pmin(pmax(sw, q[1]), q[2])
    dm$.w_ipw <- sw_t
    write_table(data.frame(
      quantity = c("n_2024_respondents_modelled", "n_followed_2025", "response_rate", "attrition_model_c_statistic",
                   "n_analytic_cohort", "sw_mean", "sw_sd", "sw_min", "sw_p01", "sw_p50", "sw_p99", "sw_max",
                   "truncation_lower_pct", "truncation_upper_pct", "sw_trunc_mean", "sw_trunc_min", "sw_trunc_max"),
      value = c(nrow(d24m), n1, p_marg, auc, nrow(dm), mean(sw), sd(sw), min(sw), q[1], median(sw), q[2], max(sw),
                100 * IPW_TRUNC[1], 100 * IPW_TRUNC[2], mean(sw_t), min(sw_t), max(sw_t))), "ipw_weights_summary")
    log_msg(sprintf("IPW: modelled n=%d, followed=%d (%.1f%%), c=%.3f; stabilized weights mean=%.3f sd=%.3f range=[%.3f, %.3f] (truncated to [%.3f, %.3f])",
                    nrow(d24m), n1, 100 * p_marg, auc, mean(sw), sd(sw), min(sw), max(sw), q[1], q[2]))
    ipw_ok <- TRUE
  }
} else log_msg(sprintf("NOTE: 2024 full file not found at %s — S9 (IPW) will be skipped.", ALL2024_PATH))

# ============================================================
# §3  Closed-form natural-effects engine (categorical exposure, linear models, optional weights)
# ============================================================
ESTIMANDS <- c("TE", "CDE", "PNDE", "TNIE", "PNIE", "INT_med", "INT_ref")
nat_eff_cat <- function(d, expo, M, Y, cv, w = NULL) {
  cv <- cv[vapply(cv, function(v) length(unique(d[[v]])) > 1L, logical(1))]
  d[[expo]] <- droplevels(d[[expo]])
  lev <- setdiff(levels(d[[expo]]), "None")
  out <- matrix(NA_real_, length(ESTIMANDS), length(lev), dimnames = list(ESTIMANDS, lev))
  if (!length(lev)) return(out)
  if (is.null(w)) w <- rep(1, nrow(d))
  d$.wt <- w
  fm <- lm(as.formula(paste0(M, " ~ ", expo, " + ", paste(cv, collapse = " + "))), data = d, weights = .wt)
  fy <- lm(as.formula(paste0(Y, " ~ ", expo, " * ", M, " + ", paste(cv, collapse = " + "))), data = d, weights = .wt)
  th2 <- unname(coef(fy)[M])
  # mean predicted mediator under A = None over the (weighted) covariate distribution
  d0 <- d; d0[[expo]] <- factor("None", levels = levels(d[[expo]]))
  m0 <- weighted.mean(predict(fm, newdata = d0), w)
  for (L in lev) {
    bL  <- unname(coef(fm)[paste0(expo, L)])
    t1L <- unname(coef(fy)[paste0(expo, L)])
    t3L <- unname(coef(fy)[paste0(expo, L, ":", M)])
    if (all(is.finite(c(bL, th2, t1L, t3L, m0)))) {
      TNIE <- (th2 + t3L) * bL; PNIE <- th2 * bL; INT_med <- t3L * bL
      CDE <- t1L; INT_ref <- t3L * m0; PNDE <- CDE + INT_ref; TE <- PNDE + TNIE
      out[, L] <- c(TE, CDE, PNDE, TNIE, PNIE, INT_med, INT_ref)
    }
  }
  out
}
boot_p_twosided <- function(v) { v <- v[is.finite(v)]; if (length(v) < 10L) return(NA_real_); min(1, 2 * min(mean(v >= 0), mean(v <= 0))) }

.PAR_CL <- NULL
if (.run_ok) { .PAR_CL <- parallel::makeCluster(N_WORKERS); on.exit({ try(parallel::stopCluster(.PAR_CL), silent = TRUE) }, add = TRUE) }

run_cell <- function(d, M, Y, w, spec, R) {
  expo <- "A_SE_cat4"; cv <- CV_ASE
  point <- nat_eff_cat(d, expo, M, Y, cv, w)
  lev <- colnames(point)
  parallel::clusterSetRNGStream(.PAR_CL, iseed = 20260524)
  parallel::clusterExport(.PAR_CL, varlist = c("d", "expo", "M", "Y", "cv", "w", "nat_eff_cat", "ESTIMANDS"), envir = environment())
  bl <- parallel::parLapply(.PAR_CL, seq_len(R), function(b) {
    idx <- sample.int(nrow(d), nrow(d), replace = TRUE)
    tryCatch(nat_eff_cat(d[idx, , drop = FALSE], expo, M, Y, cv, if (is.null(w)) NULL else w[idx]), error = function(e) NULL)
  })
  arr <- array(NA_real_, c(R, length(ESTIMANDS), length(lev)), dimnames = list(NULL, ESTIMANDS, lev))
  for (b in seq_len(R)) { v <- bl[[b]]; if (!is.null(v) && identical(dim(v), dim(point))) arr[b, , ] <- v }
  rows <- list()
  for (L in lev) for (e in ESTIMANDS) {
    v <- arr[, e, L]
    rows[[length(rows) + 1L]] <- data.frame(spec = spec, mediator = M, outcome = Y, n = nrow(d), contrast = paste0(L, " vs None"),
                                            estimand = e, estimate = point[e, L],
                                            ci_lo = unname(quantile(v, 0.025, na.rm = TRUE)), ci_hi = unname(quantile(v, 0.975, na.rm = TRUE)),
                                            p = boot_p_twosided(v), R = R, n_boot_valid = sum(is.finite(v)), stringsAsFactors = FALSE)
  }
  # joint Wald test on the TNIE vector
  bm <- arr[, "TNIE", , drop = TRUE]; if (is.null(dim(bm))) bm <- matrix(bm, ncol = length(lev))
  bm <- bm[complete.cases(bm), , drop = FALSE]
  tv <- point["TNIE", ]; k <- length(tv); W <- NA_real_; pj <- NA_real_
  if (k >= 1L && all(is.finite(tv)) && nrow(bm) >= k + 2L) {
    Sig <- cov(bm)
    Si  <- tryCatch(solve(Sig), error = function(e) tryCatch(solve(Sig + diag(1e-8 * mean(diag(Sig)), k)), error = function(e2) NULL))
    if (!is.null(Si)) { W <- as.numeric(t(tv) %*% Si %*% tv); pj <- pchisq(W, df = k, lower.tail = FALSE) }
  }
  joint <- data.frame(spec = spec, mediator = M, outcome = Y, n = nrow(d), k_contrasts = k, wald_chisq = W, df = k, p_joint = pj,
                      n_boot_valid = nrow(bm), R = R, stringsAsFactors = FALSE)
  for (L in lev) joint[[paste0("tnie_", L)]] <- unname(tv[L])
  list(cells = do.call(rbind, rows), joint = joint)
}

# ============================================================
# §4  Specifications
# ============================================================
SPECS <- list(
  REF = list(label = "Reference: primary specification", subset = function(d) rep(TRUE, nrow(d)), weights = NULL, mediators = c("W_attach_c", "W_anthrop_c")),
  S6  = list(label = "S6 two-item attachment composite (Q40.22 + Q40.24)", subset = function(d) rep(TRUE, nrow(d)), weights = NULL, mediators = c("W_attach2_c")),
  S7  = list(label = "S7 excluding January-2025 baseline responders", subset = function(d) !is.na(d$baseline_month) & d$baseline_month != "2025-01", weights = NULL, mediators = c("W_attach_c", "W_anthrop_c")),
  S8  = list(label = "S8 July-December 2025 initiators only", subset = function(d) d$ai_start == 6L, weights = NULL, mediators = c("W_attach_c", "W_anthrop_c")),
  S9  = list(label = "S9 attrition inverse-probability weighting", subset = function(d) rep(TRUE, nrow(d)), weights = ".w_ipw", mediators = c("W_attach_c", "W_anthrop_c"))
)
if (!TS_VAR %in% names(dm)) SPECS$S7 <- NULL
if (!ipw_ok) SPECS$S9 <- NULL

all_cells <- list(); all_joint <- list(); spec_rows <- list()
if (.run_ok) {
  for (sn in names(SPECS)) {
    sp <- SPECS[[sn]]
    keep <- sp$subset(dm); keep[is.na(keep)] <- FALSE
    d <- dm[keep, , drop = FALSE]
    w <- if (is.null(sp$weights)) NULL else d[[sp$weights]]
    ok <- nrow(d) >= 50L && nlevels(droplevels(d$A_SE_cat4)) >= 2L && (is.null(w) || all(is.finite(w)))
    spec_rows[[sn]] <- data.frame(spec = sn, label = sp$label, n = nrow(d), weighted = !is.null(sp$weights),
                                  cells = if (ok) paste(table(droplevels(d$A_SE_cat4)), collapse = "/") else NA_character_,
                                  estimated = ok, stringsAsFactors = FALSE)
    if (!ok) { log_msg(sprintf("[%s] SKIP: n=%d or degenerate exposure/weights", sn, nrow(d))); next }
    log_msg(sprintf(">>> [%s] %s — n=%d, A_SE cells: %s", sn, sp$label, nrow(d), paste(sprintf("%s=%d", names(table(d$A_SE_cat4)), as.integer(table(d$A_SE_cat4))), collapse = " ")))
    for (M in sp$mediators) for (Y in names(OUTCOMES)) {
      set.seed(20260524)
      res <- run_cell(d, M, Y, w, sn, R_BOOT)
      all_cells[[length(all_cells) + 1L]] <- res$cells; all_joint[[length(all_joint) + 1L]] <- res$joint
      tn <- res$cells[res$cells$estimand == "TNIE", ]
      log_msg(sprintf("  [%s | %s -> %s] joint chi2(%d)=%.3f p=%.4g; TNIE %s", sn, M, Y, res$joint$k_contrasts, res$joint$wald_chisq, res$joint$p_joint,
                      paste(sprintf("%s=%+.3f(p=%.3f)", sub(" vs None", "", tn$contrast), tn$estimate, tn$p), collapse = " ")))
    }
  }
  if (length(all_cells)) write_table(do.call(rbind, all_cells), "robustness_cells")
  if (length(all_joint)) write_table(do.call(rbind, all_joint), "robustness_joint")
  if (length(spec_rows)) write_table(do.call(rbind, spec_rows), "robustness_specs")
}

if (!is.null(.PAR_CL)) { try(parallel::stopCluster(.PAR_CL), silent = TRUE); .PAR_CL <- NULL }
writeLines(capture.output(sessionInfo()), file.path(LOGS_DIR, sprintf("sessionInfo_robustness_%s_%s.txt", RUN_MODE, .timestamp)))
log_msg("=== END ===")
