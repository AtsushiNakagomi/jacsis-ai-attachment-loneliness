# ============================================================
# ai_attach_loneliness.R
# Mediation: AI-use purpose → perceived AI-attachment → UCLA-3 loneliness
# ============================================================
# Design  : single-mediator natural-effect decomposition (CMAverse rb, EMint)
# Cohort  : 2025 AI initiators (Q37S1_2025 in {5, 6}); n ≈ 2,490 complete-case
# Exposure: 3 AI-use purposes (A_SE / A_PC / A_DI) at 2025, each rotated focal in turn;
#           primary parameterization = cat4 (None + user-tertiles)
# Mediator: W_attach = perceived AI-attachment, mean of Q40.22 + Q40.23 + Q40.24 (1-7)
# Outcome : Y_ucla3  = UCLA-3 loneliness sum @ 2025, 0-9, higher = more lonely
# Cov (C) : 39 baseline-2024 covariates + the 2 non-focal purpose composites per run
#
# Sections
#   §0  Build cohort + cat4 / tertile / binary factors + sanity logs
#   §1  PRIMARY     cat4 mediation, all 3 purposes (CMAverse rb, EMint, 4-way decomp)
#   §1b PRIMARY     omnibus cat4 joint indirect-effect test (A_SE focal; feeds the 6-cell BH-FDR family)
#   §2  SENSITIVITY tertile mediator (cat4 × W_attach_tert)
#   §3  SENSITIVITY binary (`any vs none`) × 3 purposes
#   §4  SENSITIVITY continuous overall (∓0.5 SD) × 3 purposes + CMAverse cross-check
#   §5  SENSITIVITY per-item mediator (each Q40 item used individually; A_SE focal, cat4 + binary)
#   §6  SENSITIVITY joint-mediator interventional analogue — gformula + postc:
#                   attach net-of-anthrop + the symmetric flip; A_SE focal, this script's outcome.
#                   Present in the 3 W_attach OUTCOME scripts (each produces both directions for
#                   its own outcome); not in the anthrop trio. Exploratory; outside §7.7.
#
# Bootstrap R = 1,000 in real mode; R = 100 in dummy. 8 socket workers, L'Ecuyer-CMRG seed 20260524.
# Outputs    → output/ai_mod/mediation_attach_loneliness/{tables,figures,logs}/
# Filenames  → attach_<step>.csv  (prefix = construct, folder = construct + outcome)
# LLM never reads real data. Aggregated CSVs / logs are OK to read.
# ============================================================

suppressPackageStartupMessages({ library(here); library(parallel); library(readr); library(psych) })
set.seed(20260524)

# ---- Constants ----
N_WORKERS  <- 8L
OUTCOME    <- "Y_ucla3"
MEDIATOR   <- "W_attach"
MED_CENT   <- "W_attach_c"
MED_TERT   <- "W_attach_tert"
MED_ITEMS  <- paste0("Q40.", 22:24, "_2025")
MED_ITEM_LABELS <- c(
  "Q40.22_2025" = "Q40.22 (wanting friendship)",
  "Q40.23_2025" = "Q40.23 (preferring AI to real people)",
  "Q40.24_2025" = "Q40.24 (AI as safe haven)"
)
OUT_TAG    <- "loneliness"
PREFIX     <- "attach"

# ---- CMAverse (install once: remotes::install_github("BS1125/CMAverse")) ----
HAVE_CMAVERSE <- requireNamespace("CMAverse", quietly = TRUE) &&
  tryCatch(exists("cmest", envir = asNamespace("CMAverse"), inherits = FALSE), error = function(e) FALSE)
.cma_load_method <- if (HAVE_CMAVERSE) "package" else "none"

# ---- Path / mode detection ----
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

# ---- Output tree ----
.out_sub  <- sprintf("mediation_%s_%s", PREFIX, OUT_TAG)
OUT_ROOT  <- file.path(.proj_root, "output", "ai_mod",
                       if (RUN_MODE == "real") .out_sub else file.path("dummy", .out_sub))
TABLES_DIR <- file.path(OUT_ROOT, "tables"); FIGS_DIR <- file.path(OUT_ROOT, "figures"); LOGS_DIR <- file.path(OUT_ROOT, "logs")
for (d in c(TABLES_DIR, FIGS_DIR, LOGS_DIR)) dir.create(d, recursive = TRUE, showWarnings = FALSE)
.timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
LOG_FILE   <- file.path(LOGS_DIR, sprintf("run_%s_%s_%s_%s.log", PREFIX, OUT_TAG, RUN_MODE, .timestamp))
log_msg     <- function(...) { m <- paste0(format(Sys.time(), "[%H:%M:%S] "), paste(..., collapse = " ")); cat(m, "\n", sep = ""); cat(m, "\n", sep = "", file = LOG_FILE, append = TRUE); invisible(m) }
write_table <- function(x, name) { fp <- file.path(TABLES_DIR, paste0(PREFIX, "_", name, ".csv")); utils::write.csv(x, fp, row.names = FALSE, fileEncoding = "UTF-8"); log_msg("wrote:", fp) }

log_msg(sprintf("=== START === construct=%s outcome=%s mode=%s input=%s", PREFIX, OUT_TAG, RUN_MODE, DATA_PATH))
log_msg(sprintf("CMAverse: %s (%s) | N_WORKERS=%d", HAVE_CMAVERSE, .cma_load_method, N_WORKERS))

# ============================================================
# §0  Build cohort + helpers
# ============================================================
.as_num     <- function(x) suppressWarnings(as.numeric(x))
.row_mean   <- function(d, cols, na_rm = FALSE) { cols <- intersect(cols, names(d)); if (!length(cols)) return(rep(NA_real_, nrow(d))); rowMeans(vapply(d[cols], .as_num, numeric(nrow(d))), na.rm = na_rm) }
.row_sum_fn <- function(d, cols, fn = identity, na_rm = FALSE) { cols <- intersect(cols, names(d)); if (!length(cols)) return(rep(NA_real_, nrow(d))); rowSums(vapply(d[cols], function(v) fn(.as_num(v)), numeric(nrow(d))), na.rm = na_rm) }
.ucla_rec   <- function(x) pmax(0, pmin(3, 4 - x))     # raw 1 (always) -> 3; raw 4 (never) -> 0; higher = lonelier
.k6_rec     <- function(x) pmax(0, pmin(4, 5 - x))
.lsns_rec   <- function(x) pmax(0, pmin(5, x - 1L))    # 6-point 1..6 -> 0..5
.timeuse    <- function(d, raw, p) { v <- .as_num(d[[raw]])
  d[[paste0(p, "_band_1_2")]]   <- as.integer(v %in% c(4L, 5L))
  d[[paste0(p, "_band_3_4")]]   <- as.integer(v %in% c(6L, 7L))
  d[[paste0(p, "_band_5plus")]] <- as.integer(v %in% c(8:11))
  d[[paste0(p, "_unknown")]]    <- as.integer(is.na(v) | v == 12L)
  d
}
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

A_SPEC      <- list(A_SE = paste0("Q37S3.", c(8, 9), "_2025"),
                    A_PC = paste0("Q37S3.", c(1, 2, 4, 5), "_2025"),
                    A_DI = paste0("Q37S3.", c(3, 6, 7), "_2025"))
A_VARS      <- names(A_SPEC)
A_CONT_VARS <- paste0(A_VARS, "_continuous")
A_CENT_VARS <- paste0(A_VARS, "_c")
A_CAT4_VARS <- paste0(A_VARS, "_cat4")
A_BIN_VARS  <- paste0(A_VARS, "_bin")

df <- readr::read_csv(DATA_PATH, show_col_types = FALSE)
log_msg(sprintf("Loaded: %d rows x %d cols", nrow(df), ncol(df)))
.safe <- function(cn) if (cn %in% names(df)) .as_num(df[[cn]]) else rep(NA_real_, nrow(df))

# Outcome + exposure + mediator
df$Y_ucla3 <- .row_sum_fn(df, paste0("Q66.", 1:3, "_2025"), fn = .ucla_rec)
for (a in A_VARS) df[[paste0(a, "_continuous")]] <- .row_mean(df, A_SPEC[[a]], na_rm = FALSE) - 1
df$W_attach <- .row_mean(df, MED_ITEMS, na_rm = FALSE)

# 39 baseline-2024 confounders
df$baseline_ucla3 <- .row_sum_fn(df, paste0("Q66.", 1:3, "_2024"), fn = .ucla_rec)
df$baseline_k6    <- .row_sum_fn(df, paste0("Q65.", 1:6, "_2024"), fn = .k6_rec)

# ACE: count Q77.1-8 + Q77.13 as "yes (1)"; Q77.9 inverted ("no (2)" = exposure present)
ace_cols <- intersect(paste0("Q77.", c(1:8, 13), "_2024"), names(df))
if (length(ace_cols)) {
  ap  <- vapply(df[ace_cols], function(v) as.integer(.as_num(v) == 1L), integer(nrow(df)))
  aps <- rowSums(ap, na.rm = TRUE)
  if ("Q77.9_2024" %in% names(df)) {
    q9 <- .as_num(df$Q77.9_2024); a9 <- as.integer(q9 == 2L); a9[is.na(q9)] <- 0L
  } else a9 <- 0L
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
  "walking_band_1_2", "walking_band_3_4", "walking_band_5plus", "walking_unknown"
)
stopifnot(length(C_VARS) == 39L)

# Cohort filter + complete-case
ai_start <- .safe("Q37S1_2025"); df$ai_start <- ai_start
ds <- df[ai_start %in% c(5L, 6L), , drop = FALSE]
log_msg(sprintf("Cohort (2025 initiators, Q37S1 in {5, 6}): n = %d", nrow(ds)))

required <- unique(c(OUTCOME, A_CONT_VARS, MEDIATOR, C_VARS))
dm <- ds[complete.cases(ds[, required, drop = FALSE]), , drop = FALSE]
log_msg(sprintf("Analytic n (complete-case): %d", nrow(dm)))
if (RUN_MODE == "real") stopifnot(nrow(dm) >= 2000L)

# Mean-centering
for (a in A_CONT_VARS) dm[[sub("_continuous$", "_c", a)]] <- dm[[a]] - mean(dm[[a]])
dm$W_attach_c <- dm$W_attach - mean(dm$W_attach)
# Per-item centered mediators (§5 per-item sensitivity uses each item individually)
for (it in MED_ITEMS) dm[[paste0(it, "_c")]] <- dm[[it]] - mean(dm[[it]])
log_msg("Mean-centered A_*_c, W_attach_c, and per-item Q40.*_c.")

# Sample flow
write_table(data.frame(
  step = c("raw", "cohort_initiators_5_6", "analytic_complete_case"),
  n    = c(nrow(df), nrow(ds), nrow(dm))
), "sample_flow")

# Alpha + distribution of focal mediator
.alpha <- tryCatch(suppressWarnings(suppressMessages(psych::alpha(as.matrix(dm[, MED_ITEMS]))$total$raw_alpha)),
                   error = function(e) NA_real_)
log_msg(sprintf("W_attach: mean=%.3f sd=%.3f Cronbach alpha=%.3f",
                mean(dm$W_attach), sd(dm$W_attach), .alpha))
write_table(data.frame(scale = "W_attach", items = paste(MED_ITEMS, collapse = ","),
                       mean = mean(dm$W_attach), sd = sd(dm$W_attach), raw_alpha = .alpha),
            "alpha")

# Distribution table (3 A_purpose continuous + W_attach), 10-quantile summary
dist_one <- function(x, nm) {
  q <- quantile(x, probs = seq(0, 1, .1), names = FALSE)
  data.frame(variable = nm, n = length(x), mean = mean(x), sd = sd(x),
             min = min(x), max = max(x), frac_zero = mean(x == 0), n_unique = length(unique(x)),
             q10 = q[2], q20 = q[3], q30 = q[4], q40 = q[5], q50 = q[6],
             q60 = q[7], q70 = q[8], q80 = q[9], q90 = q[10], stringsAsFactors = FALSE)
}

# Guard against degenerate dummy samples (no initiators -> n=0)
.run_ok <- nrow(dm) >= 50L && sd(dm$A_SE_c) > 0 && sd(dm$W_attach_c) > 0
if (!.run_ok) log_msg(sprintf("GUARD: degenerate sample (n=%d). Sample-flow only; skipping cmest sections.", nrow(dm)))

CELL_MIN <- 30L  # log-only warning threshold for non-None cat4 cells

if (.run_ok) {
  # cat4 + tertile mediator + binary factors
  for (a in A_VARS) dm[[paste0(a, "_cat4")]] <- make_cat4(dm[[paste0(a, "_continuous")]])
  qw  <- quantile(dm$W_attach, c(1/3, 2/3), names = FALSE)
  brw <- unique(c(-Inf, qw, Inf))
  dm$W_attach_tert <- cut(dm$W_attach, breaks = brw,
                          labels = paste0("W", seq_len(length(brw) - 1L)), include.lowest = TRUE)
  for (a in A_VARS) dm[[paste0(a, "_bin")]] <- factor(ifelse(dm[[paste0(a, "_continuous")]] > 0, "any", "none"),
                                                     levels = c("none", "any"))

  # distribution + cells tables
  write_table(rbind(do.call(rbind, lapply(A_CONT_VARS, function(v) dist_one(dm[[v]], v))),
                    dist_one(dm$W_attach, "W_attach")), "distributions")

  cells <- list()
  for (focal in A_VARS) {
    xv <- dm[[paste0(focal, "_continuous")]]; pos <- xv > 0
    qq <- if (any(pos)) quantile(xv[pos], c(1/3, 2/3), names = FALSE) else c(NA, NA)
    nb <- table(dm[[paste0(focal, "_cat4")]])
    for (L in names(nb)) cells[[length(cells) + 1L]] <- data.frame(
      purpose = focal, level = L, n = as.integer(nb[[L]]),
      user_t1_cut = qq[1], user_t2_cut = qq[2], stringsAsFactors = FALSE)
    log_msg(sprintf("cat4 cells [%s]: %s  (user cuts %.3f, %.3f)", focal,
                    paste(sprintf("%s=%d", names(nb), as.integer(nb)), collapse = " "), qq[1], qq[2]))
    small <- names(nb)[as.integer(nb) < CELL_MIN & names(nb) != "None"]
    if (length(small)) log_msg(sprintf("  NOTE [%s]: small non-None cells (n<%d): %s",
                                       focal, CELL_MIN, paste(sprintf("%s=%d", small, as.integer(nb[small])), collapse = " ")))
  }
  write_table(do.call(rbind, cells), "cat4_cells")
  log_msg(sprintf("W_attach tertile cells: %s  (cuts %.3f, %.3f)",
                  paste(sprintf("%s=%d", names(table(dm$W_attach_tert)), as.integer(table(dm$W_attach_tert))), collapse = " "),
                  qw[1], qw[2]))
  for (a in A_VARS) log_msg(sprintf("%s binary cells: %s", a,
                                    paste(sprintf("%s=%d", names(table(dm[[paste0(a, "_bin")]])),
                                                  as.integer(table(dm[[paste0(a, "_bin")]]))), collapse = " ")))
}

# Parallel cluster (used by §4 closed-form bootstrap)
.PAR_CL <- NULL
if (.run_ok) {
  .PAR_CL <- parallel::makeCluster(N_WORKERS)
  on.exit({ try(parallel::stopCluster(.PAR_CL), silent = TRUE) }, add = TRUE)
}

R_BOOT <- if (RUN_MODE == "dummy") 100L else 1000L

# CMAverse driver for one (exposure-level vs reference) contrast
cma_cat <- function(expo, astar, a, mediator_col, mreg_type, R) {
  base_purp <- sub("_cat4$|_bin$", "", expo); base_purp <- if (base_purp %in% A_VARS) base_purp else "A_SE"
  basec <- c(C_VARS, paste0(setdiff(A_VARS, base_purp), "_c"))
  cols <- unique(c(OUTCOME, expo, mediator_col, basec))
  d <- dm[, intersect(cols, names(dm)), drop = FALSE]; d <- d[complete.cases(d), , drop = FALSE]
  d[[expo]] <- droplevels(d[[expo]])
  if (!all(c(astar, a) %in% levels(d[[expo]]))) return(NULL)
  mval_default <- if (mreg_type == "linear") list(0) else list(levels(d[[mediator_col]])[1])
  fit <- tryCatch(
    CMAverse::cmest(data = d, model = "rb", outcome = OUTCOME, exposure = expo, mediator = mediator_col,
                    basec = basec, yreg = "linear", mreg = list(mreg_type),
                    astar = astar, a = a, EMint = TRUE, mval = mval_default,
                    estimation = "imputation", inference = "bootstrap",
                    nboot = R, boot.ci.type = "per"),
    error = function(e) { log_msg(sprintf("  cmest [%s: %s vs %s] failed: %s", expo, a, astar, conditionMessage(e))); NULL })
  if (is.null(fit)) return(NULL)
  ec <- summary(fit)$summarydf
  data.frame(exposure = expo, contrast = paste0(a, " vs ", astar), mediator = mediator_col,
             estimand = rownames(ec), ec,
             row.names = NULL, check.names = FALSE, stringsAsFactors = FALSE)
}

# Closed-form linear natural-effects engine (§4 continuous A; CMAverse cross-check)
nat_eff <- function(d, focal, cv, a_sd) {
  Af <- paste0(focal, "_c"); M <- MED_CENT
  cv <- cv[vapply(cv, function(v) length(unique(d[[v]])) > 1L, logical(1))]
  fm <- lm(as.formula(paste0(M,        " ~ ", paste(c(Af, cv), collapse = " + "))), data = d)
  fy <- lm(as.formula(paste0(OUTCOME, " ~ ", paste(c(Af, M, paste0(Af, ":", M), cv), collapse = " + "))), data = d)
  b1  <- unname(coef(fm)[Af])
  th1 <- unname(coef(fy)[Af]); th2 <- unname(coef(fy)[M]); th3 <- unname(coef(fy)[paste0(Af, ":", M)])
  if (any(!is.finite(c(b1, th1, th2, th3))))
    return(setNames(rep(NA_real_, 11L), c("TE","CDE","PNDE","TNDE","PNIE","TNIE","INT_med","INT_ref","PM","a_path_b1","bm_theta2")))
  a <- 0.5 * a_sd; astar <- -0.5 * a_sd; delta <- a - astar
  CDE     <- delta * th1
  PNDE    <- delta * (th1 + th3 * b1 * astar)
  TNDE    <- delta * (th1 + th3 * b1 * a)
  TNIE    <- b1 * delta * (th2 + th3 * a)
  PNIE    <- b1 * delta * (th2 + th3 * astar)
  INT_med <- TNIE - PNIE
  INT_ref <- PNDE - CDE
  TE      <- PNDE + TNIE
  PM      <- if (abs(TE) > 1e-10) TNIE / TE else NA_real_
  setNames(c(TE, CDE, PNDE, TNDE, PNIE, TNIE, INT_med, INT_ref, PM, b1, th2),
           c("TE","CDE","PNDE","TNDE","PNIE","TNIE","INT_med","INT_ref","PM","a_path_b1","bm_theta2"))
}
boot_p_twosided <- function(v) { v <- v[is.finite(v)]; if (length(v) < 10L) return(NA_real_); min(1, 2 * min(mean(v >= 0), mean(v <= 0))) }

# Closed-form per-level natural indirect effect (TNIE) for a categorical (cat4) exposure
# (linear M-model, linear Y-model with A x M interaction; matches CMAverse rb point estimate).
# Returns a named vector of TNIE for each non-None level of `expo`.
nat_eff_cat_tnie <- function(d, expo, cv) {
  M  <- MED_CENT
  cv <- cv[vapply(cv, function(v) length(unique(d[[v]])) > 1L, logical(1))]
  d[[expo]] <- droplevels(d[[expo]])
  lev <- setdiff(levels(d[[expo]]), "None")
  if (!length(lev)) return(setNames(numeric(0), character(0)))
  fm <- lm(as.formula(paste0(M,       " ~ ", expo, " + ",          paste(cv, collapse = " + "))), data = d)
  fy <- lm(as.formula(paste0(OUTCOME, " ~ ", expo, " * ", M, " + ", paste(cv, collapse = " + "))), data = d)
  th2 <- unname(coef(fy)[M])
  out <- setNames(rep(NA_real_, length(lev)), lev)
  for (L in lev) {
    bL  <- unname(coef(fm)[paste0(expo, L)])
    t3L <- unname(coef(fy)[paste0(expo, L, ":", M)])
    if (all(is.finite(c(bL, th2, t3L)))) out[L] <- (th2 + t3L) * bL
  }
  out
}

# Omnibus joint indirect-effect test for a cat4 exposure: H0 TNIE_L = 0 for all non-None L.
# Wald chi-square on the per-level TNIE vector with bootstrap covariance (R reps).
# Returns list(point, k, W, p, n_boot) or NULL if degenerate / non-invertible.
omnibus_cat <- function(d, expo, cv, R) {
  point <- nat_eff_cat_tnie(d, expo, cv)
  k <- length(point)
  if (k < 1L || any(!is.finite(point))) return(NULL)
  parallel::clusterSetRNGStream(.PAR_CL, iseed = 20260524)
  parallel::clusterExport(.PAR_CL,
                          varlist = c("d", "expo", "cv", "nat_eff_cat_tnie", "MED_CENT", "OUTCOME"),
                          envir = environment())
  bl <- parallel::parLapply(.PAR_CL, seq_len(R), function(b) {
    idx <- sample.int(nrow(d), nrow(d), replace = TRUE)
    tryCatch(nat_eff_cat_tnie(d[idx, , drop = FALSE], expo, cv), error = function(e) NULL)
  })
  bm <- matrix(NA_real_, R, k, dimnames = list(NULL, names(point)))
  for (b in seq_len(R)) { v <- bl[[b]]; if (!is.null(v) && length(v) == k) bm[b, names(point)] <- v[names(point)] }
  bm <- bm[complete.cases(bm), , drop = FALSE]
  if (nrow(bm) < (k + 2L)) return(NULL)
  Sig <- cov(bm)
  Si  <- tryCatch(solve(Sig), error = function(e)
           tryCatch(solve(Sig + diag(1e-8 * mean(diag(Sig)), k)), error = function(e2) NULL))
  if (is.null(Si)) return(NULL)
  W <- as.numeric(t(point) %*% Si %*% point)
  list(point = point, k = k, W = W, p = pchisq(W, df = k, lower.tail = FALSE), n_boot = nrow(bm))
}

# ============================================================
# §1  PRIMARY — cat4 mediation, all 3 purposes (CMAverse rb, EMint, 4-way decomposition)
# ============================================================
if (.run_ok && HAVE_CMAVERSE) {
  res <- list()
  for (focal in A_VARS) {
    expo <- paste0(focal, "_cat4")
    for (L in setdiff(levels(dm[[expo]]), "None")) {
      set.seed(20260524)
      log_msg(sprintf(">>> §1 PRIMARY cat4 [%s: %s vs None] R=%d ...", focal, L, R_BOOT))
      r <- cma_cat(expo, "None", L, MED_CENT, "linear", R_BOOT)
      if (!is.null(r)) res[[length(res) + 1L]] <- r
    }
  }
  if (length(res)) {
    rd <- do.call(rbind, res); write_table(rd, "cat4_primary")
    for (i in which(rd$estimand == "tnie")) log_msg(sprintf(
      "  [%s] %s: tnie=%+.4f (CI %+.4f, %+.4f, p=%.3g)",
      rd$exposure[i], rd$contrast[i], rd$Estimate[i],
      rd[["95% CIL"]][i], rd[["95% CIU"]][i], rd$P.val[i]))
  }
} else if (.run_ok) log_msg("§1 SKIP: CMAverse not available.")

# ============================================================
# §1b  PRIMARY OMNIBUS — cat4 joint indirect-effect test (A_SE focal only)
# Joint H0: TNIE_Low = TNIE_Mid = TNIE_High = 0 (Wald chi-square on the closed-form
# per-level TNIE vector; bootstrap covariance). One test for this (mediator x outcome)
# cell; the 6 such cells (2 mediators x 3 outcomes) form the primary multiplicity family,
# BH-FDR-corrected in the packager (codebook §7.7). The cat4 Low/Mid/High contrasts (§1)
# are descriptive dose-response, NOT separate tests.
# ============================================================
if (.run_ok) {
  set.seed(20260524)
  cv_ase <- c(C_VARS, setdiff(A_CENT_VARS, "A_SE_c"))
  log_msg(sprintf(">>> §1b omnibus cat4 indirect-effect [A_SE x %s -> %s] R=%d ...", MEDIATOR, OUTCOME, R_BOOT))
  ob <- omnibus_cat(dm, "A_SE_cat4", cv_ase, R_BOOT)
  if (!is.null(ob)) {
    row <- data.frame(mediator = MEDIATOR, outcome = OUTCOME, exposure = "A_SE_cat4",
                      k_contrasts = ob$k, wald_chisq = ob$W, df = ob$k, p_omnibus = ob$p,
                      n_boot_valid = ob$n_boot, R = R_BOOT, stringsAsFactors = FALSE)
    for (nm in names(ob$point)) row[[paste0("tnie_", nm)]] <- unname(ob$point[[nm]])
    write_table(row, "omnibus_primary")
    log_msg(sprintf("  omnibus [A_SE x %s -> %s]: chi2(%d)=%.3f p=%.4g (n_boot=%d)",
                    MEDIATOR, OUTCOME, ob$k, ob$W, ob$p, ob$n_boot))
  } else log_msg("§1b omnibus: skipped (degenerate sample / non-invertible covariance).")
}

# ============================================================
# §2  SENSITIVITY — tertile mediator (cat4 × W_attach_tert; multinomial mreg)
# ============================================================
if (.run_ok && HAVE_CMAVERSE && nlevels(droplevels(dm$W_attach_tert)) >= 2L) {
  res <- list()
  for (focal in A_VARS) {
    expo <- paste0(focal, "_cat4")
    for (L in setdiff(levels(dm[[expo]]), "None")) {
      set.seed(20260524)
      log_msg(sprintf(">>> §2 tertile-M [%s: %s vs None] R=%d ...", focal, L, R_BOOT))
      r <- cma_cat(expo, "None", L, MED_TERT, "multinomial", R_BOOT)
      if (!is.null(r)) res[[length(res) + 1L]] <- r
    }
  }
  if (length(res)) write_table(do.call(rbind, res), "tertile_sens")
}

# ============================================================
# §3  SENSITIVITY — binary exposure (any vs none) across 3 purposes
# ============================================================
if (.run_ok && HAVE_CMAVERSE) {
  res <- list()
  for (focal in A_VARS) {
    expo <- paste0(focal, "_bin")
    if (nlevels(droplevels(dm[[expo]])) != 2L) next
    set.seed(20260524)
    log_msg(sprintf(">>> §3 binary [%s: any vs none] R=%d ...", focal, R_BOOT))
    r <- cma_cat(expo, "none", "any", MED_CENT, "linear", R_BOOT)
    if (!is.null(r)) {
      res[[length(res) + 1L]] <- r
      rr <- r[r$estimand == "tnie", , drop = FALSE]
      if (nrow(rr)) log_msg(sprintf("  [%s] binary any-vs-none: tnie=%+.4f (CI %+.4f, %+.4f, p=%.3g)",
                                    focal, rr$Estimate, rr[["95% CIL"]], rr[["95% CIU"]], rr$P.val))
    }
  }
  if (length(res)) write_table(do.call(rbind, res), "binary_sens")
}

# ============================================================
# §4  SENSITIVITY — continuous exposure overall (∓0.5 SD) + CMAverse cross-check
# ============================================================
boot_nat_eff <- function(d, focal, cv, a_sd, R) {
  point <- nat_eff(d, focal, cv, a_sd)
  parallel::clusterSetRNGStream(.PAR_CL, iseed = 20260524)
  parallel::clusterExport(.PAR_CL,
                          varlist = c("d","focal","cv","a_sd","nat_eff","MED_CENT","OUTCOME"),
                          envir = environment())
  bl <- parallel::parLapply(.PAR_CL, seq_len(R), function(b) {
    idx <- sample.int(nrow(d), nrow(d), replace = TRUE)
    tryCatch(nat_eff(d[idx, , drop = FALSE], focal, cv, a_sd), error = function(e) NULL)
  })
  bm <- matrix(NA_real_, R, length(point)); colnames(bm) <- names(point)
  for (b in seq_len(R)) if (!is.null(bl[[b]])) bm[b, ] <- bl[[b]]
  data.frame(estimand = names(point), estimate = unname(point),
             se     = apply(bm, 2, sd, na.rm = TRUE),
             ci_lo  = apply(bm, 2, quantile, probs = .025, na.rm = TRUE),
             ci_hi  = apply(bm, 2, quantile, probs = .975, na.rm = TRUE),
             p      = apply(bm, 2, boot_p_twosided),
             R      = R, stringsAsFactors = FALSE)
}

if (.run_ok) {
  res_cf <- list(); res_cm <- list()
  for (focal in A_VARS) {
    cv_focal <- c(C_VARS, setdiff(A_CENT_VARS, paste0(focal, "_c")))
    a_sd     <- sd(dm[[paste0(focal, "_c")]])

    # closed-form linear + bootstrap
    set.seed(20260524)
    log_msg(sprintf(">>> §4 continuous %s (closed-form + bootstrap) ...", focal))
    rr <- boot_nat_eff(dm, focal, cv_focal, a_sd, R_BOOT); rr$focal <- focal
    res_cf[[length(res_cf) + 1L]] <- rr
    for (e in c("TE","TNIE","PNIE","INT_med","PNDE","CDE","a_path_b1")) {
      r <- rr[rr$estimand == e, ]
      if (nrow(r)) log_msg(sprintf("  [%s] %-10s = %+.4f (CI %+.4f, %+.4f, p=%.3g)",
                                   focal, e, r$estimate, r$ci_lo, r$ci_hi, r$p))
    }

    # CMAverse rb cross-check (continuous exposure)
    if (HAVE_CMAVERSE) {
      set.seed(20260524)
      expo_c <- paste0(focal, "_c")
      cols <- unique(c(OUTCOME, expo_c, MED_CENT, cv_focal))
      d <- dm[, intersect(cols, names(dm)), drop = FALSE]; d <- d[complete.cases(d), , drop = FALSE]
      fit <- tryCatch(
        CMAverse::cmest(data = d, model = "rb", outcome = OUTCOME, exposure = expo_c, mediator = MED_CENT,
                        basec = cv_focal, yreg = "linear", mreg = list("linear"),
                        astar = -0.5 * a_sd, a = 0.5 * a_sd, EMint = TRUE, mval = list(0),
                        estimation = "imputation", inference = "bootstrap",
                        nboot = R_BOOT, boot.ci.type = "per"),
        error = function(e) { log_msg(sprintf("  §4 cmest [%s] failed: %s", focal, conditionMessage(e))); NULL })
      if (!is.null(fit)) {
        ec <- summary(fit)$summarydf
        res_cm[[length(res_cm) + 1L]] <- data.frame(focal = focal, estimand = rownames(ec), ec,
                                                    row.names = NULL, check.names = FALSE)
      }
    }
  }
  if (length(res_cf)) write_table(do.call(rbind, res_cf), "continuous_sens")
  if (length(res_cm)) write_table(do.call(rbind, res_cm), "continuous_sens_cmaverse")
}

# ============================================================
# §5  SENSITIVITY — per-item mediator (each Q40 item used individually)
# A_SE focal only. Tests whether the composite construct rides on one item or
# moves together across all three. Cat4 primary + binary; no tertile / no continuous
# per-item (single ordinal items don't benefit from those reparameterizations).
# ============================================================
if (.run_ok && HAVE_CMAVERSE) {
  res_cat4 <- list(); res_bin <- list()
  for (it in MED_ITEMS) {
    mcol <- paste0(it, "_c")
    item_label <- unname(MED_ITEM_LABELS[it])

    # §5a per-item cat4 mediation, A_SE focal
    expo <- "A_SE_cat4"
    for (L in setdiff(levels(dm[[expo]]), "None")) {
      set.seed(20260524)
      log_msg(sprintf(">>> §5a per-item cat4 [item=%s, A_SE: %s vs None] R=%d ...", it, L, R_BOOT))
      r <- cma_cat(expo, "None", L, mcol, "linear", R_BOOT)
      if (!is.null(r)) {
        r$mediator_item  <- it
        r$mediator_label <- item_label
        res_cat4[[length(res_cat4) + 1L]] <- r
      }
    }

    # §5b per-item binary mediation, A_SE focal
    expo <- "A_SE_bin"
    if (nlevels(droplevels(dm[[expo]])) == 2L) {
      set.seed(20260524)
      log_msg(sprintf(">>> §5b per-item binary [item=%s, A_SE: any vs none] R=%d ...", it, R_BOOT))
      r <- cma_cat(expo, "none", "any", mcol, "linear", R_BOOT)
      if (!is.null(r)) {
        r$mediator_item  <- it
        r$mediator_label <- item_label
        res_bin[[length(res_bin) + 1L]] <- r
      }
    }
  }
  if (length(res_cat4)) write_table(do.call(rbind, res_cat4), "per_item_cat4")
  if (length(res_bin))  write_table(do.call(rbind, res_bin),  "per_item_binary")
}

# ============================================================
# §6  SENSITIVITY — joint-mediator randomized interventional analogue
# W_attach and W_anthrop correlate r≈0.65 → natural path-specific effects are unidentified
# (codebook §3). The randomized INTERVENTIONAL analogue (Vansteelandt & Daniel 2017) IS
# identified with the OTHER mediator as an exposure-induced (post-treatment) confounder
# (`postc`). Two symmetric g-formula runs, A_SE focal, UCLA-3 (this script's outcome) only:
#   (a) mediator = W_attach_c,  postc = W_anthrop_c   → r-effects through attachment, net of anthropomorphism
#   (b) mediator = W_anthrop_c, postc = W_attach_c    → r-effects through anthropomorphism, net of attachment
# CMAverse model="gformula" returns r-PREFIXED effects (cde/rpnde/rtnde/rpnie/rtnie/rintref/
# rintmed/te/rpm) — passed through verbatim, NEVER relabeled as natural effects. Single-process
# cmest bootstrap (mirrors §1–§3). Exploratory: outside the §7.7 primary family; does not touch
# Table 2/2b/3. Present in the 3 W_attach OUTCOME scripts; each produces BOTH
# directions for its own outcome. W_anthrop is built LOCALLY so §0 stays identical across the 6.
# Output: attach_interventional.csv in this outcome's folder (packager Sup Table 11).
# ============================================================
if (.run_ok && HAVE_CMAVERSE) {
  ANTHROP_ITEMS <- paste0("Q40.", 1:3, "_2025")
  dmi <- dm
  dmi$W_anthrop <- .row_mean(dmi, ANTHROP_ITEMS, na_rm = FALSE)
  dmi <- dmi[complete.cases(dmi[, "W_anthrop", drop = FALSE]), , drop = FALSE]
  if (nrow(dmi) >= 50L && sd(dmi$W_anthrop) > 0) {
    # re-center mediators + A on this (identical n=2,490) subsample for internal consistency
    dmi$W_anthrop_c <- dmi$W_anthrop - mean(dmi$W_anthrop)
    dmi$W_attach_c  <- dmi$W_attach  - mean(dmi$W_attach)
    for (a in A_VARS) dmi[[paste0(a, "_c")]] <- dmi[[paste0(a, "_continuous")]] - mean(dmi[[paste0(a, "_continuous")]])
    log_msg(sprintf("§6 interventional: n=%d, r(W_attach, W_anthrop)=%.3f", nrow(dmi), cor(dmi$W_attach, dmi$W_anthrop)))

    cma_gformula <- function(mediator_col, postc_col, astar, a, R) {
      basec <- c(C_VARS, paste0(setdiff(A_VARS, "A_SE"), "_c"))   # A_SE focal → non-focal A_PC_c, A_DI_c in basec
      cols  <- unique(c(OUTCOME, "A_SE_cat4", mediator_col, postc_col, basec))
      d <- dmi[, intersect(cols, names(dmi)), drop = FALSE]; d <- d[complete.cases(d), , drop = FALSE]
      d$A_SE_cat4 <- droplevels(d$A_SE_cat4)
      if (!all(c(astar, a) %in% levels(d$A_SE_cat4))) return(NULL)
      fit <- tryCatch(
        CMAverse::cmest(data = d, model = "gformula", outcome = OUTCOME, exposure = "A_SE_cat4",
                        mediator = mediator_col, basec = basec, postc = postc_col,
                        yreg = "linear", mreg = list("linear"), postcreg = list("linear"),
                        astar = astar, a = a, EMint = TRUE, mval = list(0),
                        estimation = "imputation", inference = "bootstrap",
                        nboot = R, boot.ci.type = "per"),
        error = function(e) { log_msg(sprintf("  §6 gformula [%s|postc=%s: %s vs %s] failed: %s",
                                              mediator_col, postc_col, a, astar, conditionMessage(e))); NULL })
      if (is.null(fit)) return(NULL)
      ec <- summary(fit)$summarydf
      data.frame(mediator = mediator_col, postc = postc_col, exposure = "A_SE_cat4",
                 contrast = paste0(a, " vs ", astar), estimand = rownames(ec), ec,
                 row.names = NULL, check.names = FALSE, stringsAsFactors = FALSE)
    }

    CONFIGS6 <- list(c("W_attach_c", "W_anthrop_c", "attach_net_of_anthrop"),
                     c("W_anthrop_c", "W_attach_c", "anthrop_net_of_attach"))
    res6 <- list()
    for (cfg in CONFIGS6) {
      for (L in setdiff(levels(dmi$A_SE_cat4), "None")) {
        set.seed(20260524)
        log_msg(sprintf(">>> §6 interventional [%s | postc=%s | A_SE %s vs None] R=%d ...", cfg[1], cfg[2], L, R_BOOT))
        r <- cma_gformula(cfg[1], cfg[2], "None", L, R_BOOT)
        if (!is.null(r)) {
          r$config <- cfg[3]
          res6[[length(res6) + 1L]] <- r
          rr <- r[r$estimand == "rtnie", , drop = FALSE]
          if (nrow(rr)) log_msg(sprintf("  [%s] %s: rTNIE=%+.4f (CI %+.4f, %+.4f, p=%.3g)  [randomized interventional analogue]",
                                        cfg[3], L, rr$Estimate, rr[["95% CIL"]], rr[["95% CIU"]], rr$P.val))
        }
      }
    }
    if (length(res6)) write_table(do.call(rbind, res6), "interventional")
  } else log_msg(sprintf("§6 SKIP: W_anthrop degenerate (n=%d).", nrow(dmi)))
} else if (.run_ok) log_msg("§6 SKIP: CMAverse not available.")

# ---- Cleanup ----
if (!is.null(.PAR_CL)) { try(parallel::stopCluster(.PAR_CL), silent = TRUE); .PAR_CL <- NULL }
writeLines(capture.output(sessionInfo()),
           file.path(LOGS_DIR, sprintf("sessionInfo_%s_%s_%s_%s.txt", PREFIX, OUT_TAG, RUN_MODE, .timestamp)))
log_msg("=== END ===")
