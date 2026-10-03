# ============================================================
# ai_mediation_tables_figures.R
# Manuscript packager — tables + figures for the mediation pipeline.
# ============================================================
# Pure downstream CSV reader. No analytic logic; fix CSVs upstream and regenerate.
#
# Reads from:
#   output/ai_mod/mediation_attach_loneliness/tables/        (attach_*.csv)
#   output/ai_mod/mediation_attach_lsns_friends/tables/      (attach_*.csv)
#   output/ai_mod/mediation_attach_lsns_family/tables/       (attach_*.csv)
#   output/ai_mod/mediation_anthrop_loneliness/tables/       (anthrop_*.csv)
#   output/ai_mod/mediation_anthrop_lsns_friends/tables/     (anthrop_*.csv)
#   output/ai_mod/mediation_anthrop_lsns_family/tables/      (anthrop_*.csv)
#   (per-item is integrated as §5 in each outcome script -> reads from the same 6 folders)
#   output/ai_mod/mediation_attach_<outcome>/tables/attach_interventional.csv
#   output/ai_mod/diagnosis_mediation/
#   raw CSV (Table 1 / Sup 4 / Sup 5 cohort characteristics; guarded)
#
# Writes to: output/ai_mod/manuscript/{tables,figures,logs}/
#
# Layout
#   Table 1     Characteristics — Total + by A_SE cat4 (None / Low / Mid / High)
#   Table 2     PRIMARY multiplicity — cat4 omnibus joint indirect-effect test, BH-FDR over 6
#               (2 candidate mediators × 3 outcomes, A_SE focal; codebook §7.7)
#   Table 3     PRIMARY cat4 A_SE × 3 outcomes × 3 contrasts — W_attach + W_anthrop side-by-side
#   (Supplementary Tables are numbered as in the published Supplementary Data; sensitivity/
#   robustness analyses are numbered S1–S9 as in the manuscript, Section 3.3.)
#   Sup Table 1 baseline characteristics by generative-AI status at 2025 (all two-wave respondents;
#               from ai_measurement_validity.R)
#   Sup Table 2 EFA — AI-use purposes (9 items): oblique loadings + factor correlations
#   Sup Table 3 EFA — 2 mediators (6 items): oblique loadings + factor correlations
#               (attach↔anthrop factor cor ≈ 0.72)
#   Sup Table 4 Characteristics — by A_PC cat4
#   Sup Table 5 Characteristics — by A_DI cat4
#   Sup Table 6 baseline predictors of 2025 social/emotional use and of the two perceptions
#               (from ai_measurement_validity.R)
#   Sup Table 7 cat4 A_PC × 3 outcomes × 3 contrasts — W_attach + W_anthrop
#   Sup Table 8 cat4 A_DI × 3 outcomes × 3 contrasts — W_attach + W_anthrop
#   Sup Table 9 S1 tertile mediator (A_SE cat4 × W_attach_tert / W_anthrop_tert) × 3 outcomes
#   Sup Table 10 S2 binary `any vs none` × 3 purposes × 3 outcomes × both mediators
#   Sup Table 11 S3 continuous (∓0.5 SD) × 3 purposes × 3 outcomes × both mediators
#   Sup Table 12 S4 joint-mediator interventional (outcome scripts §6): A_SE cat4 × 3 outcomes,
#               r-effects, 2 configs (attach net-of-anthrop; anthrop net-of-attach)
#   Sup Table 13 S5 per-item cat4 A_SE × 3 outcomes × 3 contrasts × 6 single items (outcome scripts §5)
#               + S6 two-item attachment composite row (from ai_robustness.R)
#   Table 2b    follow-up (2025) mediators and outcomes by A_SE cat4 (from ai_measurement_validity.R)
#   Table 3b    SD-standardized TNIE (TNIE ÷ SD of the 2025 outcome) for the primary cells
#   Sup Table 14 robustness of the six primary cells: REF reference / S7 January-2025 baseline
#               responders excluded / S8 July–December 2025 initiators / S9 attrition IPW
#               (from ai_robustness.R)
#   Sup Table 15 construct distinctness: correlations, reliability, HTMT (ai_measurement_validity.R)
#   Sup Table 16 EFA attachment + UCLA-3 items; Sup Table 17 EFA attachment + anthropomorphism +
#               problematic-use items (ai_measurement_validity.R)
#
#   Figure 1    Conceptual DAG (MD placeholder — drawn manually)
#   Sup Fig 1   Sample flow chart (raw → 2025 AI initiators → analytic complete-case)
#
# Subgroup / moderated-mediation results are not packaged (no signal survives BH-FDR);
# mention in manuscript text only if relevant.
# ============================================================

set.seed(20260524)
suppressPackageStartupMessages({ library(here); library(readr); library(ggplot2); library(scales); library(dplyr); library(tidyr) })

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
  else "real"
}
DATA_PATH <- {
  if (RUN_MODE == "real" && file.exists(REAL_DATA_PATH)) REAL_DATA_PATH
  else if (file.exists(DUMMY_DATA_PATH)) DUMMY_DATA_PATH
  else NA_character_
}

# ---- Per-outcome input folders (NEW symmetric structure) ----
.tag <- if (RUN_MODE == "real") "" else "dummy/"
in_folder <- function(construct, outcome_tag) {
  .norm(file.path(.proj_root, "output", "ai_mod",
                  paste0(.tag, sprintf("mediation_%s_%s", construct, outcome_tag))))
}
OUT_ROOT    <- file.path(.proj_root, "output", "ai_mod", paste0(.tag, "manuscript"))
OUT_TAB <- file.path(OUT_ROOT, "tables"); OUT_FIG <- file.path(OUT_ROOT, "figures"); OUT_LOG <- file.path(OUT_ROOT, "logs")
for (d in c(OUT_TAB, OUT_FIG, OUT_LOG)) dir.create(d, recursive = TRUE, showWarnings = FALSE)
.timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
LOG_FILE   <- file.path(OUT_LOG, sprintf("run_packager_%s_%s.log", RUN_MODE, .timestamp))
log_msg     <- function(...) { m <- paste0(format(Sys.time(), "[%H:%M:%S] "), paste(..., collapse = " ")); cat(m, "\n", sep = ""); cat(m, "\n", sep = "", file = LOG_FILE, append = TRUE); invisible(m) }
write_table <- function(x, name) { fp <- file.path(OUT_TAB, paste0(name, ".csv")); utils::write.csv(x, fp, row.names = FALSE, fileEncoding = "UTF-8"); log_msg("wrote:", fp) }
log_msg(sprintf("=== START (packager) === mode=%s", RUN_MODE))


# ---- Outcome registry ----
OUTCOMES <- c(LON = "Subjective loneliness (UCLA-3; higher = worse)",
              FR  = "Behavioral displacement: friends (LSNS-friends; lower = displaced)",
              FA  = "Behavioral displacement: family (LSNS-family; lower = displaced)")
OUTCOME_TAGS <- c(LON = "loneliness", FR = "lsns_friends", FA = "lsns_family")
OUTCOME_VARS <- c(LON = "Y_ucla3", FR = "Y_lsns_friends_2025", FA = "Y_lsns_family_2025")

# ---- Formatters ----
fmt_est_ci <- function(est, lo, hi, digits = 3) {
  if (!is.finite(est)) return("—"); fmt <- paste0("%+.", digits, "f (%+.", digits, "f, %+.", digits, "f)")
  sprintf(fmt, as.numeric(est), as.numeric(lo), as.numeric(hi))
}
fmt_p <- function(p) if (!is.finite(p)) "—" else if (p < 0.001) "<0.001" else sprintf("%.3f", p)

# ---- CSV readers ----
read_one <- function(folder, name) {
  fp <- file.path(folder, "tables", paste0(name, ".csv"))
  if (!file.exists(fp)) { log_msg(sprintf("  missing: %s", fp)); return(NULL) }
  readr::read_csv(fp, show_col_types = FALSE)
}
read_outcome <- function(construct, oc, step) {
  read_one(in_folder(construct, OUTCOME_TAGS[[oc]]), sprintf("%s_%s", construct, step))
}
# diagnosis_mediation/ reader
DIAG_DIR <- .norm(file.path(.proj_root, "output", "ai_mod", paste0(.tag, "diagnosis_mediation")))
read_diag <- function(name) {
  fp <- file.path(DIAG_DIR, paste0(name, ".csv"))
  if (!file.exists(fp)) { log_msg(sprintf("  missing: %s", fp)); return(NULL) }
  readr::read_csv(fp, show_col_types = FALSE)
}

# CMAverse cmest estimands (lowercase, written by cmest)
ESTIMANDS_FULL <- c("cde","pnde","pnie","tnie","intmed","intref","te")
# Closed-form continuous-overall estimands (uppercase in our scripts)
ESTIMANDS_CONT <- c("TE","CDE","PNDE","TNDE","PNIE","TNIE","INT_med","INT_ref","a_path_b1","bm_theta2")

# ============================================================
# §A  Analytic cohort build (raw read; for Table 1 / Sup 4 / Sup 5)
# ============================================================
.as_num     <- function(x) suppressWarnings(as.numeric(x))
.row_mean   <- function(d, cols, na_rm = FALSE) { cols <- intersect(cols, names(d)); if (!length(cols)) return(rep(NA_real_, nrow(d))); rowMeans(vapply(d[cols], .as_num, numeric(nrow(d))), na.rm = na_rm) }
.row_sum_fn <- function(d, cols, fn = identity, na_rm = TRUE) { cols <- intersect(cols, names(d)); if (!length(cols)) return(rep(NA_real_, nrow(d))); rowSums(vapply(d[cols], function(v) fn(.as_num(v)), numeric(nrow(d))), na.rm = na_rm) }
.ucla_rec   <- function(x) pmax(0, pmin(3, 4 - x))
.k6_rec     <- function(x) pmax(0, pmin(4, 5 - x))
.lsns_rec   <- function(x) pmax(0, pmin(5, x - 1L))
.timeuse <- function(d, raw, p) { v <- .as_num(d[[raw]])
  d[[paste0(p, "_band_1_2")]]   <- as.integer(v %in% c(4L, 5L))
  d[[paste0(p, "_band_3_4")]]   <- as.integer(v %in% c(6L, 7L))
  d[[paste0(p, "_band_5plus")]] <- as.integer(v %in% c(8:11))
  d[[paste0(p, "_unknown")]]    <- as.integer(is.na(v) | v == 12L); d
}
make_cat4 <- function(x) {
  f <- rep(NA_character_, length(x)); f[x == 0] <- "None"; pos <- x > 0
  if (any(pos)) { qq <- quantile(x[pos], c(1/3, 2/3), names = FALSE); br <- unique(c(-Inf, qq, Inf))
    lab <- c("Low", "Mid", "High")[seq_len(length(br) - 1L)]
    f[pos] <- as.character(cut(x[pos], breaks = br, labels = lab, include.lowest = TRUE))
  }
  factor(f, levels = intersect(c("None", "Low", "Mid", "High"), unique(f)))
}

build_cohort <- function() {
  if (is.na(DATA_PATH) || !file.exists(DATA_PATH)) { log_msg("  cohort SKIP: raw CSV not available."); return(NULL) }
  log_msg(sprintf("  building analytic cohort from %s ...", basename(DATA_PATH)))
  df <- readr::read_csv(DATA_PATH, show_col_types = FALSE)
  .safe_df <- function(cn) if (cn %in% names(df)) .as_num(df[[cn]]) else rep(NA_real_, nrow(df))
  ai_start <- .safe_df("Q37S1_2025"); ds <- df[ai_start %in% c(5L, 6L), , drop = FALSE]
  .safe <- function(cn) if (cn %in% names(ds)) .as_num(ds[[cn]]) else rep(NA_real_, nrow(ds))

  ds$age_2024  <- .safe("AGE_2024"); ds$sex_female <- as.integer(.safe("SEX_2024") == 2L)
  edu <- .safe("Q21.1_2024"); ds$edu_univ <- as.integer(edu %in% 6:8); ds$edu_grad <- as.integer(edu == 9L); ds$edu_below <- as.integer(!(ds$edu_univ == 1L | ds$edu_grad == 1L))
  emp <- .safe("Q5.1_2024")
  ds$emp_exec <- as.integer(emp == 1L); ds$emp_self <- as.integer(emp %in% 2:4); ds$emp_reg <- as.integer(emp %in% 5:6)
  ds$emp_nonreg <- as.integer(emp %in% 7:11); ds$emp_student <- as.integer(emp %in% 12:13); ds$emp_notwork <- as.integer(emp %in% 14:16 | is.na(emp))
  inc <- .safe("Q80.1_2024")
  ds$income_lt_2m <- as.integer(inc %in% 1:4); ds$income_2_6m <- as.integer(inc %in% 5:8); ds$income_6_10m <- as.integer(inc %in% 9:12)
  ds$income_10m_plus <- as.integer(inc %in% 13:18); ds$income_unknown <- as.integer(is.na(inc) | inc %in% c(19L, 20L))
  ds$married <- as.integer(.safe("Q2_2024") %in% 1:3); liv <- .safe("Q1.1_2024"); ds$living_alone <- as.integer(!is.na(liv) & liv == 1L)
  ds$baseline_lsns6_family  <- .row_sum_fn(ds, paste0("Q17.", 1:3, "_2024"), fn = .lsns_rec)
  ds$baseline_lsns6_friends <- .row_sum_fn(ds, paste0("Q17.", 4:6, "_2024"), fn = .lsns_rec)
  ds$baseline_ucla3 <- .row_sum_fn(ds, paste0("Q66.", 1:3, "_2024"), fn = .ucla_rec)
  ds$baseline_k6    <- .row_sum_fn(ds, paste0("Q65.", 1:6, "_2024"), fn = .k6_rec)
  ds$mental_physical_health <- .row_mean(ds, c("Q76.3_2024", "Q76.4_2024"))
  ace_cols <- intersect(paste0("Q77.", c(1:8, 13), "_2024"), names(ds))
  if (length(ace_cols)) {
    ap <- vapply(ds[ace_cols], function(v) as.integer(.as_num(v) == 1L), integer(nrow(ds))); aps <- rowSums(ap, na.rm = TRUE)
    if ("Q77.9_2024" %in% names(ds)) { q9 <- .as_num(ds$Q77.9_2024); a9 <- as.integer(q9 == 2L); a9[is.na(q9)] <- 0L } else a9 <- 0L
    ds$ace_score <- aps + a9
  } else ds$ace_score <- NA_real_
  ds <- .timeuse(ds, "Q28.13_2024", "smartphone"); ds <- .timeuse(ds, "Q28.14_2024", "pc_tablet")
  ds <- .timeuse(ds, "Q28.5_2024", "sitting");     ds <- .timeuse(ds, "Q28.6_2024", "walking")
  .tipi_pair <- function(d, fwd, rev) { a <- .as_num(d[[fwd]]); b <- 8 - .as_num(d[[rev]]); (a + b) / 2 }
  ds$big5_extraversion     <- .tipi_pair(ds, "Q79.1_2024", "Q79.6_2024")
  ds$big5_agreeableness    <- .tipi_pair(ds, "Q79.7_2024", "Q79.2_2024")
  ds$big5_conscientiousness <- .tipi_pair(ds, "Q79.3_2024", "Q79.8_2024")
  ds$big5_neuroticism      <- .tipi_pair(ds, "Q79.4_2024", "Q79.9_2024")
  ds$big5_openness         <- .tipi_pair(ds, "Q79.5_2024", "Q79.10_2024")
  # reference category (0–<1 h/day) = none of the 4 dummies; shown in Table 1 / Sup 4/5
  for (.p in c("smartphone", "pc_tablet", "sitting", "walking"))
    ds[[paste0(.p, "_band_0_1")]] <- as.integer(
      ds[[paste0(.p, "_band_1_2")]] + ds[[paste0(.p, "_band_3_4")]] +
      ds[[paste0(.p, "_band_5plus")]] + ds[[paste0(.p, "_unknown")]] == 0L)

  ds$A_SE <- .row_mean(ds, paste0("Q37S3.", c(8, 9), "_2025")) - 1
  ds$A_PC <- .row_mean(ds, paste0("Q37S3.", c(1, 2, 4, 5), "_2025")) - 1
  ds$A_DI <- .row_mean(ds, paste0("Q37S3.", c(3, 6, 7), "_2025")) - 1
  ds$W_attach  <- .row_mean(ds, paste0("Q40.", 22:24, "_2025"))
  ds$W_anthrop <- .row_mean(ds, paste0("Q40.", 1:3, "_2025"))
  ds$Y_ucla3 <- .row_sum_fn(ds, paste0("Q66.", 1:3, "_2025"), fn = .ucla_rec)
  ds$Y_lsns_friends_2025 <- .row_sum_fn(ds, paste0("Q20.", 4:6, "_2025"), fn = .lsns_rec)
  ds$Y_lsns_family_2025  <- .row_sum_fn(ds, paste0("Q20.", 1:3, "_2025"), fn = .lsns_rec)

  reqd <- c("Y_ucla3","Y_lsns_friends_2025","Y_lsns_family_2025",
            "A_SE","A_PC","A_DI","W_attach","W_anthrop",
            "age_2024","sex_female","baseline_ucla3","baseline_k6",
            "baseline_lsns6_family","baseline_lsns6_friends","mental_physical_health",
            "big5_extraversion","big5_agreeableness","big5_conscientiousness","big5_neuroticism","big5_openness")
  ds <- ds[complete.cases(ds[, intersect(reqd, names(ds)), drop = FALSE]), , drop = FALSE]
  ds$A_SE_cat4 <- make_cat4(ds$A_SE)
  ds$A_PC_cat4 <- make_cat4(ds$A_PC)
  ds$A_DI_cat4 <- make_cat4(ds$A_DI)
  log_msg(sprintf("  analytic cohort n = %d", nrow(ds)))
  ds
}
.COHORT <- build_cohort()

# ============================================================
# Characteristics-table builder (Table 1 / Sup 4 / Sup 5)
# ============================================================
.t1_spec <- list(
  c("n","n","n"),
  c("Age (years), mean (SD)","age_2024","cont"),
  c("Female, n (%)","sex_female","bin"),
  c("Education: below university","edu_below","bin"),
  c("Education: university","edu_univ","bin"),
  c("Education: graduate","edu_grad","bin"),
  c("Employment: regular","emp_reg","bin"),
  c("Employment: non-regular","emp_nonreg","bin"),
  c("Employment: self-employed","emp_self","bin"),
  c("Employment: student","emp_student","bin"),
  c("Employment: not working","emp_notwork","bin"),
  c("Income: <2 m JPY","income_lt_2m","bin"),
  c("Income: 2-6 m JPY","income_2_6m","bin"),
  c("Income: 6-10 m JPY","income_6_10m","bin"),
  c("Income: 10+ m JPY","income_10m_plus","bin"),
  c("Income: unknown","income_unknown","bin"),
  c("Married","married","bin"),
  c("Living alone","living_alone","bin"),
  c("LSNS-6 family subscale (0-15), mean (SD)","baseline_lsns6_family","cont"),
  c("LSNS-6 friends subscale (0-15), mean (SD)","baseline_lsns6_friends","cont"),
  c("Baseline UCLA-3 loneliness (0-9), mean (SD)","baseline_ucla3","cont"),
  c("Baseline K6 distress (0-24), mean (SD)","baseline_k6","cont"),
  c("ACE score (count), mean (SD)","ace_score","cont"),
  c("Mental & physical health composite, mean (SD)","mental_physical_health","cont"),
  c("Extraversion (TIPI-J, 1-7), mean (SD)","big5_extraversion","cont"),
  c("Agreeableness (TIPI-J, 1-7), mean (SD)","big5_agreeableness","cont"),
  c("Conscientiousness (TIPI-J, 1-7), mean (SD)","big5_conscientiousness","cont"),
  c("Neuroticism (TIPI-J, 1-7), mean (SD)","big5_neuroticism","cont"),
  c("Openness (TIPI-J, 1-7), mean (SD)","big5_openness","cont"),
  c("Smartphone: 0–<1 h/day (ref)","smartphone_band_0_1","bin"),
  c("Smartphone: 1-2 h/day","smartphone_band_1_2","bin"),
  c("Smartphone: 3-4 h/day","smartphone_band_3_4","bin"),
  c("Smartphone: 5+ h/day","smartphone_band_5plus","bin"),
  c("Smartphone: unknown","smartphone_unknown","bin"),
  c("PC/tablet: 0–<1 h/day (ref)","pc_tablet_band_0_1","bin"),
  c("PC/tablet: 1-2 h/day","pc_tablet_band_1_2","bin"),
  c("PC/tablet: 3-4 h/day","pc_tablet_band_3_4","bin"),
  c("PC/tablet: 5+ h/day","pc_tablet_band_5plus","bin"),
  c("PC/tablet: unknown","pc_tablet_unknown","bin"),
  c("Sitting: 0–<1 h/day (ref)","sitting_band_0_1","bin"),
  c("Sitting: 1-2 h/day","sitting_band_1_2","bin"),
  c("Sitting: 3-4 h/day","sitting_band_3_4","bin"),
  c("Sitting: 5+ h/day","sitting_band_5plus","bin"),
  c("Sitting: unknown","sitting_unknown","bin"),
  c("Walking: 0–<1 h/day (ref)","walking_band_0_1","bin"),
  c("Walking: 1-2 h/day","walking_band_1_2","bin"),
  c("Walking: 3-4 h/day","walking_band_3_4","bin"),
  c("Walking: 5+ h/day","walking_band_5plus","bin"),
  c("Walking: unknown","walking_unknown","bin"),
  c("A_SE social/emotional use (0-4), mean (SD)","A_SE","cont"),
  c("A_PC productivity/creative use (0-4), mean (SD)","A_PC","cont"),
  c("A_DI daily/information use (0-4), mean (SD)","A_DI","cont"),
  c("W_attach perceived AI-attachment (1-7), mean (SD)","W_attach","cont"),
  c("W_anthrop perceived anthropomorphism (1-7), mean (SD)","W_anthrop","cont"),
  c("Y_ucla3 loneliness @ 2025 (0-9), mean (SD)","Y_ucla3","cont"),
  c("Y_lsns_friends @ 2025 (0-15), mean (SD)","Y_lsns_friends_2025","cont"),
  c("Y_lsns_family  @ 2025 (0-15), mean (SD)","Y_lsns_family_2025","cont"))

build_chars_table <- function(ds, strat_col, output_name, include_total = TRUE) {
  if (is.null(ds)) { log_msg(sprintf("%s SKIP: cohort unavailable.", output_name)); return(invisible(NULL)) }
  if (!strat_col %in% names(ds)) { log_msg(sprintf("%s SKIP: %s not in cohort.", output_name, strat_col)); return(invisible(NULL)) }
  strata <- levels(ds[[strat_col]]); cols <- if (include_total) c("Total", strata) else strata
  cont <- function(x) { x <- x[is.finite(x)]; if (!length(x)) return("—"); sprintf("%.2f (%.2f)", mean(x), sd(x)) }
  bin  <- function(x) { x <- x[!is.na(x)]; if (!length(x)) return("—"); n <- sum(x == 1); sprintf("%d (%.1f%%)", n, 100 * n / length(x)) }
  rows <- lapply(.t1_spec, function(s) {
    lab <- s[1]; ex <- s[2]; ty <- s[3]
    cells <- vapply(cols, function(g) {
      sub <- if (g == "Total") ds else ds[!is.na(ds[[strat_col]]) & ds[[strat_col]] == g, , drop = FALSE]
      if (ty == "n") return(sprintf("%d", nrow(sub)))
      if (!ex %in% names(sub)) return("—")
      if (ty == "cont") cont(sub[[ex]]) else bin(sub[[ex]])
    }, character(1))
    setNames(c(list(lab), as.list(cells)), c("Characteristic", cols))
  })
  out <- do.call(rbind, lapply(rows, as.data.frame, stringsAsFactors = FALSE))
  write_table(out, output_name)
}

log_msg(">>> Table 1: characteristics — Total + by A_SE cat4")
build_chars_table(.COHORT, "A_SE_cat4", "table_1_cohort_by_A_SE", include_total = TRUE)
log_msg(">>> Sup Table 4: characteristics by A_PC cat4")
build_chars_table(.COHORT, "A_PC_cat4", "sup_table_4_cohort_by_A_PC", include_total = FALSE)
log_msg(">>> Sup Table 5: characteristics by A_DI cat4")
build_chars_table(.COHORT, "A_DI_cat4", "sup_table_5_cohort_by_A_DI", include_total = FALSE)

# ============================================================
# Paired-mediator cat4 mediation table (Table 3 / Sup 7 / Sup 8)
# Each row = (Outcome, Contrast); columns = each estimand × Attach + Anthrop (with p).
# ============================================================
build_cat4_paired <- function(focal_exposure, output_name) {
  rows <- list()
  for (oc in names(OUTCOMES)) {
    da <- read_outcome("attach",  oc, "cat4_primary")
    dn <- read_outcome("anthrop", oc, "cat4_primary")
    if (is.null(da) && is.null(dn)) next
    for (con in c("Low vs None","Mid vs None","High vs None")) {
      get_cells <- function(d) {
        empty <- setNames(replicate(length(ESTIMANDS_FULL), list(est = NA_real_, lo = NA_real_, hi = NA_real_, p = NA_real_), simplify = FALSE), ESTIMANDS_FULL)
        if (is.null(d)) return(empty)
        sub <- d[d$exposure == focal_exposure & d$contrast == con, , drop = FALSE]
        out <- lapply(ESTIMANDS_FULL, function(e) {
          r <- sub[sub$estimand == e, ]
          if (!nrow(r)) return(list(est = NA_real_, lo = NA_real_, hi = NA_real_, p = NA_real_))
          list(est = as.numeric(r$Estimate), lo = as.numeric(r[["95% CIL"]]), hi = as.numeric(r[["95% CIU"]]), p = as.numeric(r$P.val))
        })
        names(out) <- ESTIMANDS_FULL; out
      }
      ca <- get_cells(da); cn <- get_cells(dn)
      r <- list(Outcome = unname(OUTCOMES[oc]), Contrast = con)
      for (e in ESTIMANDS_FULL) {
        eu <- toupper(e)
        r[[paste0(eu, "_attach")]]    <- fmt_est_ci(ca[[e]]$est, ca[[e]]$lo, ca[[e]]$hi)
        r[[paste0(eu, "_attach_p")]]  <- fmt_p(ca[[e]]$p)
        r[[paste0(eu, "_anthrop")]]   <- fmt_est_ci(cn[[e]]$est, cn[[e]]$lo, cn[[e]]$hi)
        r[[paste0(eu, "_anthrop_p")]] <- fmt_p(cn[[e]]$p)
      }
      rows[[length(rows) + 1L]] <- as.data.frame(r, stringsAsFactors = FALSE)
    }
  }
  if (!length(rows)) { log_msg(sprintf("%s SKIP: no cat4 inputs", output_name)); return(invisible(NULL)) }
  write_table(do.call(rbind, rows), output_name)
}
log_msg(">>> Table 3: PRIMARY cat4 A_SE × W_attach + W_anthrop")
build_cat4_paired("A_SE_cat4", "table_3_primary_cat4_A_SE")
log_msg(">>> Sup Table 7: cat4 A_PC × W_attach + W_anthrop")
build_cat4_paired("A_PC_cat4", "sup_table_7_cat4_A_PC")
log_msg(">>> Sup Table 8: cat4 A_DI × W_attach + W_anthrop")
build_cat4_paired("A_DI_cat4", "sup_table_8_cat4_A_DI")

# ============================================================
# Table 2 — PRIMARY multiplicity: cat4 omnibus joint indirect-effect test,
# BH-FDR across the 6 primary cells (2 candidate mediators × 3 outcomes, A_SE focal).
# Reads each outcome script's §1b output (`<construct>_omnibus_primary.csv`). The
# cat4 Low/Mid/High dose contrasts in Table 3 are descriptive, NOT separate tests
# (codebook §7.7); the family is 6, BH-corrected here.
# ============================================================
log_msg(">>> Table 2: primary omnibus + BH-FDR over 6 (2 mediators × 3 outcomes)")
build_primary_omnibus_bh <- function() {
  rows <- list()
  for (oc in names(OUTCOMES)) {
    for (construct in c("attach", "anthrop")) {
      d <- read_outcome(construct, oc, "omnibus_primary"); if (is.null(d)) next
      d$Outcome  <- unname(OUTCOMES[oc])
      d$Mediator <- if (construct == "attach") "W_attach (AI-attachment)" else "W_anthrop (anthropomorphism)"
      rows[[length(rows) + 1L]] <- d
    }
  }
  if (!length(rows)) { log_msg("Table 2 SKIP: omnibus_primary CSVs missing."); return(invisible(NULL)) }
  out <- dplyr::bind_rows(rows)
  out$q_BH         <- p.adjust(out$p_omnibus, method = "BH")
  out$survives_q05 <- ifelse(is.finite(out$q_BH) & out$q_BH < 0.05, "yes", "no")
  out$p_omnibus_fmt <- vapply(out$p_omnibus, fmt_p, character(1))
  out$q_BH_fmt      <- vapply(out$q_BH, fmt_p, character(1))
  keep <- intersect(c("Outcome","Mediator","exposure","k_contrasts","wald_chisq","df",
                      "p_omnibus","p_omnibus_fmt","q_BH","q_BH_fmt","survives_q05","n_boot_valid","R"), names(out))
  write_table(out[, keep, drop = FALSE], "table_2_primary_omnibus_bh")
  for (i in seq_len(nrow(out))) log_msg(sprintf("  [%s | %s] chi2=%.3f p=%s q_BH=%s (survives: %s)",
    out$Mediator[i], out$Outcome[i], out$wald_chisq[i], out$p_omnibus_fmt[i], out$q_BH_fmt[i], out$survives_q05[i]))
}
build_primary_omnibus_bh()

# ============================================================
# Sup Table 9 — S1 tertile-mediator sensitivity (A_SE cat4 × W_attach_tert / W_anthrop_tert)
# Long format: Outcome × Mediator × Contrast × Estimand × est_ci × p
# ============================================================
log_msg(">>> Sup Table 9 (S1): tertile mediator sens, A_SE × 3 outcomes")
build_sup9_tertile <- function() {
  rows <- list()
  for (oc in names(OUTCOMES)) {
    for (med in c("W_attach (tertile)", "W_anthrop (tertile)")) {
      construct <- if (startsWith(med, "W_attach")) "attach" else "anthrop"
      d <- read_outcome(construct, oc, "tertile_sens"); if (is.null(d)) next
      d <- d[d$exposure == "A_SE_cat4" & d$estimand %in% c("tnie","intmed","pnie","intref","cde","te"), , drop = FALSE]
      if (!nrow(d)) next
      d$Outcome <- unname(OUTCOMES[oc]); d$Mediator <- med
      d$est_ci <- mapply(fmt_est_ci, d$Estimate, d[["95% CIL"]], d[["95% CIU"]])
      d$p <- vapply(d$P.val, fmt_p, character(1))
      rows[[length(rows) + 1L]] <- d[, c("Outcome","Mediator","contrast","estimand","est_ci","p"), drop = FALSE]
    }
  }
  if (!length(rows)) { log_msg("Sup 9 SKIP (tertile)."); return(invisible(NULL)) }
  write_table(do.call(rbind, rows), "sup_table_9_tertile_mediator_S1")
}
build_sup9_tertile()

# ============================================================
# Sup Table 10 — S2 binary sensitivity (`any vs none`) × 3 purposes × both mediators
# ============================================================
log_msg(">>> Sup Table 10 (S2): binary (any vs none) × 3 purposes × both mediators")
build_sup10_binary <- function() {
  rows <- list()
  for (oc in names(OUTCOMES)) {
    for (construct in c("attach","anthrop")) {
      d <- read_outcome(construct, oc, "binary_sens"); if (is.null(d)) next
      d <- d[d$estimand %in% c("tnie","intmed","pnie","intref","cde","te"), , drop = FALSE]
      if (!nrow(d)) next
      d$Outcome <- unname(OUTCOMES[oc]); d$Mediator <- if (construct == "attach") "W_attach" else "W_anthrop"
      d$est_ci <- mapply(fmt_est_ci, d$Estimate, d[["95% CIL"]], d[["95% CIU"]])
      d$p <- vapply(d$P.val, fmt_p, character(1))
      rows[[length(rows) + 1L]] <- d[, c("Outcome","Mediator","exposure","estimand","est_ci","p"), drop = FALSE]
    }
  }
  if (!length(rows)) { log_msg("Sup 10 SKIP (binary)."); return(invisible(NULL)) }
  write_table(do.call(rbind, rows), "sup_table_10_binary_3purposes_S2")
}
build_sup10_binary()

# ============================================================
# Sup Table 11 — S3 continuous (∓0.5 SD) sensitivity × 3 purposes × both mediators
# Note: closed-form CSVs use lowercase column names (estimate/ci_lo/ci_hi/p).
# ============================================================
log_msg(">>> Sup Table 11 (S3): continuous (∓0.5 SD) × 3 purposes × both mediators")
build_sup11_continuous <- function() {
  rows <- list()
  for (oc in names(OUTCOMES)) {
    for (construct in c("attach","anthrop")) {
      d <- read_outcome(construct, oc, "continuous_sens"); if (is.null(d)) next
      d <- d[d$estimand %in% ESTIMANDS_CONT, , drop = FALSE]; if (!nrow(d)) next
      d$Outcome <- unname(OUTCOMES[oc]); d$Mediator <- if (construct == "attach") "W_attach" else "W_anthrop"
      d$est_ci <- mapply(fmt_est_ci, d$estimate, d$ci_lo, d$ci_hi)
      d$p <- vapply(d$p, fmt_p, character(1))
      rows[[length(rows) + 1L]] <- d[, c("Outcome","Mediator","focal","estimand","est_ci","p","R"), drop = FALSE]
    }
  }
  if (!length(rows)) { log_msg("Sup 11 SKIP (continuous)."); return(invisible(NULL)) }
  write_table(do.call(rbind, rows), "sup_table_11_continuous_3purposes_S3")
}
build_sup11_continuous()

# ============================================================
# Sup Table 13 — S5 per-item sensitivity (A_SE cat4 × 6 items × 3 outcomes)
# (the S6 two-item attachment composite row of the same published table is written by
#  build_sup14_robustness() below, from the ai_robustness.R output)
# Reads each outcome script's §5 output (`<construct>_per_item_cat4.csv` in each
# per-outcome folder) and aggregates with the Outcome column added during merge.
# ============================================================
log_msg(">>> Sup Table 13 (S5): per-item cat4 A_SE × 3 outcomes × 6 items")
build_sup13_per_item <- function() {
  rows <- list()
  for (oc in names(OUTCOMES)) {
    for (construct in c("attach","anthrop")) {
      d <- read_outcome(construct, oc, "per_item_cat4"); if (is.null(d)) next
      d <- d[d$estimand %in% c("tnie","intmed","pnie","intref","cde","te"), , drop = FALSE]
      if (!nrow(d)) next
      d$Outcome <- unname(OUTCOMES[oc])
      d$Mediator_construct <- if (construct == "attach") "W_attach (per item)" else "W_anthrop (per item)"
      d$est_ci <- mapply(fmt_est_ci, d$Estimate, d[["95% CIL"]], d[["95% CIU"]])
      d$p <- vapply(d$P.val, fmt_p, character(1))
      rows[[length(rows) + 1L]] <- d[, c("Outcome","Mediator_construct","mediator_item","mediator_label","contrast","estimand","est_ci","p"), drop = FALSE]
    }
  }
  if (!length(rows)) { log_msg("Sup 13 SKIP: per-item CSVs missing."); return(invisible(NULL)) }
  write_table(do.call(rbind, rows), "sup_table_13_per_item_cat4_A_SE_S5")
}
build_sup13_per_item()

# ============================================================
# Sup Table 12 — S4 joint-mediator interventional sensitivity
# Reads §6 of the 3 W_attach OUTCOME scripts (`attach_interventional.csv` in each of the
# 3 attach outcome folders). Randomized interventional analogue r-effects, A_SE cat4 ×
# each outcome, two symmetric configs (attach net-of-anthrop; anthrop net-of-attach).
# r-prefixed effects passed through verbatim — NOT natural effects. Exploratory; outside §7.7.
# ============================================================
log_msg(">>> Sup Table 12 (S4): joint-mediator interventional (outcome scripts §6) — A_SE cat4 × 3 outcomes, both configs")
build_sup12_interventional <- function() {
  rows <- list()
  for (oc in names(OUTCOMES)) {
    d <- read_outcome("attach", oc, "interventional"); if (is.null(d)) next
    d <- d[d$estimand %in% c("rtnie","rpnie","rintmed","rpnde","cde","te"), , drop = FALSE]
    if (!nrow(d)) next
    d$Outcome <- unname(OUTCOMES[oc])
    d$Config  <- ifelse(d$config == "attach_net_of_anthrop",
                        "Attachment (net of anthropomorphism)", "Anthropomorphism (net of attachment)")
    d$est_ci <- mapply(fmt_est_ci, d$Estimate, d[["95% CIL"]], d[["95% CIU"]])
    d$p <- vapply(d$P.val, fmt_p, character(1))
    rows[[length(rows) + 1L]] <- d[, c("Outcome","Config","mediator","postc","contrast","estimand","est_ci","p"), drop = FALSE]
  }
  if (!length(rows)) { log_msg("Sup 12 SKIP: attach_interventional.csv missing (run §6 of the 3 attach scripts)."); return(invisible(NULL)) }
  write_table(do.call(rbind, rows), "sup_table_12_interventional_S4")
}
build_sup12_interventional()

# ============================================================
# Sup Tables 2 & 3 — EFA of the used items.
# Sup 2 = AI-use purposes (9 Q37S3 items); Sup 3 = the 2 mediators (6 Q40 items).
# Each emits: oblique loadings (+ per-item h2/u2/complexity/MSA) + factor-correlation
# matrix + a `_fit.csv` with KMO / Bartlett / RMSEA / TLI / RMSR / BIC / model χ²
# (Sup 3's attach↔anthrop factor cor ≈ 0.72 is the empirical basis for the parallel
# single-mediator design, codebook §3). Pure passthrough of the diagnosis CSVs.
# ============================================================
build_efa_sup <- function(stem, tab_loadings, tab_factorcor, label) {
  load_d <- read_diag(paste0(stem, "_loadings"))
  if (is.null(load_d)) { log_msg(sprintf("%s SKIP: %s_loadings.csv missing (run diagnosis_mediation.R on real).", label, stem)); return(invisible(NULL)) }
  write_table(load_d, tab_loadings)                                              # loadings + h2/u2/complexity/MSA_item
  fcor_d <- read_diag(paste0(stem, "_factor_cor")); if (!is.null(fcor_d)) write_table(fcor_d, tab_factorcor)
  fit_d  <- read_diag(paste0(stem, "_fit"));        if (!is.null(fit_d))  write_table(fit_d,  paste0(tab_loadings, "_fit"))   # KMO/Bartlett/RMSEA/TLI/RMSR/BIC
  var_d  <- read_diag(paste0(stem, "_variance"))
  if (!is.null(var_d) && "cum_var" %in% names(var_d)) log_msg(sprintf("  %s cumulative variance = %.1f%%", label, 100 * max(var_d$cum_var, na.rm = TRUE)))
}
log_msg(">>> Sup Table 2: EFA — AI-use purposes (9 items)")
build_efa_sup("08_efa_purposes", "sup_table_2_efa_purposes", "sup_table_2_efa_purposes_factor_cor", "Sup 2 (purposes EFA)")
log_msg(">>> Sup Table 3: EFA — mediators (6 items: attach + anthrop)")
build_efa_sup("09_efa_mediators", "sup_table_3_efa_mediators", "sup_table_3_efa_mediators_factor_cor", "Sup 3 (mediators EFA)")

# ============================================================
# Revision-stage inputs: ai_measurement_validity.R and ai_robustness.R outputs
# ============================================================
VALID_DIR <- .norm(file.path(.proj_root, "output", "ai_mod", paste0(.tag, "validity"), "tables"))
ROBUST_DIR <- .norm(file.path(.proj_root, "output", "ai_mod", paste0(.tag, "robustness"), "tables"))
read_dir <- function(dir, name) {
  fp <- file.path(dir, paste0(name, ".csv"))
  if (!file.exists(fp)) { log_msg(sprintf("  missing: %s", fp)); return(NULL) }
  readr::read_csv(fp, show_col_types = FALSE)
}

# ---- Table 2b — follow-up (2025) mediators and outcomes by A_SE cat4 (passthrough) ----
log_msg(">>> Table 2b: follow-up (2025) mediators/outcomes by A_SE cat4")
{ d <- read_dir(VALID_DIR, "followup_by_A_SE"); if (!is.null(d)) write_table(d, "table_2b_followup_by_A_SE") else log_msg("Table 2b SKIP (run ai_measurement_validity.R).") }

# ---- Table 3b — SD-standardized TNIE for the primary cells (TNIE ÷ SD of the 2025 outcome) ----
log_msg(">>> Table 3b: standardized TNIE (per SD of the 2025 outcome)")
build_table3b <- function() {
  sdd <- read_dir(VALID_DIR, "outcome_sd"); if (is.null(sdd)) { log_msg("Table 3b SKIP: outcome_sd.csv missing."); return(invisible(NULL)) }
  rows <- list()
  for (oc in names(OUTCOMES)) {
    sd_y <- sdd$sd_2025[sdd$outcome == OUTCOME_VARS[[oc]]]; if (!length(sd_y)) next
    for (construct in c("attach", "anthrop")) {
      d <- read_outcome(construct, oc, "cat4_primary"); if (is.null(d)) next
      d <- d[d$exposure == "A_SE_cat4" & d$estimand %in% c("tnie", "intmed", "te"), , drop = FALSE]
      for (i in seq_len(nrow(d))) rows[[length(rows) + 1L]] <- data.frame(
        Outcome = unname(OUTCOMES[oc]), Mediator = if (construct == "attach") "W_attach (AI-attachment)" else "W_anthrop (anthropomorphism)",
        Contrast = d$contrast[i], Estimand = toupper(d$estimand[i]),
        estimate = as.numeric(d$Estimate[i]), ci_lo = as.numeric(d[["95% CIL"]][i]), ci_hi = as.numeric(d[["95% CIU"]][i]),
        sd_outcome_2025 = sd_y,
        std_estimate = as.numeric(d$Estimate[i]) / sd_y, std_ci_lo = as.numeric(d[["95% CIL"]][i]) / sd_y, std_ci_hi = as.numeric(d[["95% CIU"]][i]) / sd_y,
        est_ci = fmt_est_ci(d$Estimate[i], d[["95% CIL"]][i], d[["95% CIU"]][i]),
        std_est_ci = fmt_est_ci(as.numeric(d$Estimate[i]) / sd_y, as.numeric(d[["95% CIL"]][i]) / sd_y, as.numeric(d[["95% CIU"]][i]) / sd_y),
        p = fmt_p(as.numeric(d$P.val[i])), stringsAsFactors = FALSE)
    }
  }
  if (!length(rows)) { log_msg("Table 3b SKIP: no cat4 inputs."); return(invisible(NULL)) }
  write_table(do.call(rbind, rows), "table_3b_standardized_effects")
}
build_table3b()

# ---- Sup Tables 13 (S6 row) and 14 — robustness of the six primary cells ----
# ai_robustness.R writes REF and S6–S9 together; in the published Supplementary Data the S6
# two-item attachment rows sit in Supplementary Table 13 (with the S5 per-item results) and
# REF + S7–S9 form Supplementary Table 14.
log_msg(">>> Sup Table 14: robustness specifications (REF, S7–S9) + Sup Table 13 S6 row")
build_sup14_robustness <- function() {
  cells <- read_dir(ROBUST_DIR, "robustness_cells"); joint <- read_dir(ROBUST_DIR, "robustness_joint"); specs <- read_dir(ROBUST_DIR, "robustness_specs")
  if (is.null(cells) || is.null(joint)) { log_msg("Sup 13 (S6) / Sup 14 SKIP (run ai_robustness.R)."); return(invisible(NULL)) }
  med_lab <- c(W_attach_c = "W_attach (AI-attachment)", W_anthrop_c = "W_anthrop (anthropomorphism)", W_attach2_c = "W_attach, 2 items (Q40.22 + Q40.24)")
  out_lab <- setNames(unname(OUTCOMES), unname(OUTCOME_VARS))
  rows <- list()
  for (i in seq_len(nrow(joint))) {
    j <- joint[i, ]
    sub <- cells[cells$spec == j$spec & cells$mediator == j$mediator & cells$outcome == j$outcome, , drop = FALSE]
    r <- list(Specification = if (!is.null(specs)) specs$label[match(j$spec, specs$spec)] else j$spec, spec = j$spec,
              Mediator = unname(med_lab[j$mediator]), Outcome = unname(out_lab[j$outcome]), n = j$n)
    for (con in c("Low vs None", "Mid vs None", "High vs None")) for (e in c("TNIE", "INT_med", "TE")) {
      c1 <- sub[sub$contrast == con & sub$estimand == e, , drop = FALSE]
      r[[paste0(e, "_", sub(" vs None", "", con))]]   <- if (nrow(c1)) fmt_est_ci(c1$estimate, c1$ci_lo, c1$ci_hi) else "—"
      r[[paste0(e, "_", sub(" vs None", "", con), "_p")]] <- if (nrow(c1)) fmt_p(c1$p) else "—"
    }
    r$wald_chisq <- round(j$wald_chisq, 2); r$df <- j$df; r$p_joint <- fmt_p(j$p_joint)
    rows[[length(rows) + 1L]] <- as.data.frame(r, stringsAsFactors = FALSE, check.names = FALSE)
  }
  out <- do.call(rbind, rows)
  is_s6 <- out$spec == "S6"
  if (any(is_s6))  write_table(out[is_s6, , drop = FALSE],  "sup_table_13_two_item_attachment_S6")
  write_table(out[!is_s6, , drop = FALSE], "sup_table_14_robustness_S7_S9")
  for (nm in c("robustness_specs", "timing_baseline_month", "ipw_weights_summary", "ipw_attrition_model")) { d <- read_dir(ROBUST_DIR, nm); if (!is.null(d)) write_table(d, paste0("sup_table_14_", nm)) }
}
build_sup14_robustness()

# ---- Sup Table 15 — construct distinctness (correlations, reliability, HTMT) ----
log_msg(">>> Sup Table 15: correlations / reliability / HTMT")
for (nm in c("correlations_r", "correlations_p", "attachment_items_vs_loneliness", "reliability", "htmt")) { d <- read_dir(VALID_DIR, nm); if (!is.null(d)) write_table(d, paste0("sup_table_15_", nm)) }

# ---- Sup Tables 16 & 17 — EFAs (passthrough incl. parallel-analysis solutions when written) ----
log_msg(">>> Sup Tables 16-17: EFA attachment + UCLA-3; attachment + anthropomorphism + problematic use")
for (st in list(c("efa_attach_ucla", "sup_table_16"), c("efa_attach_anthrop_pcus", "sup_table_17"))) {
  for (suffix in c("loadings", "factor_cor", "fit", "variance", "parallel", "pa_loadings", "pa_factor_cor", "pa_fit")) {
    d <- read_dir(VALID_DIR, paste0(st[1], "_", suffix)); if (!is.null(d)) write_table(d, paste0(st[2], "_", st[1], "_", suffix))
  }
}

# ---- Sup Table 6 — baseline predictors of 2025 social/emotional use and of the two perceptions ----
log_msg(">>> Sup Table 6: selection models")
{ d <- read_dir(VALID_DIR, "selection_models")
  if (!is.null(d)) { d$est_ci <- mapply(fmt_est_ci, d$estimate, d$ci_lo, d$ci_hi); d$p_fmt <- vapply(d$p, fmt_p, character(1)); write_table(d, "sup_table_6_selection_models") } }

# ---- Sup Table 1 — baseline characteristics by generative-AI status at 2025 ----
log_msg(">>> Sup Table 1: baseline characteristics by AI status")
{ d <- read_dir(VALID_DIR, "ai_status_groups_baseline"); if (!is.null(d)) write_table(d, "sup_table_1_ai_status_groups") }

# ============================================================
# Figure 1 — Conceptual DAG placeholder (MD; drawn manually)
# ============================================================
log_msg(">>> Figure 1: conceptual DAG placeholder MD")
writeLines(c(
  "# Figure 1 — Conceptual framework (DAG, to be drawn manually)",
  "",
  "Nodes / arrows for the single-mediator decomposition:",
  "",
  "  A (categorical cat4: None / Low / Mid / High)",
  "          |                  +-> M = W_attach  (perceived AI-attachment)  -+",
  "          |  (a-path x b)    |                                              |",
  "          |                  +-> M = W_anthrop (perceived anthropomorphism) +",
  "          |                                                                 |",
  "          +--- direct path -----------------------------------------------> Y",
  "",
  "  Each mediator is analyzed in its own single-mediator pipeline (not combined,",
  "  because r(W_attach, W_anthrop) ~ 0.65 destabilizes joint identifiability).",
  "",
  "Two outcome domains:",
  "  Y_subjective = UCLA-3 loneliness @ 2025                       (felt isolation)",
  "  Y_behavioral = LSNS-friends 2025 / LSNS-family 2025           (objective network",
  "                                                                  displacement vs",
  "                                                                  baseline 2024 LSNS",
  "                                                                  in C - Kraut behavioral test)",
  "",
  "Confounders: 44 baseline-2024 covariates (incl. baseline_ucla3, baseline_lsns_*, Big Five).",
  "Mediated interaction (A x M) is allowed: INT_med carries the indirect effect."),
  con = file.path(OUT_FIG, "figure_1_DAG_notes.md"))
log_msg("  -> figure_1_DAG_notes.md")

# Figure 2 (paired TNIE forest) removed — its content is fully contained in Table 3
# (the paired W_attach vs W_anthrop cat4 decomposition); the manuscript cites no figures here.

# ============================================================
# Sup Figure 1 — Sample flow chart (ggplot box-and-arrow)
# ============================================================
log_msg(">>> Sup Figure 1: sample flow chart")
build_sup_fig1 <- function() {
  sf <- read_outcome("attach", "LON", "sample_flow")
  if (is.null(sf)) { log_msg("Sup Fig 1 SKIP: attach_sample_flow.csv not found."); return(invisible(NULL)) }
  step_lab <- c(
    raw                    = "JACSIS 2024 + 2025 panel\n(raw)",
    cohort_initiators_5_6  = "2025 AI initiators\n(Q37S1_2025 in {5, 6})",
    analytic_complete_case = "Analytic complete-case\n(44 C + A + M + Y)"
  )
  steps <- intersect(names(step_lab), sf$step)
  if (length(steps) < 2L) { log_msg("Sup Fig 1 SKIP: need at least 2 sample-flow steps."); return(invisible(NULL)) }
  ns <- setNames(sf$n[match(steps, sf$step)], steps)
  df <- data.frame(
    step  = factor(steps, levels = steps),
    label = sprintf("%s\nn = %s", unname(step_lab[steps]), format(ns, big.mark = ",")),
    y     = rev(seq_along(steps)),
    stringsAsFactors = FALSE)
  excl <- if (length(steps) >= 2L) data.frame(
    y_mid = (df$y[-nrow(df)] + df$y[-1]) / 2,
    text  = vapply(seq_len(nrow(df) - 1L),
                   function(i) sprintf("Excluded: n = %s", format(ns[i] - ns[i + 1L], big.mark = ",")),
                   character(1)),
    stringsAsFactors = FALSE) else NULL
  p <- ggplot(df, aes(x = 1, y = y)) +
    geom_tile(width = 2.0, height = 0.7, fill = "grey95", colour = "grey25") +
    geom_text(aes(label = label), size = 3.6, lineheight = 1.0) +
    { if (!is.null(excl)) geom_segment(data = df[-nrow(df), ], aes(x = 1, xend = 1, y = y - 0.35, yend = y - 0.65),
                                       arrow = grid::arrow(length = grid::unit(0.18, "cm"), type = "closed"),
                                       inherit.aes = FALSE) } +
    { if (!is.null(excl)) geom_text(data = excl, aes(x = 2.2, y = y_mid, label = text),
                                    hjust = 0, size = 3.2, colour = "grey25", inherit.aes = FALSE) } +
    scale_x_continuous(limits = c(-0.1, 4.5), expand = c(0, 0)) +
    scale_y_continuous(limits = c(0.3, nrow(df) + 0.7), expand = c(0, 0)) +
    labs(title = "Sup Figure 1 - Sample flow", x = NULL, y = NULL) +
    theme_void(base_size = 11) + theme(plot.title = element_text(face = "bold", hjust = 0.5))
  for (ext in c("png","pdf")) ggsave(file.path(OUT_FIG, sprintf("sup_figure_1_flow_chart.%s", ext)), p, width = 7, height = 5, dpi = 200)
  log_msg("  -> sup_figure_1_flow_chart.{png,pdf}")
}
build_sup_fig1()

log_msg("=== END (packager) ===")
