# ============================================================
# ai_measurement_validity.R
# Descriptive and measurement analyses for the mediation paper:
#   §1  Follow-up (2025) descriptives — mediators and outcomes by social/emotional use category,
#       with change from the 2024 baseline for the three outcomes; SDs used to standardize effects
#   §2  Construct distinctness — correlations, Cronbach's alpha, and HTMT ratios among perceived
#       AI-attachment, perceived anthropomorphism, UCLA-3 loneliness (2025 and 2024), and
#       problematic generative-AI use (Q38.1-11; 2025)
#   §3  EFA (a) attachment items + UCLA-3 items (2025): are attachment and loneliness separable?
#       EFA (b) attachment + anthropomorphism + problematic-use items: is attachment distinct from
#       problematic/dependent use?  (MINRES, oblimin, Horn's parallel analysis — as in
#       diagnosis_mediation.R)
#   §4  Selection into social/emotional use and into attachment — associations of baseline (2024)
#       loneliness, distress, and network size with 2025 social/emotional use (any use; intensity)
#       and with 2025 perceived AI-attachment, adjusted for the other baseline covariates
#   §5  Baseline (2024) characteristics of the two-wave respondents by generative-AI status at 2025
#       (never used; used before but not now; 2022-23 initiators; 2024 initiators; 2025 initiators
#       = analytic cohort), for judging the generalizability of the new-user cohort
# Cohort recodes are identical to the outcome scripts (44 baseline covariates).
# Outputs: output/ai_mod/[dummy/]validity/tables/
# LLM never reads real data. Aggregated CSVs / logs are OK to read.
# ============================================================

suppressPackageStartupMessages({ library(here); library(readr); library(psych) })
set.seed(20260524)

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

# ---- Output tree ----
OUT_ROOT <- file.path(.proj_root, "output", "ai_mod", if (RUN_MODE == "real") "validity" else file.path("dummy", "validity"))
TABLES_DIR <- file.path(OUT_ROOT, "tables"); LOGS_DIR <- file.path(OUT_ROOT, "logs")
for (d in c(TABLES_DIR, LOGS_DIR)) dir.create(d, recursive = TRUE, showWarnings = FALSE)
.timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
LOG_FILE   <- file.path(LOGS_DIR, sprintf("run_validity_%s_%s.log", RUN_MODE, .timestamp))
log_msg     <- function(...) { m <- paste0(format(Sys.time(), "[%H:%M:%S] "), paste(..., collapse = " ")); cat(m, "\n", sep = ""); cat(m, "\n", sep = "", file = LOG_FILE, append = TRUE); invisible(m) }
write_table <- function(x, name) { fp <- file.path(TABLES_DIR, paste0(name, ".csv")); utils::write.csv(x, fp, row.names = FALSE, fileEncoding = "UTF-8"); log_msg("wrote:", fp) }
log_msg(sprintf("=== START === validity mode=%s input=%s", RUN_MODE, DATA_PATH))

# ============================================================
# §0  Build (identical recodes to the outcome scripts)
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
  d[[paste0(p, "_band_0_1")]]   <- as.integer(v %in% c(1:3))
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

df <- readr::read_csv(DATA_PATH, show_col_types = FALSE)
log_msg(sprintf("Loaded: %d rows x %d cols", nrow(df), ncol(df)))
.safe <- function(cn) if (cn %in% names(df)) .as_num(df[[cn]]) else rep(NA_real_, nrow(df))

# baseline covariates (44)
df$baseline_ucla3 <- .row_sum_fn(df, paste0("Q66.", 1:3, "_2024"), fn = .ucla_rec)
df$baseline_k6    <- .row_sum_fn(df, paste0("Q65.", 1:6, "_2024"), fn = .k6_rec)
ace_cols <- intersect(paste0("Q77.", c(1:8, 13), "_2024"), names(df))
if (length(ace_cols)) {
  ap  <- vapply(df[ace_cols], function(v) as.integer(.as_num(v) == 1L), integer(nrow(df))); aps <- rowSums(ap, na.rm = TRUE)
  if ("Q77.9_2024" %in% names(df)) { q9 <- .as_num(df$Q77.9_2024); a9 <- as.integer(q9 == 2L); a9[is.na(q9)] <- 0L } else a9 <- 0L
  df$ace_score <- aps + a9
} else df$ace_score <- NA_real_
df$ace_1 <- as.integer(df$ace_score == 1L); df$ace_2_3 <- as.integer(df$ace_score %in% 2:3); df$ace_4plus <- as.integer(df$ace_score >= 4L)
df$age_2024  <- .safe("AGE_2024"); df$sex_female <- as.integer(.safe("SEX_2024") == 2L)
edu <- .safe("Q21.1_2024"); df$edu_univ <- as.integer(edu %in% 6:8); df$edu_grad <- as.integer(edu == 9L); df$edu_below <- as.integer(!(df$edu_univ == 1L | df$edu_grad == 1L))
emp <- .safe("Q5.1_2024")
df$emp_exec <- as.integer(emp == 1L); df$emp_self <- as.integer(emp %in% 2:4); df$emp_reg <- as.integer(emp %in% 5:6)
df$emp_nonreg <- as.integer(emp %in% 7:11); df$emp_student <- as.integer(emp %in% 12:13); df$emp_notwork <- as.integer(emp %in% 14:16 | is.na(emp))
inc <- .safe("Q80.1_2024")
df$income_lt_2m <- as.integer(inc %in% 1:4); df$income_2_6m <- as.integer(inc %in% 5:8); df$income_6_10m <- as.integer(inc %in% 9:12)
df$income_10m_plus <- as.integer(inc %in% 13:18); df$income_unknown <- as.integer(is.na(inc) | inc %in% c(19L, 20L))
df$married <- as.integer(.safe("Q2_2024") %in% 1:3); liv <- .safe("Q1.1_2024"); df$living_alone <- as.integer(!is.na(liv) & liv == 1L)
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
C_VARS <- c(
  "age_2024", "sex_female", "edu_univ", "edu_grad",
  "emp_exec", "emp_self", "emp_nonreg", "emp_student", "emp_notwork",
  "income_2_6m", "income_6_10m", "income_10m_plus", "income_unknown", "married", "living_alone",
  "baseline_lsns6_family", "baseline_lsns6_friends", "baseline_ucla3", "baseline_k6",
  "ace_1", "ace_2_3", "ace_4plus", "mental_physical_health",
  "smartphone_band_1_2", "smartphone_band_3_4", "smartphone_band_5plus", "smartphone_unknown",
  "pc_tablet_band_1_2", "pc_tablet_band_3_4", "pc_tablet_band_5plus", "pc_tablet_unknown",
  "sitting_band_1_2", "sitting_band_3_4", "sitting_band_5plus", "sitting_unknown",
  "walking_band_1_2", "walking_band_3_4", "walking_band_5plus", "walking_unknown",
  "big5_extraversion", "big5_agreeableness", "big5_conscientiousness", "big5_neuroticism", "big5_openness")
stopifnot(length(C_VARS) == 44L)

# 2025 exposures, mediators, outcomes, problematic use
A_SPEC <- list(A_SE = paste0("Q37S3.", c(8, 9), "_2025"), A_PC = paste0("Q37S3.", c(1, 2, 4, 5), "_2025"), A_DI = paste0("Q37S3.", c(3, 6, 7), "_2025"))
ATTACH_ITEMS  <- paste0("Q40.", 22:24, "_2025")
ANTHROP_ITEMS <- paste0("Q40.", 1:3, "_2025")
UCLA25_ITEMS  <- paste0("Q66.", 1:3, "_2025")
UCLA24_ITEMS  <- paste0("Q66.", 1:3, "_2024")
PCUS_ITEMS    <- paste0("Q38.", 1:11, "_2025")
for (a in names(A_SPEC)) df[[a]] <- .row_mean(df, A_SPEC[[a]], na_rm = FALSE) - 1
df$W_attach  <- .row_mean(df, ATTACH_ITEMS); df$W_anthrop <- .row_mean(df, ANTHROP_ITEMS)
df$W_attach2 <- .row_mean(df, paste0("Q40.", c(22, 24), "_2025"))
df$PCUS      <- .row_mean(df, PCUS_ITEMS)
df$Y_ucla3             <- .row_sum_fn(df, UCLA25_ITEMS, fn = .ucla_rec)
df$Y_lsns_friends_2025 <- .row_sum_fn(df, paste0("Q20.", 4:6, "_2025"), fn = .lsns_rec)
df$Y_lsns_family_2025  <- .row_sum_fn(df, paste0("Q20.", 1:3, "_2025"), fn = .lsns_rec)
for (it in UCLA25_ITEMS) df[[paste0(it, "_rec")]] <- .ucla_rec(.as_num(df[[it]]))   # item-level, higher = lonelier
df$ai_start <- .safe("Q37S1_2025")

ds <- df[df$ai_start %in% c(5L, 6L), , drop = FALSE]
required <- unique(c("Y_ucla3", "Y_lsns_friends_2025", "Y_lsns_family_2025", names(A_SPEC), "W_attach", "W_anthrop", C_VARS))
dm <- ds[complete.cases(ds[, required, drop = FALSE]), , drop = FALSE]
log_msg(sprintf("Cohort n = %d; analytic n = %d", nrow(ds), nrow(dm)))
.run_ok <- nrow(dm) >= 50L
dm$A_SE_cat4 <- make_cat4(dm$A_SE)
dm$A_SE_any  <- as.integer(dm$A_SE > 0)

# ============================================================
# §1  Follow-up (2025) descriptives by social/emotional use category
# ============================================================
if (.run_ok) {
  dm$change_ucla3   <- dm$Y_ucla3 - dm$baseline_ucla3
  dm$change_friends <- dm$Y_lsns_friends_2025 - dm$baseline_lsns6_friends
  dm$change_family  <- dm$Y_lsns_family_2025  - dm$baseline_lsns6_family
  vars1 <- c(W_attach = "Perceived AI-attachment (1-7), 2025", W_anthrop = "Perceived anthropomorphism (1-7), 2025",
             Y_ucla3 = "UCLA-3 loneliness (0-9), 2025", baseline_ucla3 = "UCLA-3 loneliness (0-9), 2024",
             change_ucla3 = "Change in UCLA-3 (2025 - 2024)",
             Y_lsns_friends_2025 = "LSNS-6 friends (0-15), 2025", baseline_lsns6_friends = "LSNS-6 friends (0-15), 2024",
             change_friends = "Change in LSNS-6 friends (2025 - 2024)",
             Y_lsns_family_2025 = "LSNS-6 family (0-15), 2025", baseline_lsns6_family = "LSNS-6 family (0-15), 2024",
             change_family = "Change in LSNS-6 family (2025 - 2024)")
  groups <- c("Total", levels(dm$A_SE_cat4))
  rows <- lapply(names(vars1), function(v) {
    cells <- vapply(groups, function(g) { x <- if (g == "Total") dm[[v]] else dm[[v]][dm$A_SE_cat4 == g]; x <- x[is.finite(x)]
      sprintf("%.2f (%.2f)", mean(x), sd(x)) }, character(1))
    # one-way ANOVA p across the four categories (descriptive)
    pa <- tryCatch(anova(lm(dm[[v]] ~ dm$A_SE_cat4))[["Pr(>F)"]][1], error = function(e) NA_real_)
    data.frame(variable = v, label = unname(vars1[v]), t(cells), p_anova = signif(pa, 3), check.names = FALSE, stringsAsFactors = FALSE)
  })
  out1 <- do.call(rbind, rows)
  nrow_ <- data.frame(variable = "n", label = "n", t(vapply(groups, function(g) if (g == "Total") sprintf("%d", nrow(dm)) else sprintf("%d", sum(dm$A_SE_cat4 == g)), character(1))), p_anova = NA_real_, check.names = FALSE, stringsAsFactors = FALSE)
  write_table(rbind(nrow_, out1), "followup_by_A_SE")
  # SDs for standardizing effects (2025 outcome SD in the analytic cohort; baseline SD for reference)
  write_table(data.frame(outcome = c("Y_ucla3", "Y_lsns_friends_2025", "Y_lsns_family_2025"),
                         sd_2025 = c(sd(dm$Y_ucla3), sd(dm$Y_lsns_friends_2025), sd(dm$Y_lsns_family_2025)),
                         sd_2024 = c(sd(dm$baseline_ucla3), sd(dm$baseline_lsns6_friends), sd(dm$baseline_lsns6_family)),
                         mean_2025 = c(mean(dm$Y_ucla3), mean(dm$Y_lsns_friends_2025), mean(dm$Y_lsns_family_2025)),
                         n = nrow(dm)), "outcome_sd")
  log_msg(sprintf("2025 SDs: UCLA-3 %.2f; LSNS friends %.2f; LSNS family %.2f", sd(dm$Y_ucla3), sd(dm$Y_lsns_friends_2025), sd(dm$Y_lsns_family_2025)))
}

# ============================================================
# §2  Construct distinctness — correlations, alpha, HTMT
# ============================================================
alpha_for <- function(items, data) {
  items <- intersect(items, names(data)); if (length(items) < 2L) return(NA_real_)
  m <- as.matrix(data[, items, drop = FALSE]); mode(m) <- "numeric"; m <- m[complete.cases(m), , drop = FALSE]
  a <- NA_real_
  invisible(capture.output(a <- tryCatch(suppressWarnings(suppressMessages(psych::alpha(m, check.keys = FALSE)$total$raw_alpha)), error = function(e) NA_real_)))
  a
}
htmt <- function(items_a, items_b, data) {
  # Henseler, Ringle & Sarstedt (2015): mean heterotrait-heteromethod r / geometric mean of the mean monotrait-heteromethod r
  a <- intersect(items_a, names(data)); b <- intersect(items_b, names(data))
  if (length(a) < 2L || length(b) < 2L) return(NA_real_)
  m <- as.matrix(data[, c(a, b), drop = FALSE]); mode(m) <- "numeric"; m <- m[complete.cases(m), , drop = FALSE]
  R <- abs(cor(m))
  het <- mean(R[a, b]); mono_a <- mean(R[a, a][upper.tri(R[a, a])]); mono_b <- mean(R[b, b][upper.tri(R[b, b])])
  het / sqrt(mono_a * mono_b)
}
if (.run_ok) {
  cvars <- c(W_attach = "Perceived AI-attachment (3 items)", W_attach2 = "Perceived AI-attachment (2 items: Q40.22, Q40.24)",
             W_anthrop = "Perceived anthropomorphism", PCUS = "Problematic generative-AI use (Q38.1-11)",
             Y_ucla3 = "UCLA-3 loneliness, 2025", baseline_ucla3 = "UCLA-3 loneliness, 2024",
             Y_lsns_friends_2025 = "LSNS-6 friends, 2025", Y_lsns_family_2025 = "LSNS-6 family, 2025", A_SE = "Social/emotional use (0-4)")
  present <- names(cvars)[vapply(names(cvars), function(v) v %in% names(dm) && sum(is.finite(dm[[v]])) > 10L, logical(1))]
  M <- as.matrix(dm[, present, drop = FALSE]); mode(M) <- "numeric"
  ct <- psych::corr.test(M, use = "pairwise", adjust = "none")
  rmat <- round(ct$r, 3); dimnames(rmat) <- list(present, present)
  write_table(data.frame(variable = present, label = unname(cvars[present]), rmat, check.names = FALSE), "correlations_r")
  pmat <- signif(ct$p, 3); dimnames(pmat) <- list(present, present)
  write_table(data.frame(variable = present, pmat, check.names = FALSE), "correlations_p")
  log_msg(sprintf("r(attachment, UCLA-3 2025) = %.3f; r(attachment, UCLA-3 2024) = %.3f; r(attachment, anthropomorphism) = %.3f; r(attachment, PCUS) = %.3f",
                  rmat["W_attach", "Y_ucla3"], rmat["W_attach", "baseline_ucla3"], rmat["W_attach", "W_anthrop"],
                  if ("PCUS" %in% present) rmat["W_attach", "PCUS"] else NA_real_))
  # item-level: each attachment item vs UCLA-3 total (2025) and vs each UCLA-3 item
  it_rows <- lapply(ATTACH_ITEMS, function(it) {
    x <- .as_num(dm[[it]])
    data.frame(attachment_item = it,
               r_ucla3_total_2025 = round(cor(x, dm$Y_ucla3, use = "complete.obs"), 3),
               r_ucla3_total_2024 = round(cor(x, dm$baseline_ucla3, use = "complete.obs"), 3),
               r_ucla_item1 = round(cor(x, dm[[paste0(UCLA25_ITEMS[1], "_rec")]], use = "complete.obs"), 3),
               r_ucla_item2 = round(cor(x, dm[[paste0(UCLA25_ITEMS[2], "_rec")]], use = "complete.obs"), 3),
               r_ucla_item3 = round(cor(x, dm[[paste0(UCLA25_ITEMS[3], "_rec")]], use = "complete.obs"), 3),
               r_anthrop = round(cor(x, dm$W_anthrop, use = "complete.obs"), 3),
               r_pcus = if ("PCUS" %in% present) round(cor(x, dm$PCUS, use = "complete.obs"), 3) else NA_real_)
  })
  write_table(do.call(rbind, it_rows), "attachment_items_vs_loneliness")
  # reliability
  write_table(data.frame(scale = c("W_attach (3 items)", "W_attach2 (2 items)", "W_anthrop (3 items)", "UCLA-3 2025", "UCLA-3 2024", "PCUS (11 items)"),
                         alpha = c(alpha_for(ATTACH_ITEMS, dm), alpha_for(paste0("Q40.", c(22, 24), "_2025"), dm), alpha_for(ANTHROP_ITEMS, dm),
                                   alpha_for(paste0(UCLA25_ITEMS, "_rec"), dm), alpha_for(UCLA24_ITEMS, dm), alpha_for(PCUS_ITEMS, dm))), "reliability")
  # HTMT
  ucla_rec_items <- paste0(UCLA25_ITEMS, "_rec")
  pairs <- list(c("AI-attachment", "UCLA-3 loneliness (2025)"), c("AI-attachment", "Perceived anthropomorphism"),
                c("AI-attachment", "Problematic generative-AI use"), c("Perceived anthropomorphism", "UCLA-3 loneliness (2025)"),
                c("Perceived anthropomorphism", "Problematic generative-AI use"), c("AI-attachment (2 items)", "UCLA-3 loneliness (2025)"))
  sets <- list("AI-attachment" = ATTACH_ITEMS, "AI-attachment (2 items)" = paste0("Q40.", c(22, 24), "_2025"), "UCLA-3 loneliness (2025)" = ucla_rec_items,
               "Perceived anthropomorphism" = ANTHROP_ITEMS, "Problematic generative-AI use" = PCUS_ITEMS)
  ht <- do.call(rbind, lapply(pairs, function(p) data.frame(construct_1 = p[1], construct_2 = p[2], HTMT = round(htmt(sets[[p[1]]], sets[[p[2]]], dm), 3),
                                                            below_0.85 = htmt(sets[[p[1]]], sets[[p[2]]], dm) < 0.85, stringsAsFactors = FALSE)))
  write_table(ht, "htmt")
  for (i in seq_len(nrow(ht))) log_msg(sprintf("HTMT %s vs %s = %.3f", ht$construct_1[i], ht$construct_2[i], ht$HTMT[i]))
}

# ============================================================
# §3  EFA — (a) attachment + UCLA-3 items; (b) attachment + anthropomorphism + problematic use
# ============================================================
# Fits the theoretically specified number of factors (n_theory); Horn's parallel analysis is run and
# its suggestion recorded in `<tag>_fit.csv` (and, when it differs from n_theory, that solution is
# also written under the suffix `_pa`).
run_efa_block <- function(efa_items, tag, label, n_theory, item_map, data, nfact_override = NULL) {
  efa_items <- intersect(efa_items, names(data))
  if (length(efa_items) < 3L) { log_msg(sprintf("  [%s] EFA SKIP: <3 items present.", tag)); return(invisible(FALSE)) }
  m <- as.matrix(data[, efa_items, drop = FALSE]); mode(m) <- "numeric"; m <- m[complete.cases(m), , drop = FALSE]
  if (nrow(m) < 100L) { log_msg(sprintf("  [%s] EFA SKIP: n_eff=%d < 100.", tag, nrow(m))); return(invisible(FALSE)) }
  pa <- tryCatch(suppressWarnings(suppressMessages(psych::fa.parallel(m, fa = "fa", fm = "minres", plot = FALSE, n.iter = 20))), error = function(e) NULL)
  nfact_suggest <- if (!is.null(pa) && is.finite(pa$nfact)) max(1L, as.integer(pa$nfact)) else NA_integer_
  nfact_use <- if (!is.null(nfact_override)) as.integer(nfact_override) else as.integer(n_theory)
  log_msg(sprintf("  [%s] parallel analysis suggests %s factors; fitting EFA with %d (%s).", tag, ifelse(is.na(nfact_suggest), "NA", as.character(nfact_suggest)), nfact_use, label))
  if (!is.null(pa) && is.null(nfact_override)) write_table(data.frame(factor = seq_along(pa$fa.values), eigen_actual = pa$fa.values,
    eigen_resampled = if (!is.null(pa$fa.sim)) pa$fa.sim else NA_real_, nfact_suggested = nfact_suggest, stringsAsFactors = FALSE), sprintf("%s_parallel", tag))
  rotate_use <- if (nfact_use > 1L && requireNamespace("GPArotation", quietly = TRUE)) "oblimin" else if (nfact_use > 1L) "varimax" else "none"
  fit_fa <- tryCatch(suppressWarnings(suppressMessages(psych::fa(m, nfactors = nfact_use, rotate = rotate_use, fm = "minres"))), error = function(e) { log_msg(sprintf("  [%s] fa() failed: %s", tag, conditionMessage(e))); NULL })
  if (is.null(fit_fa)) return(invisible(FALSE))
  kmo  <- tryCatch(suppressWarnings(suppressMessages(psych::KMO(m))), error = function(e) NULL)
  bart <- tryCatch(suppressWarnings(suppressMessages(psych::cortest.bartlett(cor(m), n = nrow(m)))), error = function(e) NULL)
  msai <- if (!is.null(kmo) && !is.null(kmo$MSAi)) kmo$MSAi else setNames(rep(NA_real_, ncol(m)), colnames(m))
  h2 <- fit_fa$communality; u2 <- fit_fa$uniquenesses; cmplx <- fit_fa$complexity
  L <- unclass(fit_fa$loadings); Lm <- matrix(as.numeric(L), nrow = nrow(L), dimnames = dimnames(L)); fac_names <- colnames(Lm)
  load_df <- data.frame(item = rownames(Lm), theoretical_group = item_map[rownames(Lm)], stringsAsFactors = FALSE)
  for (j in seq_len(ncol(Lm))) load_df[[fac_names[j]]] <- round(Lm[, j], 3)
  load_df$top_factor <- fac_names[apply(abs(Lm), 1, which.max)]
  load_df$h2 <- round(unname(h2[rownames(Lm)]), 3); load_df$u2 <- round(unname(u2[rownames(Lm)]), 3)
  load_df$complexity <- round(unname(cmplx[rownames(Lm)]), 3); load_df$MSA_item <- round(unname(msai[rownames(Lm)]), 3)
  write_table(load_df, sprintf("%s_loadings", tag))
  va <- fit_fa$Vaccounted
  cum_row <- if ("Cumulative Var" %in% rownames(va)) "Cumulative Var" else "Proportion Var"
  write_table(data.frame(factor = colnames(va), SS_loadings = va["SS loadings", ], prop_var = va["Proportion Var", ], cum_var = va[cum_row, ], stringsAsFactors = FALSE), sprintf("%s_variance", tag))
  phi <- fit_fa$Phi
  if (!is.null(phi) && is.matrix(phi) && nrow(phi) > 1L) {
    phi_m <- matrix(as.numeric(phi), nrow = nrow(phi), dimnames = dimnames(phi))
    write_table(data.frame(factor = rownames(phi_m), round(phi_m, 3), check.names = FALSE, stringsAsFactors = FALSE), sprintf("%s_factor_cor", tag))
  }
  getf <- function(x, i = 1L) if (!is.null(x) && length(x) >= i && is.finite(x[i])) as.numeric(x[i]) else NA_real_
  cum_var_use <- as.numeric(va[cum_row, ncol(va)])
  write_table(data.frame(
    metric = c("n_obs", "n_items", "n_factors", "n_factors_parallel_analysis", "KMO_overall", "bartlett_chisq", "bartlett_df", "bartlett_p",
               "model_chisq", "model_chisq_df", "model_chisq_p", "RMSEA", "RMSEA_lower", "RMSEA_upper", "TLI", "RMSR", "BIC", "cum_var_pct"),
    value = c(nrow(m), ncol(m), nfact_use, nfact_suggest, if (!is.null(kmo)) round(getf(kmo$MSA), 3) else NA_real_,
              if (!is.null(bart)) round(getf(bart$chisq), 2) else NA_real_, if (!is.null(bart)) getf(bart$df) else NA_real_, if (!is.null(bart)) signif(getf(bart$p.value), 3) else NA_real_,
              round(getf(fit_fa$STATISTIC), 2), getf(fit_fa$dof), signif(getf(fit_fa$PVAL), 3),
              round(getf(fit_fa$RMSEA, 1L), 3), round(getf(fit_fa$RMSEA, 2L), 3), round(getf(fit_fa$RMSEA, 3L), 3),
              round(getf(fit_fa$TLI), 3), round(getf(fit_fa$rms), 3), round(getf(fit_fa$BIC), 1), round(100 * cum_var_use, 1)),
    stringsAsFactors = FALSE), sprintf("%s_fit", tag))
  log_msg(sprintf("  [%s] EFA (%d factors): cum var=%.1f%%; KMO=%s; RMSEA=%.3f; TLI=%.3f; items->%d top-factors (vs %d groups).", tag, nfact_use, 100 * cum_var_use,
                  if (!is.null(kmo)) sprintf("%.2f", getf(kmo$MSA)) else "NA", getf(fit_fa$RMSEA, 1L), getf(fit_fa$TLI), length(unique(load_df$top_factor)), n_theory))
  # if parallel analysis suggests a different number of factors, also write that solution (suffix _pa)
  if (is.null(nfact_override) && is.finite(nfact_suggest) && nfact_suggest != nfact_use)
    run_efa_block(efa_items, paste0(tag, "_pa"), paste0(label, " [parallel-analysis solution]"), n_theory, item_map, data, nfact_override = nfact_suggest)
  invisible(TRUE)
}
if (.run_ok) {
  ucla_rec_items <- paste0(UCLA25_ITEMS, "_rec")
  map_a <- c(setNames(rep("AI-attachment", 3), ATTACH_ITEMS), setNames(rep("UCLA-3 loneliness", 3), ucla_rec_items))
  run_efa_block(c(ATTACH_ITEMS, ucla_rec_items), "efa_attach_ucla", "attachment (3) + UCLA-3 (3) items, 2025", n_theory = 2L, item_map = map_a, data = dm)
  map_b <- c(setNames(rep("AI-attachment", 3), ATTACH_ITEMS), setNames(rep("Anthropomorphism", 3), ANTHROP_ITEMS), setNames(rep("Problematic use", 11), PCUS_ITEMS))
  run_efa_block(c(ATTACH_ITEMS, ANTHROP_ITEMS, PCUS_ITEMS), "efa_attach_anthrop_pcus", "attachment (3) + anthropomorphism (3) + problematic use (11) items, 2025", n_theory = 3L, item_map = map_b, data = dm)
}

# ============================================================
# §4  Selection into social/emotional use and into attachment (baseline predictors)
# ============================================================
if (.run_ok) {
  cv <- C_VARS[vapply(C_VARS, function(v) length(unique(dm[[v]])) > 1L, logical(1))]
  fml <- function(y) as.formula(paste(y, "~", paste(cv, collapse = " + ")))
  tidy_fit <- function(fit, model, exp_coef = FALSE) {
    co <- summary(fit)$coefficients; ci <- suppressMessages(tryCatch(confint.default(fit), error = function(e) cbind(NA, NA)))
    data.frame(model = model, term = rownames(co), estimate = if (exp_coef) exp(co[, 1]) else co[, 1],
               ci_lo = if (exp_coef) exp(ci[, 1]) else ci[, 1], ci_hi = if (exp_coef) exp(ci[, 2]) else ci[, 2],
               p = co[, 4], scale = if (exp_coef) "odds ratio" else "coefficient", row.names = NULL, stringsAsFactors = FALSE)
  }
  f1 <- glm(fml("A_SE_any"), data = dm, family = binomial())
  f2 <- lm(fml("A_SE"), data = dm)
  f3 <- lm(fml("W_attach"), data = dm)
  f4 <- lm(fml("W_anthrop"), data = dm)
  sel <- rbind(tidy_fit(f1, "Any social/emotional use, 2025 (logistic)", TRUE),
               tidy_fit(f2, "Social/emotional use intensity, 2025 (0-4; linear)"),
               tidy_fit(f3, "Perceived AI-attachment, 2025 (1-7; linear)"),
               tidy_fit(f4, "Perceived anthropomorphism, 2025 (1-7; linear)"))
  sel$key_predictor <- sel$term %in% c("baseline_ucla3", "baseline_k6", "baseline_lsns6_friends", "baseline_lsns6_family", "living_alone", "married", "age_2024", "sex_female",
                                       "big5_extraversion", "big5_neuroticism", "big5_agreeableness", "big5_conscientiousness", "big5_openness")
  write_table(sel, "selection_models")
  for (m_ in unique(sel$model)) { r <- sel[sel$model == m_ & sel$term == "baseline_ucla3", ]
    log_msg(sprintf("  [%s] baseline UCLA-3: %s=%.3f (%.3f, %.3f) p=%.3g", m_, r$scale, r$estimate, r$ci_lo, r$ci_hi, r$p)) }
}

# ============================================================
# §5  Baseline characteristics by generative-AI status at 2025 (all two-wave respondents)
# ============================================================
{
  st <- df$ai_start
  grp <- factor(ifelse(st == 1L, "Never used", ifelse(st == 2L, "Used before, not now", ifelse(st == 3L, "Initiated 2022-2023",
                ifelse(st == 4L, "Initiated 2024", ifelse(st %in% c(5L, 6L), "Initiated 2025 (analytic cohort)", NA))))),
                levels = c("Never used", "Used before, not now", "Initiated 2022-2023", "Initiated 2024", "Initiated 2025 (analytic cohort)"))
  dd <- df[!is.na(grp), , drop = FALSE]; dd$ai_group <- droplevels(grp[!is.na(grp)])
  spec <- list(
    c("n", "n", "n"), c("Age (years), mean (SD)", "age_2024", "cont"), c("Female, n (%)", "sex_female", "bin"),
    c("Education: below university", "edu_below", "bin"), c("Education: university", "edu_univ", "bin"), c("Education: graduate", "edu_grad", "bin"),
    c("Employment: regular", "emp_reg", "bin"), c("Employment: non-regular", "emp_nonreg", "bin"), c("Employment: self-employed", "emp_self", "bin"),
    c("Employment: student", "emp_student", "bin"), c("Employment: not working", "emp_notwork", "bin"),
    c("Income: <2 m JPY", "income_lt_2m", "bin"), c("Income: 2-6 m JPY", "income_2_6m", "bin"), c("Income: 6-10 m JPY", "income_6_10m", "bin"),
    c("Income: 10+ m JPY", "income_10m_plus", "bin"), c("Income: unknown", "income_unknown", "bin"),
    c("Married", "married", "bin"), c("Living alone", "living_alone", "bin"),
    c("K6 psychological distress (0-24), mean (SD)", "baseline_k6", "cont"), c("ACE score (count), mean (SD)", "ace_score", "cont"),
    c("Mental and physical health composite, mean (SD)", "mental_physical_health", "cont"),
    c("Extraversion (TIPI-J, 1-7), mean (SD)", "big5_extraversion", "cont"), c("Agreeableness (TIPI-J, 1-7), mean (SD)", "big5_agreeableness", "cont"),
    c("Conscientiousness (TIPI-J, 1-7), mean (SD)", "big5_conscientiousness", "cont"), c("Neuroticism (TIPI-J, 1-7), mean (SD)", "big5_neuroticism", "cont"),
    c("Openness (TIPI-J, 1-7), mean (SD)", "big5_openness", "cont"),
    c("UCLA-3 loneliness (0-9), mean (SD)", "baseline_ucla3", "cont"), c("LSNS-6 friends (0-15), mean (SD)", "baseline_lsns6_friends", "cont"),
    c("LSNS-6 family (0-15), mean (SD)", "baseline_lsns6_family", "cont"))
  cols <- levels(dd$ai_group)
  cont <- function(x) { x <- x[is.finite(x)]; if (!length(x)) return("—"); sprintf("%.2f (%.2f)", mean(x), sd(x)) }
  bin  <- function(x) { x <- x[!is.na(x)]; if (!length(x)) return("—"); n <- sum(x == 1); sprintf("%d (%.1f%%)", n, 100 * n / length(x)) }
  rows <- lapply(spec, function(s) {
    cells <- vapply(cols, function(g) { sub <- dd[dd$ai_group == g, , drop = FALSE]
      if (s[3] == "n") return(sprintf("%d", nrow(sub))); if (!s[2] %in% names(sub)) return("—")
      if (s[3] == "cont") cont(sub[[s[2]]]) else bin(sub[[s[2]]]) }, character(1))
    # p across groups (ANOVA for continuous, chi-square for binary), descriptive
    p <- tryCatch(if (s[3] == "cont") anova(lm(dd[[s[2]]] ~ dd$ai_group))[["Pr(>F)"]][1]
                  else if (s[3] == "bin") suppressWarnings(chisq.test(table(dd[[s[2]]], dd$ai_group))$p.value) else NA_real_, error = function(e) NA_real_)
    setNames(c(list(s[1]), as.list(cells), list(signif(p, 3))), c("Characteristic", cols, "p"))
  })
  out5 <- do.call(rbind, lapply(rows, as.data.frame, stringsAsFactors = FALSE, check.names = FALSE))
  write_table(out5, "ai_status_groups_baseline")
  log_msg(sprintf("AI-status groups (two-wave respondents, n=%d): %s", nrow(dd), paste(sprintf("%s=%d", cols, as.integer(table(dd$ai_group))), collapse = "; ")))
}

writeLines(capture.output(sessionInfo()), file.path(LOGS_DIR, sprintf("sessionInfo_validity_%s_%s.txt", RUN_MODE, .timestamp)))
log_msg("=== END ===")
