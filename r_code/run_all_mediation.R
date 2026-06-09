# ============================================================
# run_all_mediation.R
# Sequential orchestrator for the mediation pipeline.
# ============================================================
# Runs the 6 outcome scripts + the manuscript packager in fresh R processes
# (clean global environment per stage; one stage's crash does not poison the next).
# Writes a master log + per-stage timing CSV to output/ai_mod/logs/.
#
# Per-item mediator sensitivity is integrated as section 5 of each outcome script
# (not a separate script); it runs as part of each stage below. The joint-mediator
# interventional sensitivity is section 6 of the three W_attach outcome scripts.
#
# Usage: Rscript r_code/run_all_mediation.R
# ============================================================

suppressPackageStartupMessages({ library(here) })

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
.proj_root <- find_proj_root()

# ---- Stages (run in this order) ----
STAGES <- c(
  "ai_attach_loneliness.R",
  "ai_attach_lsns_friends.R",
  "ai_attach_lsns_family.R",
  "ai_anthrop_loneliness.R",
  "ai_anthrop_lsns_friends.R",
  "ai_anthrop_lsns_family.R",
  "ai_mediation_tables_figures.R"
)

# ---- Output logs ----
LOG_DIR <- file.path(.proj_root, "output", "ai_mod", "logs"); dir.create(LOG_DIR, recursive = TRUE, showWarnings = FALSE)
.timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
MASTER_LOG <- file.path(LOG_DIR, sprintf("run_all_mediation_%s.log", .timestamp))
TIMING_CSV <- file.path(LOG_DIR, sprintf("run_all_mediation_timings_%s.csv", .timestamp))
log_msg <- function(...) { m <- paste0(format(Sys.time(), "[%H:%M:%S] "), paste(..., collapse = " ")); cat(m, "\n", sep = ""); cat(m, "\n", sep = "", file = MASTER_LOG, append = TRUE); invisible(m) }

log_msg("================================================================")
log_msg("=== run_all_mediation.R ===  project root:", .proj_root)
log_msg("================================================================")

# ---- Locate Rscript executable ----
.R_HOME <- R.home("bin")
RSCRIPT <- if (.Platform$OS.type == "windows") file.path(.R_HOME, "Rscript.exe") else file.path(.R_HOME, "Rscript")
if (!file.exists(RSCRIPT)) stop("Rscript executable not found at: ", RSCRIPT)
log_msg("Rscript:", RSCRIPT)

# ---- Run each stage in a fresh R process ----
timings <- data.frame(stage = character(), start = character(), end = character(),
                      seconds = double(), exit_code = integer(), stringsAsFactors = FALSE)

for (i in seq_along(STAGES)) {
  stage  <- STAGES[i]
  script <- file.path(.proj_root, stage)        # .proj_root IS the r_code/ folder
  if (!file.exists(script)) { log_msg(sprintf("[%d/%d] MISSING: %s -- skipping", i, length(STAGES), stage)); next }

  log_msg(sprintf("[%d/%d] >>> %s", i, length(STAGES), stage))
  t0 <- Sys.time()
  rc <- system2(RSCRIPT, args = shQuote(script), stdout = "", stderr = "")
  t1 <- Sys.time()
  dt <- as.numeric(difftime(t1, t0, units = "secs"))
  log_msg(sprintf("[%d/%d] <<< %s  exit=%d  elapsed=%.1f s", i, length(STAGES), stage, rc, dt))

  timings <- rbind(timings, data.frame(
    stage = stage,
    start = format(t0, "%Y-%m-%d %H:%M:%S"),
    end   = format(t1, "%Y-%m-%d %H:%M:%S"),
    seconds = dt, exit_code = rc, stringsAsFactors = FALSE))
}

utils::write.csv(timings, TIMING_CSV, row.names = FALSE, fileEncoding = "UTF-8")
log_msg("wrote:", TIMING_CSV)
log_msg("=== ALL STAGES COMPLETE ===")
