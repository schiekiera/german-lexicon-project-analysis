#!/usr/bin/env Rscript
# ============================================================================
# 05_reliability_icc.R  --  Item-level (word) reliability for the GLP
# ----------------------------------------------------------------------------
# Computes, per condition (overall / words / pseudowords) and per dependent
# variable (log RT, inverse RT, accuracy), BOTH method families side by side:
#
#   (A) In-house approaches (from 04_reliability_summarize_approaches.R)
#       A1  mixed-model item ICC  lmer(y ~ 1 + (1|word) + (1|subject))   ~ ICC3k (consistency = Cronbach's alpha)
#       A2  gmeanrel / ICC2.lme   one-way group-mean reliability         ~ ICC1k (absolute agreement; conservative)
#       A3  split-half + Spearman-Brown (rank-based, resampled)          ~ ICC3k (consistency)
#
#   (B) Brysbaert's ICC_stimulus_long  (scripts/ICC_stimulus_long.R)
#       ICC1k / ICC2k / ICC3k with harmonic k and 3-SD outlier removal,
#       PLUS BLUP item scores (shrunken, robust per-word estimates).
#       ICC2k (generalisation to new participant samples) is the piece the
#       in-house approaches do not provide.
#
# Conceptual mapping (why we report both): the in-house approaches recover
# ICC1k (A2) and ICC3k (A1, A3); Brysbaert's function reproduces those AND
# adds ICC2k + BLUPs in one robust pass. Reporting them together shows they
# converge and lets us pick the coefficient to headline (ICC3k = alpha for
# consistency; ICC2k for generalisability).
#
# OUTPUTS
#   output/summary/reliability_icc_comparison.csv     (one row per condition x dv x method x coefficient)
#   output/summary/reliability_blups_<cond>_<dv>.csv  (per-word N, Raw_Mean, BLUP)
#   output/plots/reliability_icc_comparison.{pdf,png}
#   output/plots/reliability_blup_vs_raw_overall_logrt.pdf
#   output/log/reliability_icc_<timestamp>.log
#
# RUNTIME WARNING: the crossed mixed models are fit on millions of rows
# (the "overall" condition is the full cleaned dataset). Expect long runtimes
# and high RAM. Test first with SUBSAMPLE_PARTICIPANTS set to e.g. 300, and/or
# narrow CONDITIONS / DVS, before the full run.
# ============================================================================

rm(list = ls())

suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(lme4)
  library(nlme)
  library(multilevel)
  library(data.table)
  library(parallel)
  library(pbapply)
  library(here)
})

pboptions(type = "txt")
set.seed(12345)

# ---------------------------------------------------------------------------
# Config  (edit these for quick test runs vs. the full final run)
# ---------------------------------------------------------------------------
CONDITIONS             <- c("overall", "words_only", "pseudo_only")
DVS                    <- c("log_rt", "inv_rt", "accuracy")  # inv_rt = -1000/rt
USE_CORRECT_ONLY       <- TRUE      # RT reliability on CORRECT trials only (set FALSE to match the 04 script, which used all trials)
RT_MIN                 <- 200
RT_MAX                 <- 4000
OUTLIER_SD             <- 3         # Brysbaert 3-SD residual outlier removal (NULL to disable)
N_SPLIT                <- 200       # split-half iterations for A3 (original script used 1000)
SUBSAMPLE_PARTICIPANTS <- NULL      # e.g. 300 for a fast test; NULL = use all participants
RUN_INHOUSE            <- TRUE
RUN_APPROACH2          <- TRUE      # A2 (nlme::lme + gmeanrel) is the SLOWEST; set FALSE if it stalls on the full data
RUN_BRYSBAERT          <- TRUE
N_CORES                <- max(1, detectCores() - 1)

# ---------------------------------------------------------------------------
# Logging
# ---------------------------------------------------------------------------
run_ts  <- format(Sys.time(), "%Y-%m-%d_%H-%M-%S")
log_dir <- here::here("output", "log")
dir.create(log_dir, showWarnings = FALSE, recursive = TRUE)
dir.create(here::here("output", "summary"), showWarnings = FALSE, recursive = TRUE)
dir.create(here::here("output", "plots"),   showWarnings = FALSE, recursive = TRUE)
log_file <- file.path(log_dir, paste0("reliability_icc_", run_ts, ".log"))

log_info <- function(...) {
  msg <- paste0("[", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "] ", paste(..., collapse = " "))
  cat(msg, "\n")
  cat(msg, "\n", file = log_file, append = TRUE)
}

log_info("=== Reliability (ICC) analysis started ===")
log_info("Config: correct_only =", USE_CORRECT_ONLY, "| rt =", RT_MIN, "-", RT_MAX,
         "| outlier_sd =", ifelse(is.null(OUTLIER_SD), "NULL", OUTLIER_SD),
         "| n_split =", N_SPLIT, "| subsample =", ifelse(is.null(SUBSAMPLE_PARTICIPANTS), "all", SUBSAMPLE_PARTICIPANTS))

# ---------------------------------------------------------------------------
# Load Brysbaert's stand-alone function
# ---------------------------------------------------------------------------
icc_fun_path <- here::here("scripts", "ICC_stimulus_long.R")
if (!file.exists(icc_fun_path)) {
  alt <- here::here("presentation", "context", "reliability", "attached", "mail3_ICC_stimulus_long.R")
  if (file.exists(alt)) icc_fun_path <- alt else stop("ICC_stimulus_long.R not found in scripts/ or presentation context.")
}
source(icc_fun_path)
log_info("Sourced ICC_stimulus_long from:", icc_fun_path)

# ---------------------------------------------------------------------------
# Locate + load the cleaned data
# ---------------------------------------------------------------------------
find_clean_file <- function() {
  dirs <- c(here::here("clean_data"), here::here("output", "clean_data"), "clean_data", "output/clean_data")
  for (d in dirs) {
    fs <- list.files(d, pattern = "anonymized_final_data_precleaned_.*\\.csv$", full.names = TRUE)
    if (length(fs)) return(fs[which.max(file.info(fs)$mtime)])
  }
  stop("No cleaned data file found in clean_data/ or output/clean_data/.")
}
file_path <- find_clean_file()
log_info("Using input file:", file_path)

glp <- readr::read_csv(file_path, show_col_types = FALSE) |>
  dplyr::select(subject_id, word, rt, type, accuracy) |>
  dplyr::mutate(
    accuracy_bin = dplyr::case_when(
      tolower(as.character(accuracy)) == "correct"   ~ 1,
      tolower(as.character(accuracy)) == "incorrect" ~ 0,
      TRUE ~ NA_real_
    ),
    type       = as.character(type),
    subject_id = as.factor(subject_id),
    word       = as.factor(word)
  )
log_info("Rows loaded:", nrow(glp), "| participants:", dplyr::n_distinct(glp$subject_id),
         "| items:", dplyr::n_distinct(glp$word))

if (!is.null(SUBSAMPLE_PARTICIPANTS)) {
  keep_ids <- sample(unique(glp$subject_id), min(SUBSAMPLE_PARTICIPANTS, dplyr::n_distinct(glp$subject_id)))
  glp <- dplyr::filter(glp, subject_id %in% keep_ids) |> droplevels()
  log_info("SUBSAMPLE active -> participants:", dplyr::n_distinct(glp$subject_id), "| rows:", nrow(glp))
}

# ---------------------------------------------------------------------------
# Prepare a long (subject_id, word, resp) table for a given DV
# ---------------------------------------------------------------------------
prep_long <- function(df, dv = c("log_rt", "inv_rt", "accuracy"),
                      correct_only = TRUE, rt_min = 200, rt_max = 4000) {
  dv <- match.arg(dv)
  d <- df
  if (dv %in% c("log_rt", "inv_rt")) {
    if (correct_only) d <- dplyr::filter(d, accuracy_bin == 1)
    d <- dplyr::filter(d, !is.na(rt), rt >= rt_min, rt <= rt_max)
    d <- dplyr::mutate(d, resp = if (dv == "log_rt") log(rt) else -1000 / rt)
  } else {  # accuracy: 0/1, all valid trials (reliability is sign/scale invariant)
    d <- dplyr::filter(d, !is.na(accuracy_bin))
    d <- dplyr::mutate(d, resp = as.numeric(accuracy_bin))
  }
  d |>
    dplyr::select(subject_id, word, resp) |>
    dplyr::filter(!is.na(resp), !is.na(subject_id), !is.na(word)) |>
    droplevels()
}

# ---------------------------------------------------------------------------
# In-house approaches (adapted from 04_reliability_summarize_approaches.R)
# Operate on a long data.frame with columns: subject_id, word, resp
# ---------------------------------------------------------------------------

# A1: mixed-model item ICC  ~ ICC3k (consistency)
inhouse_a1_mixed <- function(d) {
  m  <- lme4::lmer(resp ~ 1 + (1 | word) + (1 | subject_id), data = d, REML = FALSE)
  vc <- as.data.frame(lme4::VarCorr(m))
  s_item  <- vc$vcov[vc$grp == "word"]
  s_error <- vc$vcov[vc$grp == "Residual"]
  n_avg   <- mean(table(d$word))                       # simple mean obs/word (vs harmonic k)
  list(value = s_item / (s_item + s_error / n_avg), k = n_avg)
}

# A2: gmeanrel / ICC2.lme on word (one-way)  ~ ICC1k (absolute agreement)
inhouse_a2_gmeanrel <- function(d) {
  mod <- nlme::lme(
    resp ~ 1, random = ~ 1 | word, data = d, na.action = na.omit,
    control = nlme::lmeControl(maxIter = 100, msMaxIter = 100, msMaxEval = 400, opt = "optim")
  )
  gr <- multilevel::gmeanrel(mod)
  list(value = mean(gr$MeanRel), k = NA_real_)
}

# A3: split-half over participants + Spearman-Brown (Spearman corr)  ~ ICC3k
inhouse_a3_splithalf <- function(d, n_samp = 200, n_cores = 1, base_seed = 10000) {
  parts <- unique(d$subject_id)
  if (length(parts) < 4) return(list(value = NA_real_, k = n_samp))

  sb_iteration <- function(seed) {
    set.seed(seed)
    half  <- floor(length(parts) / 2)
    A_ids <- sample(parts, size = half, replace = FALSE)
    B_ids <- setdiff(parts, A_ids)
    means_A <- d[d$subject_id %in% A_ids, ] |>
      dplyr::group_by(word) |> dplyr::summarise(mA = mean(resp), .groups = "drop")
    means_B <- d[d$subject_id %in% B_ids, ] |>
      dplyr::group_by(word) |> dplyr::summarise(mB = mean(resp), .groups = "drop")
    merged <- dplyr::inner_join(means_A, means_B, by = "word")
    r  <- suppressWarnings(cor(merged$mA, merged$mB, method = "spearman"))
    (2 * r) / (1 + r)
  }

  cl <- parallel::makeCluster(n_cores)
  on.exit(parallel::stopCluster(cl), add = TRUE)
  parallel::clusterExport(cl, varlist = c("d", "parts", "sb_iteration"), envir = environment())
  invisible(parallel::clusterEvalQ(cl, library(dplyr)))
  sbs <- pbapply::pblapply(seq_len(n_samp), function(s) sb_iteration(base_seed + s), cl = cl)
  list(value = mean(unlist(sbs), na.rm = TRUE), k = n_samp)
}

# ---------------------------------------------------------------------------
# Main loop: condition x DV x method
# ---------------------------------------------------------------------------
results <- list()
blups   <- list()
add_row <- function(...) results[[length(results) + 1]] <<- tibble::tibble(...)

cond_filter <- function(cond) switch(cond,
  overall     = glp,
  words_only  = dplyr::filter(glp, type == "word"),
  pseudo_only = dplyr::filter(glp, type == "pseudoword")
)

for (cond in CONDITIONS) {
  dfc <- cond_filter(cond)
  for (dv in DVS) {
    d  <- prep_long(dfc, dv, correct_only = USE_CORRECT_ONLY, rt_min = RT_MIN, rt_max = RT_MAX)
    np <- dplyr::n_distinct(d$subject_id)
    ni <- dplyr::n_distinct(d$word)
    log_info(sprintf("--- condition=%s | dv=%s | rows=%d | participants=%d | items=%d ---",
                     cond, dv, nrow(d), np, ni))

    # (B) Brysbaert ICC_stimulus_long: ICC1k / ICC2k / ICC3k + BLUPs
    if (RUN_BRYSBAERT) {
      tryCatch({
        # Outlier removal is residual-based and assumes a continuous response;
        # disable it for binary accuracy, otherwise the minority ("incorrect")
        # class gets flagged as outliers and removed.
        ol_sd <- if (dv == "accuracy") NULL else OUTLIER_SD
        b <- ICC_stimulus_long(participant = "subject_id", stimulus = "word",
                               response = "resp", data = as.data.frame(d),
                               outlier_sd = ol_sd)
        add_row(condition = cond, dv = dv, method = "Brysbaert ICC_stimulus_long",
                coefficient = b$ICCs$Measure, value = b$ICCs$Value,
                k_or_n = b$ICCs$k_Harmonic, n_participants = np, n_items = ni,
                outliers_removed = b$Outliers_Removed)
        bl <- as.data.frame(b$Stimuli)
        bl$condition <- cond; bl$dv <- dv
        blups[[paste(cond, dv, sep = "_")]] <- bl
        log_info(sprintf("  Brysbaert: ICC1k=%.3f ICC2k=%.3f ICC3k=%.3f (k=%.1f, outliers=%d)",
                         b$ICCs$Value[1], b$ICCs$Value[2], b$ICCs$Value[3],
                         b$ICCs$k_Harmonic[1], b$Outliers_Removed))
      }, error = function(e) log_info("  [Brysbaert FAILED]:", conditionMessage(e)))
    }

    # (A) In-house approaches
    if (RUN_INHOUSE) {
      tryCatch({
        a1 <- inhouse_a1_mixed(d)
        add_row(condition = cond, dv = dv, method = "In-house A1 (mixed)",
                coefficient = "ICC3k-equiv", value = a1$value, k_or_n = a1$k,
                n_participants = np, n_items = ni, outliers_removed = NA_integer_)
        log_info(sprintf("  A1 mixed (ICC3k-equiv): %.3f", a1$value))
      }, error = function(e) log_info("  [A1 FAILED]:", conditionMessage(e)))

      if (RUN_APPROACH2) {
        tryCatch({
          a2 <- inhouse_a2_gmeanrel(d)
          add_row(condition = cond, dv = dv, method = "In-house A2 (gmeanrel)",
                  coefficient = "ICC1k-equiv", value = a2$value, k_or_n = a2$k,
                  n_participants = np, n_items = ni, outliers_removed = NA_integer_)
          log_info(sprintf("  A2 gmeanrel (ICC1k-equiv): %.3f", a2$value))
        }, error = function(e) log_info("  [A2 FAILED]:", conditionMessage(e)))
      }

      tryCatch({
        a3 <- inhouse_a3_splithalf(d, n_samp = N_SPLIT, n_cores = N_CORES)
        add_row(condition = cond, dv = dv, method = "In-house A3 (split-half SB)",
                coefficient = "ICC3k-equiv", value = a3$value, k_or_n = a3$k,
                n_participants = np, n_items = ni, outliers_removed = NA_integer_)
        log_info(sprintf("  A3 split-half SB (ICC3k-equiv): %.3f", a3$value))
      }, error = function(e) log_info("  [A3 FAILED]:", conditionMessage(e)))
    }
  }
}

# ---------------------------------------------------------------------------
# Write comparison table + BLUPs
# ---------------------------------------------------------------------------
results_tbl <- dplyr::bind_rows(results) |>
  dplyr::mutate(
    condition = factor(condition, levels = c("overall", "words_only", "pseudo_only")),
    dv        = factor(dv,        levels = c("log_rt", "inv_rt", "accuracy"))
  ) |>
  dplyr::arrange(condition, dv, method, coefficient)

readr::write_csv(results_tbl, here::here("output", "summary", "reliability_icc_comparison.csv"))
log_info("Wrote comparison table: output/summary/reliability_icc_comparison.csv  (", nrow(results_tbl), "rows )")
print(as.data.frame(results_tbl), digits = 3)

for (nm in names(blups)) {
  out <- here::here("output", "summary", paste0("reliability_blups_", nm, ".csv"))
  readr::write_csv(blups[[nm]], out)
  log_info("Wrote BLUPs:", out, "(", nrow(blups[[nm]]), "items )")
}

# ---------------------------------------------------------------------------
# Plots
# ---------------------------------------------------------------------------
rt_results <- dplyr::filter(results_tbl, dv %in% c("log_rt", "inv_rt"))
if (nrow(rt_results) > 0) {
  p <- ggplot(rt_results, aes(x = coefficient, y = value, color = method, shape = dv)) +
    geom_point(size = 3, position = position_dodge(width = 0.5)) +
    geom_hline(yintercept = 0.8, linetype = "dashed", color = "grey50") +
    facet_wrap(~ condition) +
    coord_cartesian(ylim = c(0, 1)) +
    labs(
      title = "Item-level reliability: in-house approaches vs Brysbaert ICCs",
      subtitle = "ICC1k = conservative (absolute) | ICC2k = generalisable | ICC3k = consistency (= Cronbach's alpha)",
      x = "Coefficient", y = "Reliability", color = "Method", shape = "DV"
    ) +
    theme_classic() +
    theme(legend.position = "top", axis.text.x = element_text(angle = 30, hjust = 1))
  ggsave(here::here("output", "plots", "reliability_icc_comparison.pdf"), p, width = 11, height = 4.5)
  ggsave(here::here("output", "plots", "reliability_icc_comparison.png"), p, width = 11, height = 4.5, dpi = 150)
  log_info("Wrote plot: output/plots/reliability_icc_comparison.{pdf,png}")
}

# BLUP vs raw means (overall, log RT) -- illustrates shrinkage
if (!is.null(blups[["overall_log_rt"]])) {
  bl <- blups[["overall_log_rt"]]
  pb <- ggplot(bl, aes(x = Raw_Mean, y = BLUP)) +
    geom_point(alpha = 0.2, size = 0.5) +
    geom_abline(slope = 1, intercept = 0, color = "red") +
    labs(title = "BLUP vs raw item means (overall, log RT)",
         subtitle = "Shrinkage pulls extreme raw means toward the grand mean",
         x = "Raw mean (log RT)", y = "BLUP (shrunken estimate)") +
    theme_classic()
  ggsave(here::here("output", "plots", "reliability_blup_vs_raw_overall_logrt.pdf"), pb, width = 6, height = 5)
  log_info("Wrote plot: output/plots/reliability_blup_vs_raw_overall_logrt.pdf")
}

log_info("=== Reliability (ICC) analysis finished ===")
