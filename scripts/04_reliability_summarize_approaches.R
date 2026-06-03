rm(list = ls())

# ============================================================================
# Reliability of item-level (word) norms -- feasible methods compared
# ----------------------------------------------------------------------------
# Single, self-contained script. Runs on the OBSERVED cleaned data and compares
# the feasible reliability methods on the actual sample (no projection).
#
# Methods compared (item-level, on log RT of correct trials):
#   approach_1_mixed_icc   lmer(log_rt ~ 1+(1|word)+(1|subject))   ~ ICC3k (consistency = Cronbach's alpha)
#   approach_3_split_half  split-half over participants + Spearman-Brown  ~ ICC3k (consistency)
#   brysbaert_icc1k        ICC_stimulus_long, one-way random       ICC1k (absolute agreement; conservative)
#   brysbaert_icc2k        ICC_stimulus_long, two-way random       ICC2k (generalisation to new samples)
#   brysbaert_icc3k        ICC_stimulus_long, two-way mixed        ICC3k (= Cronbach's alpha)
# Brysbaert's function additionally returns BLUP item scores (shrunken means).
#
# NOTE: the original "approach 2" (gmeanrel / ICC2.lme via nlme) is omitted --
# nlme is infeasible at this scale (~77,500 item grouping levels). The
# coefficient it targets (ICC1k) is reported here via brysbaert_icc1k (lme4).
# ============================================================================

# -----------------------------
# Libraries
# -----------------------------
library(dplyr)
library(tidyr)
library(purrr)
library(ggplot2)
library(lme4)
library(data.table) # fast data load, fast split-half aggregation, and ICC_stimulus_long
library(here)

set.seed(12345)

# -----------------------------
# Settings
# -----------------------------
n_samp <- 200 # split-half iterations (stable Spearman-Brown estimate; raise for more precision)
OUTLIER_SD <- 3 # Brysbaert 3-SD residual outlier removal (NULL to disable)

# Progress log (appended live so a long run can be monitored)
PROG <- here::here("output", "log", "reliability_progress.log")
if (file.exists(PROG)) invisible(file.remove(PROG))
plog <- function(...) {
  line <- sprintf("[%s] %s", format(Sys.time(), "%H:%M:%S"), paste0(...))
  cat(line, "\n")
  cat(line, "\n", file = PROG, append = TRUE)
}
plog("libraries loaded; starting reliability analysis")

# ============================================================================
# ICC_stimulus_long  (embedded; verbatim from Marc Brysbaert, Ghent University,
# "Calculating the reliability of word norms", May 2026). Returns ICC1k/2k/3k
# with harmonic k, 3-SD outlier removal, and per-stimulus BLUPs.
# ============================================================================
ICC_stimulus_long <- function(participant, stimulus, response, data, outlier_sd = 3) {
  if (!require("lme4")) install.packages("lme4")
  if (!require("data.table")) install.packages("data.table")

  library(lme4)
  library(data.table)

  # --- 1. Fast Data Prep ---
  dt <- as.data.table(data)
  dt <- dt[, .(
    P = as.factor(get(participant)),
    S = as.factor(get(stimulus)),
    R = as.numeric(as.character(get(response)))
  )]
  dt <- dt[!is.na(R)]

  # --- 2. High-Speed Outlier Detection (Expected Value Logic) ---
  num_outliers <- 0
  if (!is.null(outlier_sd)) {
    dt[, mu_S := mean(R), by = S]
    dt[, mu_P := mean(R), by = P]
    grand_mu <- mean(dt$R)

    dt[, expected := mu_S + mu_P - grand_mu]
    dt[, resid := R - expected]

    sd_resid <- sd(dt$resid)
    outlier_idx <- abs(dt$resid) > (outlier_sd * sd_resid)
    num_outliers <- sum(outlier_idx)

    if (num_outliers > 0) {
      dt <- dt[!outlier_idx]
      message(paste("Removed", num_outliers, "outliers based on expected value residuals."))
    }
    dt[, c("mu_S", "mu_P", "expected", "resid") := NULL]
  }

  # --- 3. Harmonic k ---
  counts <- dt[, .N, by = S]$N
  k_harm <- length(counts) / sum(1 / counts)

  # --- 4. Models ---
  tryCatch(
    {
      m_main <- lmer(R ~ 1 + (1 | S) + (1 | P), data = dt)
      vc <- as.data.frame(VarCorr(m_main))

      v_S <- vc$vcov[vc$grp == "S"]
      v_P <- vc$vcov[vc$grp == "P"]
      v_resid <- vc$vcov[vc$grp == "Residual"]

      m_oneside <- lmer(R ~ 1 + (1 | S), data = dt)
      v_S1 <- as.data.frame(VarCorr(m_oneside))$vcov[1]
      v_resid1 <- as.data.frame(VarCorr(m_oneside))$vcov[2]

      # --- 5. ICCs ---
      icc1k <- v_S1 / (v_S1 + (v_resid1 / k_harm)) # ICC(1,k) one-way random
      icc2k <- v_S / (v_S + (v_P + v_resid) / k_harm) # ICC(2,k) two-way random
      icc3k <- v_S / (v_S + (v_resid / k_harm)) # ICC(3,k) two-way mixed

      # --- 6. Stimulus Scores (BLUPs) ---
      grand_mean <- fixef(m_main)["(Intercept)"]
      blups <- ranef(m_main)$S

      stim_stats <- dt[, .(N = as.integer(.N), Raw_Mean = mean(R)), by = .(Stimulus = S)]
      blup_table <- data.table(Stimulus = rownames(blups), BLUP = blups[, 1] + grand_mean)
      final_stimuli <- merge(stim_stats, blup_table, by = "Stimulus")
      setcolorder(final_stimuli, c("Stimulus", "N", "Raw_Mean", "BLUP"))

      return(list(
        ICCs = data.frame(
          Measure = c("ICC1k", "ICC2k", "ICC3k"),
          Value = c(icc1k, icc2k, icc3k), k_Harmonic = k_harm
        ),
        Stimuli = final_stimuli,
        Outliers_Removed = num_outliers
      ))
    },
    error = function(e) {
      stop("Model fitting failed: ", e$message)
    }
  )
}

# -----------------------------
# Data: observed cleaned study data
# -----------------------------
data_dir <- "clean_data"
input_files <- list.files(data_dir, pattern = "anonymized_final_data_precleaned_.*\\.csv$", full.names = TRUE)
if (length(input_files) == 0) stop("No cleaned data file found in clean_data/")
file_path <- input_files[which.max(file.info(input_files)$mtime)]
plog("Using input file: ", file_path)

# RT reliability is computed on CORRECT trials, trimmed to 200-4000 ms.
glp <- data.table::fread(file_path,
  select = c("subject_id", "word", "rt", "type", "accuracy"),
  showProgress = FALSE
) |>
  as.data.frame() |>
  dplyr::filter(!is.na(subject_id), !is.na(word), !is.na(rt), !is.na(type)) |>
  dplyr::filter(accuracy == "correct", rt >= 200, rt <= 4000) |>
  dplyr::mutate(
    log_rt     = log(rt),
    type       = as.character(type),
    word       = as.factor(word),
    subject_id = as.factor(subject_id)
  )

n_part <- length(unique(glp$subject_id))
n_item <- length(unique(glp$word))
plog("Data loaded -- rows: ", nrow(glp), " | participants: ", n_part, " | items: ", n_item)

# -----------------------------
# Define 3 analysis conditions (consistent naming)
# -----------------------------
make_conditions <- function(df) {
  list(
    overall     = df,
    words_only  = dplyr::filter(df, type == "word"),
    pseudo_only = dplyr::filter(df, type == "pseudoword")
  )
}

conds <- make_conditions(glp)

# -----------------------------
# APPROACH 1: mixed-model ICC-like item reliability   (~ ICC3k)
# -----------------------------
reliability_approach1 <- function(df) {
  m <- lmer(log_rt ~ 1 + (1 | word) + (1 | subject_id), data = df, REML = FALSE)

  vc <- as.data.frame(VarCorr(m))

  sigma_item <- vc[vc$grp == "word", "vcov"] |> as.numeric()
  sigma_error <- vc[vc$grp == "Residual", "vcov"] |> as.numeric()

  # average observations per item (word) in THIS condition
  n_per_word <- table(df$word)
  n_avg <- round(mean(n_per_word), 0)

  icc_item <- sigma_item / (sigma_item + sigma_error / n_avg)

  list(
    reliability_current = as.numeric(icc_item),
    n_current = length(unique(df$subject_id))
  )
}

# -----------------------------
# APPROACH 3: split-half over participants (Spearman-Brown corrected)   (~ ICC3k)
# -----------------------------
split_half_reliability <- function(df, n_samp = 200, base_seed = 10000) {
  # data.table aggregation: fast group means over tens of thousands of items,
  # so the resampling runs serially in seconds (no parallel cluster needed).
  dt <- data.table::as.data.table(df)[, .(subject_id, word, log_rt)]
  participants <- unique(dt$subject_id)
  if (length(participants) < 4) stop("Too few participants for split-half reliability.")
  half_n <- floor(length(participants) / 2)

  sb <- numeric(n_samp)
  items_used <- integer(n_samp)
  for (s in seq_len(n_samp)) {
    set.seed(base_seed + s)
    A_ids <- sample(participants, size = half_n, replace = FALSE)
    inA <- dt$subject_id %in% A_ids

    means_A <- dt[inA, .(mean_A = mean(log_rt, na.rm = TRUE)), by = word]
    means_B <- dt[!inA, .(mean_B = mean(log_rt, na.rm = TRUE)), by = word]
    merged <- merge(means_A, means_B, by = "word")

    r <- suppressWarnings(cor(merged$mean_A, merged$mean_B, method = "spearman"))
    sb[s] <- (2 * r) / (1 + r)
    items_used[s] <- nrow(merged)
  }

  list(
    reliability_current = mean(sb, na.rm = TRUE),
    n_current = length(participants),
    sd_sb = sd(sb, na.rm = TRUE),
    ci_low = quantile(sb, 0.025, na.rm = TRUE),
    ci_high = quantile(sb, 0.975, na.rm = TRUE),
    mean_items_used = mean(items_used)
  )
}

reliability_approach3 <- function(df, n_samp) {
  out <- split_half_reliability(df, n_samp = n_samp)
  list(
    reliability_current = out$reliability_current,
    n_current = out$n_current,
    sd_sb = out$sd_sb
  )
}

# -----------------------------
# Brysbaert ICC1k / ICC2k / ICC3k (+ BLUPs), computed on log_rt
# -----------------------------
reliability_brysbaert <- function(df, outlier_sd = 3) {
  ICC_stimulus_long(
    participant = "subject_id",
    stimulus    = "word",
    response    = "log_rt",
    data        = as.data.frame(df),
    outlier_sd  = outlier_sd
  )
}

# -----------------------------
# Run all feasible methods x conditions (on the observed data, no projection)
# -----------------------------
# Approach 1
res_a1 <- imap_dfr(conds, function(df_cond, cond_name) {
  plog("A1 mixed ICC: ", cond_name)
  out <- reliability_approach1(df_cond)
  tibble(
    condition = cond_name, approach = "approach_1_mixed_icc",
    reliability_current = out$reliability_current, n_current = out$n_current
  )
})

# Approach 3 (split-half)
res_a3 <- imap_dfr(conds, function(df_cond, cond_name) {
  plog("A3 split-half (", n_samp, " iters): ", cond_name)
  out <- reliability_approach3(df_cond, n_samp = n_samp)
  tibble(
    condition = cond_name, approach = "approach_3_split_half",
    reliability_current = out$reliability_current, n_current = out$n_current
  )
})

# Brysbaert ICC1k/2k/3k (+ collect BLUPs)
res_icc_list <- list()
blups_list <- list()
for (cond_name in names(conds)) {
  plog("Brysbaert ICCs: ", cond_name)
  df_cond <- conds[[cond_name]]
  b <- reliability_brysbaert(df_cond, outlier_sd = OUTLIER_SD)
  res_icc_list[[cond_name]] <- tibble(
    condition = cond_name,
    approach = paste0("brysbaert_", tolower(b$ICCs$Measure)), # brysbaert_icc1k/2k/3k
    reliability_current = b$ICCs$Value,
    n_current = length(unique(df_cond$subject_id))
  )
  blups_list[[cond_name]] <- as.data.frame(b$Stimuli) |> dplyr::mutate(condition = cond_name)
}
res_icc <- bind_rows(res_icc_list)

approach_levels <- c(
  "approach_1_mixed_icc", "approach_3_split_half",
  "brysbaert_icc1k", "brysbaert_icc2k", "brysbaert_icc3k"
)

res_current <- bind_rows(res_a1, res_a3, res_icc) %>%
  mutate(
    condition = factor(condition, levels = c("overall", "words_only", "pseudo_only")),
    approach  = factor(approach, levels = approach_levels)
  ) %>%
  arrange(condition, approach)

print(as.data.frame(res_current), digits = 4)

# Wide comparison (one number per method x condition)
res_current_wide <- res_current %>%
  dplyr::select(condition, approach, reliability_current) %>%
  tidyr::pivot_wider(names_from = condition, values_from = reliability_current)
cat("\n--- Current reliability by method x condition (observed data) ---\n")
print(as.data.frame(res_current_wide), digits = 4)

# -----------------------------
# ONE barplot comparing the methods (conditions as grouped bars; no projection)
# -----------------------------
# Approach labels A1-A5 with short method descriptions:
nice_labels <- c(
  approach_3_split_half = "A1: classic split-half SB",
  approach_1_mixed_icc  = "A2: Sascha's ICC3k",
  brysbaert_icc1k       = "A3: Marc's ICC1k\n(conservative)",
  brysbaert_icc2k       = "A4: Marc's ICC2k\n(generalisability)",
  brysbaert_icc3k       = "A5: Marc's ICC3k\n(= Cronbach alpha)"
)

p <- ggplot(res_current, aes(x = approach, y = reliability_current, fill = condition)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.72) +
  geom_text(aes(label = sprintf("%.3f", reliability_current)),
    position = position_dodge(width = 0.8), vjust = -0.4, size = 2.6
  ) +
  geom_hline(yintercept = 0.8, linetype = "dashed", color = "grey50") +
  scale_x_discrete(labels = nice_labels) +
  scale_fill_brewer(palette = "Set2") +
  coord_cartesian(ylim = c(0.70, 1.0)) +
  labs(x = NULL, y = "Item-level reliability", fill = "Condition") +
  theme_classic() +
  theme(legend.position = "top", axis.text.x = element_text(size = 8))

print(p)
ggsave(here::here("output/plots/reliability_methods_comparison.png"), p, width = 9.5, height = 5, dpi = 300)
ggsave(here::here("output/plots/reliability_methods_comparison.pdf"), p, width = 9.5, height = 5)

# -----------------------------
# Save tables + BLUPs
# -----------------------------
data.table::fwrite(res_current, here::here("output/summary/reliability_methods_comparison.csv"))
for (cond_name in names(blups_list)) {
  data.table::fwrite(
    blups_list[[cond_name]],
    here::here(paste0("output/summary/reliability_blups_", cond_name, ".csv"))
  )
}
plog("DONE -- wrote comparison table, barplot, and BLUP files")
