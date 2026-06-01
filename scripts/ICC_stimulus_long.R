# ==========================================================
# Function: ICC_stimulus_long
# Purpose: Calculates ICCs and BLUPs for stimuli in long-format data
# Usage:
#   source("ICC_stimulus_long.R")
#   results <- ICC_stimulus_long("SubjectID", "ItemID", "Score", my_df)
#
# PROVENANCE: Verbatim copy of the stand-alone function provided by
#   Marc Brysbaert (Ghent University), "Calculating the reliability of word
#   norms" (May 1, 2026). Original attached in
#   presentation/context/reliability/attached/mail3_ICC_stimulus_long.R
#   Kept here unmodified so the pipeline (05_reliability_icc.R) can source it.
#
# Returns a list with:
#   $ICCs            data.frame of ICC1k / ICC2k / ICC3k + harmonic k
#                      ICC1k = one-way random   (absolute agreement; conservative)
#                      ICC2k = two-way random   (generalisation to new raters)
#                      ICC3k = two-way mixed     (consistency; = Cronbach's alpha)
#   $Stimuli         per-stimulus N, Raw_Mean, and BLUP (shrunken estimate)
#   $Outliers_Removed number of 3-SD residual outliers dropped
# ==========================================================

ICC_stimulus_long <- function(participant, stimulus, response, data, outlier_sd = 3) {
  if (!require("lme4")) install.packages("lme4")
  if (!require("data.table")) install.packages("data.table")

  library(lme4)
  library(data.table)

  # --- 1. Fast Data Prep ---
  dt <- as.data.table(data)
  dt <- dt[, .(P = as.factor(get(participant)),
               S = as.factor(get(stimulus)),
               R = as.numeric(as.character(get(response))))]
  dt <- dt[!is.na(R)]

  # --- 2. High-Speed Outlier Detection (Expected Value Logic) ---
  num_outliers <- 0
  if (!is.null(outlier_sd)) {
    # Calculate components for expected value: E(y) = mu + alpha_i + beta_j
    dt[, mu_S := mean(R), by = S]
    dt[, mu_P := mean(R), by = P]
    grand_mu <- mean(dt$R)

    # Residual = Observed - (Stimulus_Mean + Participant_Mean - Grand_Mean)
    dt[, expected := mu_S + mu_P - grand_mu]
    dt[, resid := R - expected]

    sd_resid <- sd(dt$resid)
    outlier_idx <- abs(dt$resid) > (outlier_sd * sd_resid)
    num_outliers <- sum(outlier_idx)

    if (num_outliers > 0) {
      dt <- dt[!outlier_idx]
      message(paste("Removed", num_outliers, "outliers based on expected value residuals."))
    }
    # Clean up temp columns to save memory
    dt[, c("mu_S", "mu_P", "expected", "resid") := NULL]
  }

  # --- 3. Harmonic k ---
  counts <- dt[, .N, by = S]$N
  k_harm <- length(counts) / sum(1 / counts)

  # --- 4. Single Model Optimization ---
  # We only need ONE well-specified model to derive ICC2 and ICC3 components.
  # This saves ~60% of the processing time.
  tryCatch({
    # Two-way Random Effects Model
    m_main <- lmer(R ~ 1 + (1|S) + (1|P), data = dt)
    vc <- as.data.frame(VarCorr(m_main))

    v_S     <- vc$vcov[vc$grp == "S"]
    v_P     <- vc$vcov[vc$grp == "P"]
    v_resid <- vc$vcov[vc$grp == "Residual"]

    # One-way model is still needed for ICC1 specifically
    m_oneside <- lmer(R ~ 1 + (1|S), data = dt)
    v_S1      <- as.data.frame(VarCorr(m_oneside))$vcov[1]
    v_resid1  <- as.data.frame(VarCorr(m_oneside))$vcov[2]

    # --- 5. Calculate ICCs ---
    # ICC(1,k): One-way random
    icc1k <- v_S1 / (v_S1 + (v_resid1 / k_harm))

    # ICC(2,k): Two-way random (Agreement)
    icc2k <- v_S / (v_S + (v_P + v_resid) / k_harm)

    # ICC(3,k): Two-way mixed (Consistency)
    icc3k <- v_S / (v_S + (v_resid / k_harm))

    # --- 6. Stimulus Scores ---
    grand_mean <- fixef(m_main)["(Intercept)"]
    blups      <- ranef(m_main)$S

    # Calculate counts and raw means first
    stim_stats <- dt[, .(N = as.integer(.N), Raw_Mean = mean(R)), by = .(Stimulus = S)]

    # Create the BLUP table
    blup_table <- data.table(
      Stimulus = rownames(blups),
      BLUP = blups[,1] + grand_mean
    )

    # Merge and reorder columns: Stimulus, N, Raw_Mean, BLUP
    final_stimuli <- merge(stim_stats, blup_table, by = "Stimulus")
    setcolorder(final_stimuli, c("Stimulus", "N", "Raw_Mean", "BLUP"))

    return(list(
      ICCs = data.frame(
        Measure = c("ICC1k", "ICC2k", "ICC3k"),
        Value = c(icc1k, icc2k, icc3k),
        k_Harmonic = k_harm
      ),
      Stimuli = final_stimuli,
      Outliers_Removed = num_outliers
    ))

  }, error = function(e) {
    stop("Model fitting failed: ", e$message)
  })
}
