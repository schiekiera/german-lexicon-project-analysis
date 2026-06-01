rm(list = ls())

# -----------------------------
# Libraries
# -----------------------------
library(readr)
library(dplyr)
library(tidyr)
library(purrr)
library(ggplot2)
library(lme4)
library(nlme)
library(multilevel)
library(parallel)
library(pbapply)

pboptions(type = "txt")

set.seed(12345)

# -----------------------------
# Inputs
# -----------------------------
file_path <- "output/shared_anonymized_data_precleaned_2026-03-03_09-51-51.csv"

targets <- c(1440, 1680, 1920, 2160)  # projections in participant counts

n_samp  <- 1000                       # split-half iterations
n_cores <- max(1, detectCores() - 1)  # for split-half parallelism

# -----------------------------
# Load + clean
# -----------------------------
glp <- read_csv(file_path, show_col_types = FALSE) |>
  dplyr::select(anon_id, word, rt, type) |>
  filter(!is.na(anon_id), !is.na(word), !is.na(rt), !is.na(type)) |>
  filter(rt >= 200, rt <= 4000) |>
  mutate(
    log_rt = log(rt),
    type   = as.character(type),
    word   = as.factor(word),
    anon_id = as.factor(anon_id)
  )

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
# Helper: Spearman–Brown projection as function of participant N
# -----------------------------
sb_project <- function(r_current, n_current, targets) {
  tibble(participants = targets) |>
    mutate(
      k = participants / n_current,
      reliability_projected = (k * r_current) / (1 + (k - 1) * r_current)
    )
}

# -----------------------------
# APPROACH 1: mixed-model ICC-like item reliability
# -----------------------------
reliability_approach1 <- function(df) {
  # Minimal model with word + anon random intercepts
  m <- lmer(log_rt ~ 1 + (1 | word) + (1 | anon_id), data = df, REML = FALSE)
  
  vc <- as.data.frame(VarCorr(m))
  
  sigma_item  <- vc[vc$grp == "word", "vcov"] |> as.numeric()
  sigma_error <- vc[vc$grp == "Residual", "vcov"] |> as.numeric()
  
  # average observations per item (word) in THIS condition
  n_per_word <- table(df$word)
  n_avg <- round(mean(n_per_word), 0)
  
  icc_item <- sigma_item / (sigma_item + sigma_error / n_avg)
  
  list(
    reliability_current = as.numeric(icc_item),
    n_current = length(unique(df$anon_id))
  )
}

# -----------------------------
# APPROACH 2: ICC2.lme2 / gmeanrel
# -----------------------------
ICC2.lme2 <- function(dv, grp, data, weighted = FALSE) {
  dv  <- data %>% dplyr::select({{ dv }})  %>% purrr::reduce(c)
  grp <- data %>% dplyr::select({{ grp }}) %>% purrr::reduce(c) %>% factor()
  
  mod <- lme(
    dv ~ 1,
    random = list(~1 | grp),
    na.action = na.omit,
    control = lmeControl(maxIter = 100, msMaxIter = 100, msMaxEval = 400, opt = "optim")
  )
  
  gr <- gmeanrel(mod)
  
  if (!weighted) {
    icc2 <- mean(gr$MeanRel)
  } else {
    icc2 <- weighted.mean(gr$MeanRel, gr$GrpSize)
  }
  
  icc2
}

reliability_approach2 <- function(df) {
  r <- ICC2.lme2(log_rt, word, data = df, weighted = FALSE)
  list(
    reliability_current = as.numeric(r),
    n_current = length(unique(df$anon_id))
  )
}

# -----------------------------
# APPROACH 3: split-half over participants (Spearman–Brown corrected)
# -----------------------------
split_half_reliability <- function(df, n_samp = 1000, n_cores = 1, base_seed = 10000) {
  participants <- unique(df$anon_id)
  
  if (length(participants) < 4) stop("Too few participants for split-half reliability.")
  
  sb_iteration <- function(seed = NULL) {
    if (!is.null(seed)) set.seed(seed)
    
    half_n <- floor(length(participants) / 2)
    A_ids  <- sample(participants, size = half_n, replace = FALSE)
    B_ids  <- setdiff(participants, A_ids)
    
    means_A <- df %>%
      filter(anon_id %in% A_ids) %>%
      group_by(word) %>%
      summarise(mean_A = mean(log_rt, na.rm = TRUE), .groups = "drop")
    
    means_B <- df %>%
      filter(anon_id %in% B_ids) %>%
      group_by(word) %>%
      summarise(mean_B = mean(log_rt, na.rm = TRUE), .groups = "drop")
    
    merged <- inner_join(means_A, means_B, by = "word")
    
    r <- suppressWarnings(cor(merged$mean_A, merged$mean_B, method = "spearman"))
    sb <- (2 * r) / (1 + r)
    
    tibble(r = r, sb = sb, n_items_used = nrow(merged))
  }
  
  # parallel PSOCK cluster (cross-platform)
  cl <- makeCluster(n_cores)
  on.exit(stopCluster(cl), add = TRUE)
  
  clusterExport(cl, varlist = c("df", "participants", "sb_iteration"), envir = environment())
  invisible(clusterEvalQ(cl, { library(dplyr) }))
  
  res <- pblapply(
    X = 1:n_samp,
    FUN = function(s) sb_iteration(seed = base_seed + s),
    cl = cl
  ) %>%
    list_rbind() %>%
    mutate(split = row_number())
  
  res_summ <- res %>%
    summarise(
      reliability_current = mean(sb, na.rm = TRUE),
      sd_sb   = sd(sb, na.rm = TRUE),
      ci_low  = quantile(sb, 0.025, na.rm = TRUE),
      ci_high = quantile(sb, 0.975, na.rm = TRUE),
      mean_items_used = mean(n_items_used, na.rm = TRUE),
      .groups = "drop"
    )
  
  list(
    reliability_current = as.numeric(res_summ$reliability_current),
    n_current = length(unique(df$anon_id)),
    split_details = res,
    split_summary = res_summ
  )
}

reliability_approach3 <- function(df, n_samp, n_cores) {
  out <- split_half_reliability(df, n_samp = n_samp, n_cores = n_cores)
  list(
    reliability_current = out$reliability_current,
    n_current = out$n_current,
    split_summary = out$split_summary
  )
}

# -----------------------------
# Run all conditions × approaches
# -----------------------------
approaches <- list(
  approach_1_mixed_icc  = reliability_approach1,
  approach_2_gmeanrel   = reliability_approach2
  # approach 3 handled separately due to parallel + more output
)

# Approach 1 + 2
res_12 <- imap_dfr(conds, function(df_cond, cond_name) {
  map_dfr(names(approaches), function(a_name) {
    out <- approaches[[a_name]](df_cond)
    tibble(
      condition = cond_name,
      approach  = a_name,
      reliability_current = out$reliability_current,
      n_current = out$n_current
    )
  })
})

# Approach 3 (split-half) per condition
res_3 <- imap_dfr(conds, function(df_cond, cond_name) {
  out <- reliability_approach3(df_cond, n_samp = n_samp, n_cores = n_cores)
  tibble(
    condition = cond_name,
    approach  = "approach_3_split_half",
    reliability_current = out$reliability_current,
    n_current = out$n_current
  )
})

res_current <- bind_rows(res_12, res_3) %>%
  mutate(
    condition = factor(condition, levels = c("overall", "words_only", "pseudo_only")),
    approach  = factor(approach, levels = c("approach_1_mixed_icc", "approach_2_gmeanrel", "approach_3_split_half"))
  )

# -----------------------------
# Projections (tidy long table: one row per target)
# -----------------------------
res_proj <- res_current %>%
  dplyr::select(condition, approach, reliability_current, n_current) %>%
  dplyr::group_by(condition, approach) %>%
  dplyr::group_modify(~{
    sb_project(
      r_current = .x$reliability_current[1],
      n_current = .x$n_current[1],
      targets   = targets
    )
  }) %>%
  dplyr::ungroup()

# Final tidy table requested:
# - current: one row per condition × approach
# - projected: one row per condition × approach × target
results_tidy <- res_current %>%
  dplyr::select(condition, approach, reliability_current, n_current) %>%
  left_join(res_proj, by = c("condition", "approach")) %>%
  # keep only relevant columns (and keep n_current for the marker)
  dplyr::select(condition, approach, n_current, reliability_current, participants, reliability_projected) %>%
  arrange(condition, approach, participants)

print(results_tidy)

# -----------------------------
# One plot: current + projections
# -----------------------------
# We'll plot projected reliability vs participant targets (lines/points),
# and overlay the current estimate at current N.
x_breaks <- c(1102, 1440, 1680, 1920, 2160)

p <- ggplot() +
  geom_line(
    data = results_tidy,
    aes(x = participants, y = reliability_projected, color = approach, group = approach)
  ) +
  geom_point(
    data = results_tidy,
    aes(x = participants, y = reliability_projected, color = approach),
    size = 2
  ) +
  geom_point(
    data = res_current,
    aes(x = n_current, y = reliability_current, color = approach),
    shape = 21, fill = "white", size = 3, stroke = 1
  ) +
  geom_hline(yintercept = 0.85, linetype = "dashed", color = "grey50") +
  facet_wrap(~ condition) +
  scale_x_continuous(breaks = x_breaks) +
  labs(
    x = "Number of participants (N)",
    y = "Item-level reliability",
    color = "Approach",
    title = "Current reliability and Spearman–Brown projections (3 approaches × 3 conditions)"
  ) +
  theme_classic() +
  theme(legend.position = "top")

print(p)
ggsave("output/plots/reliability_projections_all_conditions.png", p, width = 10, height = 4, dpi = 300)
ggsave("output/plots/reliability_projections_all_conditions.pdf", p)

# Also save the tidy results table for convenience
write_csv(results_tidy, "output/log/reliability_results_tidy.csv")