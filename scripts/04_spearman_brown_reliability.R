library(readr)
library(dplyr)
library(tidyr)
library(purrr)
library(parallel)
library(pbapply)
pboptions(type = "txt")

# -----------------------------
# Load and prepare data
# -----------------------------
cat("Loading data...\n")
glp <- read_csv("output/shared_anonymized_data_precleaned_2026-03-03_09-51-51.csv", show_col_types = FALSE) %>%
  # need participant id to split by participants
  select(anon_id, word, rt) %>%
  filter(!is.na(anon_id), !is.na(word), !is.na(rt)) %>%
  filter(rt >= 200, rt <= 4000) %>%          # apply your RT bounds (adjust if needed)
  mutate(log_rt = log(rt))


participants <- unique(glp$anon_id)

if (length(participants) < 4) stop("Too few participants for split-half reliability.")

# -----------------------------
# One split-half iteration
# -----------------------------
sb_iteration <- function(seed = NULL) {
  if (!is.null(seed)) set.seed(seed)

  # Split participants (global split)
  half_n <- floor(length(participants) / 2)
  A_ids  <- sample(participants, size = half_n, replace = FALSE)
  B_ids  <- setdiff(participants, A_ids)

  # Item means in each half
  means_A <- glp %>%
    filter(anon_id %in% A_ids) %>%
    group_by(word) %>%
    summarise(mean_A = mean(log_rt, na.rm = TRUE), .groups = "drop")

  means_B <- glp %>%
    filter(anon_id %in% B_ids) %>%
    group_by(word) %>%
    summarise(mean_B = mean(log_rt, na.rm = TRUE), .groups = "drop")

  merged <- inner_join(means_A, means_B, by = "word")

  # Correlate item mean vectors across words
  r <- suppressWarnings(cor(merged$mean_A, merged$mean_B, method = "spearman"))

  # Spearman–Brown correction (NO abs())
  sb <- (2 * r) / (1 + r)

  tibble(
    r = r,
    sb = sb,
    n_items_used = nrow(merged)
  )
}

# -----------------------------
# Run many splits (parallel)
# -----------------------------
n_samp <- 1000
n_cores <- max(1, detectCores() - 1)
cat("Starting split-half reliability:", n_samp, "iterations on", n_cores, "cores\n")

# Cross-platform parallel via PSOCK cluster
cl <- makeCluster(n_cores)
on.exit(stopCluster(cl), add = TRUE)

clusterExport(cl, varlist = c("glp", "participants", "sb_iteration"), envir = environment())
invisible(clusterEvalQ(cl, { library(dplyr); library(tidyr) }))

res <- pblapply(
  X = 1:n_samp,
  FUN = function(s) sb_iteration(seed = 10000 + s),
  cl = cl
) %>%
  list_rbind() %>%
  mutate(split = row_number())
cat("\nFinished split-half reliability runs.\n")

# -----------------------------
# Summaries + plot
# -----------------------------
res_summ <- res %>%
  summarise(
    mean_r  = mean(r, na.rm = TRUE),
    mean_sb = mean(sb, na.rm = TRUE),
    sd_sb   = sd(sb, na.rm = TRUE),
    ci_low  = quantile(sb, 0.025, na.rm = TRUE),
    ci_high = quantile(sb, 0.975, na.rm = TRUE),
    mean_items_used = mean(n_items_used)
)

print(res_summ)

plot(density(res$sb, na.rm = TRUE),
     main = "Split-half item-level reliability (Spearman–Brown corrected)",
     xlab = "Spearman–Brown reliability (sb)")


# -----------------------------
# Spearman-Brown projections
# -----------------------------

N_current <- 1102
r_current <- res_summ$mean_sb

targets <- c(
  1440,
  1440 + 240,
  1440 + 480,
  1440 + 720
)

projection <- tibble(
  participants = targets
) %>%
  mutate(
    k = participants / N_current,
    predicted_reliability = (k * r_current) / (1 + (k - 1) * r_current)
  )

print(projection)
