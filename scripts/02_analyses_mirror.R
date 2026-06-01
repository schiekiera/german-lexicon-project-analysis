#!/usr/bin/env Rscript

# Lexical Decision RT and Accuracy Analysis Script
# Date: 2026-02-26

# ============================================================
# 1. Setup
# ============================================================
# 1.1 Workspace and libraries
rm(list = ls())

library(tidyverse)
library(readr)
library(stringr)
library(scales)
library(ggplot2)
library(dePlzMap)
library(here)

# 1.2 Run metadata
run_timestamp <- format(Sys.time(), "%Y-%m-%d_%H-%M-%S")

# ============================================================
# 2. Paths and Logging
# ============================================================
# 2.1 Output directories (local + mirror)
output_dir <- "output"
data_dir <- "clean_data"
summary_dir <- file.path(output_dir, "summary")
plots_dir <- file.path(output_dir, "plots")
log_dir <- file.path(output_dir, "log")

summary_dir_presentation <- here::here("output/summary")
plots_dir_presentation <- here::here("output/plots")
log_dir_presentation <- here::here("output/log")

# 2.2 Ensure directories exist
for (d in c(summary_dir, plots_dir, log_dir, summary_dir_presentation, plots_dir_presentation, log_dir_presentation)) {
  if (!dir.exists(d)) dir.create(d, recursive = TRUE, showWarnings = FALSE)
}



# 2.3 Dual log files (both log directories)
log_file_local <- file.path(log_dir, paste0("analysis_log_", run_timestamp, ".log"))
log_file_presentation <- file.path(log_dir_presentation, paste0("analysis_log_", run_timestamp, ".log"))

log_line <- function(level, ...) {
  line <- sprintf(
    "[%s] [%s] %s",
    format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
    level,
    paste(..., collapse = " ")
  )
  cat(line, "\n")
  write(line, file = log_file_local, append = TRUE)
  write(line, file = log_file_presentation, append = TRUE)
}

log_stage <- function(...) log_line("STAGE", ...)
log_info <- function(...) log_line("INFO", ...)
log_table <- function(title, x, useNA = "ifany") {
  tbl <- as.data.frame(table(x, useNA = useNA), stringsAsFactors = FALSE)
  names(tbl) <- c("value", "n")
  log_info(title)
  for (i in seq_len(nrow(tbl))) {
    log_info(" -", tbl$value[i], ":", tbl$n[i])
  }
}

# 2.4 Save helpers (write to both local + mirror)
write_csv_both <- function(df, filename, local_dir = summary_dir, mirror_dir = summary_dir_presentation) {
  local_path <- file.path(local_dir, filename)
  mirror_path <- file.path(mirror_dir, filename)
  write_csv(df, local_path, na = "")
  write_csv(df, mirror_path, na = "")
  log_info("Saved CSV:", local_path)
  log_info("Saved CSV mirror:", mirror_path)
}

save_plot_both <- function(plot_obj, filename, width = 9, height = 6, local_dir = plots_dir, mirror_dir = plots_dir_presentation) {
  local_path <- file.path(local_dir, filename)
  mirror_path <- file.path(mirror_dir, filename)
  ggsave(local_path, plot_obj, width = width, height = height)
  ggsave(mirror_path, plot_obj, width = width, height = height)
  log_info("Saved plot:", local_path)
  log_info("Saved plot mirror:", mirror_path)
}

save_base_plot_both <- function(plot_fn, filename, width = 9, height = 7, local_dir = plots_dir, mirror_dir = plots_dir_presentation) {
  local_path <- file.path(local_dir, filename)
  mirror_path <- file.path(mirror_dir, filename)

  save_one <- function(path) {
    if (capabilities("cairo")) {
      grDevices::cairo_pdf(path, width = width, height = height, family = "sans")
    } else {
      grDevices::pdf(path, width = width, height = height, family = "Helvetica")
    }
    plot_obj <- plot_fn()
    if (inherits(plot_obj, c("ggplot", "grob", "gtable", "patchwork"))) {
      print(plot_obj)
    }
    grDevices::dev.off()
  }

  save_one(local_path)
  save_one(mirror_path)
  log_info("Saved plot:", local_path)
  log_info("Saved plot mirror:", mirror_path)
}

log_stage("2. Paths and logging initialized")
log_info("Run timestamp:", run_timestamp)
log_info("Local log file:", log_file_local)
log_info("Mirror log file:", log_file_presentation)

# ============================================================
# 3. Load Data
# ============================================================
# 3.1 Locate latest merged dataset (final precleaned for all analyses)
log_stage("3. Load data")
input_files <- list.files(data_dir, pattern = "anonymized_final_data_precleaned_.*\\.csv$", full.names = TRUE)
if (length(input_files) == 0) stop("No cleaned data file found in output/")

latest_file <- input_files[which.max(file.info(input_files)$mtime)]
log_info("Using input file:", latest_file)

# 3.2 Read
df <- read_csv(latest_file, show_col_types = FALSE)
log_info("Rows loaded:", nrow(df))
log_info("Participants loaded:", dplyr::n_distinct(df$participant_id))

# 3.3 Load snapshot after step 3 (used only for accuracy histogram)
snapshot_files <- list.files(output_dir, pattern = "snapshot_after_step3_.*\\.csv$", full.names = TRUE)
if (length(snapshot_files) == 0) stop("No snapshot_after_step3 file found in output/")

latest_snapshot_file <- snapshot_files[which.max(file.info(snapshot_files)$mtime)]
log_info("Using snapshot file for accuracy histogram:", latest_snapshot_file)

df_snapshot <- read_csv(latest_snapshot_file, show_col_types = FALSE)
log_info("Snapshot rows loaded:", nrow(df_snapshot))
log_info("Snapshot participants loaded:", dplyr::n_distinct(df_snapshot$participant_id))

# ============================================================
# 4. Data Preparation
# ============================================================
# 4.1 Core analysis variables
log_stage("4. Prepare analysis dataset")
df <- df %>%
  mutate(
    age_raw = as.character(age),
    age_clean = parse_number(age_raw),
    age_clean = if_else(age_clean >= 16 & age_clean <= 100, age_clean, NA_real_),
    rt = suppressWarnings(as.numeric(rt)),
    block = suppressWarnings(as.integer(block)),
    accuracy_bin = case_when(
      tolower(as.character(accuracy)) == "correct" ~ 1,
      tolower(as.character(accuracy)) == "incorrect" ~ 0,
      TRUE ~ NA_real_
    ),
    rt_trimmed = if_else(!is.na(rt) & rt >= 200 & rt <= 3000, rt, NA_real_)
  )

participant_demo <- df %>% distinct(participant_id, .keep_all = TRUE)
log_info("Valid RT rows:", sum(!is.na(df$rt)))
log_info("Trimmed RT rows (200-3000ms):", sum(!is.na(df$rt_trimmed)))
log_info("Participants with valid age:", sum(!is.na(participant_demo$age_clean)))

# ============================================================
# 5. Summary Tables
# ============================================================
# 5.1 Overall
log_stage("5. Compute summary tables")
overall_summary <- df %>%
  summarise(
    n_trials = n(),
    n_participants = n_distinct(participant_id),
    rt_mean = mean(rt, na.rm = TRUE),
    rt_median = median(rt, na.rm = TRUE),
    rt_sd = sd(rt, na.rm = TRUE),
    rt_min = min(rt, na.rm = TRUE),
    rt_max = max(rt, na.rm = TRUE),
    rt_trimmed_mean = mean(rt_trimmed, na.rm = TRUE),
    rt_trimmed_median = median(rt_trimmed, na.rm = TRUE),
    rt_trimmed_sd = sd(rt_trimmed, na.rm = TRUE),
    rt_trimmed_min = min(rt_trimmed, na.rm = TRUE),
    rt_trimmed_max = max(rt_trimmed, na.rm = TRUE)
  )

# 5.2 Age and demographic summaries
age_summary <- participant_demo %>%
  summarise(
    n_participants = n(),
    n_valid_age = sum(!is.na(age_clean)),
    n_missing_or_invalid_age = sum(is.na(age_clean)),
    min_age = min(age_clean, na.rm = TRUE),
    q1_age = quantile(age_clean, 0.25, na.rm = TRUE),
    median_age = median(age_clean, na.rm = TRUE),
    mean_age = mean(age_clean, na.rm = TRUE),
    q3_age = quantile(age_clean, 0.75, na.rm = TRUE),
    max_age = max(age_clean, na.rm = TRUE),
    sd_age = sd(age_clean, na.rm = TRUE)
  )



# 5.3 RT and accuracy by type/block/participant
rt_by_type <- df %>%
  group_by(type) %>%
  summarise(
    n = n(),
    rt_mean = mean(rt, na.rm = TRUE),
    rt_median = median(rt, na.rm = TRUE),
    rt_sd = sd(rt, na.rm = TRUE),
    rt_trimmed_mean = mean(rt_trimmed, na.rm = TRUE),
    rt_trimmed_median = median(rt_trimmed, na.rm = TRUE),
    accuracy = mean(accuracy_bin, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(n))

accuracy_by_block <- df %>%
  group_by(block) %>%
  summarise(
    n = n(),
    n_participants = n_distinct(participant_id),
    accuracy = mean(accuracy_bin, na.rm = TRUE),
    rt_trimmed_mean = mean(rt_trimmed, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(block)

# 5.3.1 Within-person accuracy aggregation (requested table)
within_person_accuracy <- df %>%
  group_by(participant_id) %>%
  summarise(
    n_trials = n(),
    accuracy_mean = mean(accuracy_bin, na.rm = TRUE),
    accuracy_sd = sd(accuracy_bin, na.rm = TRUE),
    accuracy_median = median(accuracy_bin, na.rm = TRUE),
    accuracy_min = min(accuracy_bin, na.rm = TRUE),
    accuracy_max = max(accuracy_bin, na.rm = TRUE),
    mean_rt_trimmed = mean(rt_trimmed, na.rm = TRUE),
    median_rt_trimmed = median(rt_trimmed, na.rm = TRUE),
    sd_rt_trimmed = sd(rt_trimmed, na.rm = TRUE),
    .groups = "drop"
  )

# 5.3.2 Across-participants summary of within-person accuracy
within_person_accuracy_across_participants <- within_person_accuracy %>%
  summarise(
    n_participants = n(),
    accuracy_mean_across_participants = mean(accuracy_mean, na.rm = TRUE),
    accuracy_sd_across_participants = sd(accuracy_mean, na.rm = TRUE),
    accuracy_median_across_participants = median(accuracy_mean, na.rm = TRUE),
    accuracy_min_across_participants = min(accuracy_mean, na.rm = TRUE),
    accuracy_max_across_participants = max(accuracy_mean, na.rm = TRUE)
  )

# Alias used by downstream plot code
participant_summary <- within_person_accuracy %>%
  rename(mean_accuracy = accuracy_mean)

# Participant accuracy summary from step3 snapshot (only for accuracy histogram).
participant_summary_snapshot <- df_snapshot %>%
  mutate(
    accuracy_bin = case_when(
      tolower(as.character(accuracy)) == "correct" ~ 1,
      tolower(as.character(accuracy)) == "incorrect" ~ 0,
      TRUE ~ NA_real_
    )
  ) %>%
  group_by(participant_id) %>%
  summarise(
    mean_accuracy = mean(accuracy_bin, na.rm = TRUE),
    .groups = "drop"
  )

# Accuracy by type from snapshot data (post-step3, pre-step4).
accuracy_by_type_snapshot <- df_snapshot %>%
  mutate(
    accuracy_bin = case_when(
      tolower(as.character(accuracy)) == "correct" ~ 1,
      tolower(as.character(accuracy)) == "incorrect" ~ 0,
      TRUE ~ NA_real_
    )
  ) %>%
  filter(!is.na(type)) %>%
  group_by(type) %>%
  summarise(
    n = n(),
    n_participants = n_distinct(participant_id),
    accuracy = mean(accuracy_bin, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(n))

# Descriptive accuracy statistics based on snapshot data (after step 3).
snapshot_accuracy_descriptives <- participant_summary_snapshot %>%
  summarise(
    n_participants = n(),
    accuracy_mean_snapshot = mean(mean_accuracy, na.rm = TRUE),
    accuracy_sd_snapshot = sd(mean_accuracy, na.rm = TRUE),
    accuracy_median_snapshot = median(mean_accuracy, na.rm = TRUE),
    accuracy_min_snapshot = min(mean_accuracy, na.rm = TRUE),
    accuracy_max_snapshot = max(mean_accuracy, na.rm = TRUE)
  )


# 5.4 Write summaries to both summary directories
log_stage("5. Write summary tables")
write_csv_both(overall_summary, "summary_overall.csv")
write_csv_both(age_summary, "summary_age.csv")
write_csv_both(rt_by_type, "summary_rt_by_type.csv")
write_csv_both(accuracy_by_block, "summary_accuracy_by_block.csv")
write_csv_both(accuracy_by_type_snapshot, "summary_accuracy_by_type.csv")
write_csv_both(within_person_accuracy_across_participants, "summary_within_person_accuracy_across_participants.csv")
write_csv_both(snapshot_accuracy_descriptives, "summary_accuracy_snapshot_descriptives.csv")
log_info(
  "Snapshot accuracy descriptives:",
  paste(
    sprintf("n=%s", snapshot_accuracy_descriptives$n_participants),
    sprintf("mean=%.4f", snapshot_accuracy_descriptives$accuracy_mean_snapshot),
    sprintf("sd=%.4f", snapshot_accuracy_descriptives$accuracy_sd_snapshot),
    sprintf("median=%.4f", snapshot_accuracy_descriptives$accuracy_median_snapshot),
    sprintf("min=%.4f", snapshot_accuracy_descriptives$accuracy_min_snapshot),
    sprintf("max=%.4f", snapshot_accuracy_descriptives$accuracy_max_snapshot),
    collapse = ", "
  )
)



# Participant-level demographic frame (avoid trial-level double counting).
participant_demo <- df %>%
  distinct(participant_id, .keep_all = TRUE)

# 5.5 Demographic summaries

log_info("Demographic summaries")

# Gender
log_table("Gender (Geschlecht)", participant_demo$gender)

# Education
log_table("Education (Höchster Bildungsabschluss)", participant_demo$education)

# Handedness
participant_demo$handedness <- if_else(participant_demo$handedness == "Rechtshänder", "Rechtshänder:in", participant_demo$handedness)
log_table("Handedness (Händigkeit)", participant_demo$handedness)

# Native German
log_table("Native German (Haben Sie Deutsch als Muttersprache (vor dem 2. Lebensjahr) erworben?)", participant_demo$native_german)

# Other language early
log_table("Other language early (Haben Sie vor dem 2. Lebensjahr eine andere Sprache als Deutsch gelernt?)", participant_demo$other_lang_early)

# Multilingual
log_table("Multilingual (Sind Sie mehrsprachig (d.h., sprechen Sie mehr als eine Sprache)?)", participant_demo$multilingual)

# Language variety
log_table("Language variety (Welche ist Ihre vertrauteste Sprechweise?)", participant_demo$lang_variety)


# ============================================================
# 6. Plots
# ============================================================
log_stage("6. Build and save plots")

# 6.1 RT boxplot by type (no outlier points)
p_rt_box <- ggplot(
  df %>% filter(!is.na(rt_trimmed), !is.na(type)),
  aes(x = type, y = rt_trimmed, fill = type)
) +
  geom_boxplot(outlier.shape = NA, width = 0.6) +
  coord_cartesian(ylim = quantile(df$rt_trimmed, probs = c(0.01, 0.99), na.rm = TRUE)) +
  labs(x = "Type", y = "Reaction Time (ms)") +
  theme_minimal(base_size = 13) +
  theme(legend.position = "none")
save_plot_both(p_rt_box, "rt_boxplot_by_type.pdf")

# 6.2 RT violin+boxplot by type (no outlier points)
p_rt_violin <- ggplot(
  df %>% filter(!is.na(rt_trimmed), !is.na(type)),
  aes(x = type, y = rt_trimmed, fill = type)
) +
  geom_violin(trim = FALSE, alpha = 0.4) +
  geom_boxplot(width = 0.18, outlier.shape = NA) +
  labs(x = "Type", y = "Reaction Time (ms)") +
  theme_minimal(base_size = 13) +
  theme(legend.position = "none")
save_plot_both(p_rt_violin, "rt_violin_by_type.pdf")

# 6.3 Accuracy by type
p_acc_type <- df %>%
  filter(!is.na(accuracy_bin), !is.na(type)) %>%
  group_by(type) %>%
  summarise(accuracy = mean(accuracy_bin), n = n(), .groups = "drop") %>%
  ggplot(aes(x = type, y = accuracy, fill = type)) +
  geom_col(width = 0.7) +
  scale_y_continuous(labels = percent_format(accuracy = 1), limits = c(0, 1)) +
  labs(x = "Type", y = "Accuracy") +
  theme_minimal(base_size = 13) +
  theme(legend.position = "none")
save_plot_both(p_acc_type, "accuracy_by_type.pdf", width = 8, height = 6)

# 6.4 Accuracy by block
p_acc_block <- accuracy_by_block %>%
  ggplot(aes(x = block, y = accuracy)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  scale_x_continuous(breaks = sort(unique(accuracy_by_block$block))) +
  scale_y_continuous(labels = percent_format(accuracy = 1), limits = c(0.9, 0.95)) +
  labs(x = "Block", y = "Accuracy") +
  theme_minimal(base_size = 13)
save_plot_both(p_acc_block, "accuracy_by_block.pdf")

# 6.5 Mean reaction time by block
p_rt_block <- accuracy_by_block %>%
  ggplot(aes(x = block, y = rt_trimmed_mean)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  scale_x_continuous(breaks = sort(unique(accuracy_by_block$block))) +
  labs(x = "Block", y = "Mean Trimmed RT (ms)") +
  theme_minimal(base_size = 13)
save_plot_both(p_rt_block, "rt_by_block.pdf")

# 6.6 Participant speed-accuracy scatter
p_rt_acc <- participant_summary %>%
  filter(!is.na(mean_accuracy), !is.na(mean_rt_trimmed)) %>%
  ggplot(aes(x = mean_rt_trimmed, y = mean_accuracy)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = FALSE, linewidth = 0.9, color = "steelblue4") +
  scale_y_continuous(labels = percent_format(accuracy = 1), limits = c(0, 1)) +
  labs(x = "Mean RT (trimmed, ms)", y = "Mean Accuracy") +
  theme_minimal(base_size = 13)
save_plot_both(p_rt_acc, "participant_rt_accuracy_scatter.pdf")

# 6.7 RT histogram
p_rt_hist <- ggplot(df %>% filter(!is.na(rt_trimmed)), aes(x = rt_trimmed)) +
  geom_histogram(bins = 80, fill = "steelblue", color = "white", alpha = 0.9) +
  geom_vline(
    xintercept = mean(df$rt_trimmed, na.rm = TRUE),
    linewidth = 1
  ) +
    geom_vline(
    xintercept = median(df$rt_trimmed, na.rm = TRUE),
    linetype = "dashed",
    linewidth = 1  ) +
  scale_x_continuous(breaks = seq(0, max(df$rt_trimmed, na.rm = TRUE) + 200, 200)) +
  labs(
    x = "Reaction Time (ms)",
    y = "Trial Count"
  ) +
  theme_minimal(base_size = 13)
save_plot_both(p_rt_hist, "rt_histogram.pdf")

# 6.8 Accuracy histogram
p_acc_hist <- ggplot(participant_summary_snapshot %>% filter(!is.na(mean_accuracy)), aes(x = mean_accuracy)) +
  geom_histogram(bins = 40, fill = "darkseagreen3", color = "white", alpha = 0.9) +
  geom_vline(
    xintercept = mean(participant_summary_snapshot$mean_accuracy, na.rm = TRUE),
    linewidth = 0.9
  ) +
    geom_vline(
    xintercept = 0.75,
    linewidth = 0.9,
    color = "red"
  ) +
  scale_x_continuous(labels = percent_format(accuracy = 1), limits = c(0, 1), breaks = seq(0, 1, 0.1)) +
  labs(x = "Mean Accuracy", y = "Participant Count") +
  theme_minimal(base_size = 13)
save_plot_both(p_acc_hist, "accuracy_histogram.pdf")

# 6.9 Age histogram (participant-level)
p_age_hist <- ggplot(participant_demo %>% filter(!is.na(age_clean)), aes(x = age_clean)) +
  geom_histogram(
    binwidth = 1,
    boundary = 0,
    fill = "mediumpurple2",
    color = "white",
    alpha = 0.9
  ) +
  scale_x_continuous(breaks = pretty(participant_demo$age_clean, n = 15)) +
  labs(
    x = "Age (years)",
    y = "Participant Count"
  ) +
  theme_minimal(base_size = 13)
save_plot_both(p_age_hist, "age_histogram.pdf")

# 6.10 German PLZ map (participant origins, 2-digit prefixes)
participant_geo_base <- participant_demo %>%
  mutate(
    origin_region_clean = str_to_lower(as.character(origin_region)),
    origin_group = case_when(
      str_detect(origin_region_clean, "deutsch|german|\\bde\\b|\\bd\\b|brd|ddr|bundesrep|ostdeutsch") ~ "D",
      str_detect(origin_region_clean, "österreich|austria") ~ "A",
      str_detect(origin_region_clean, "schweiz") ~ "CH",
      TRUE ~ "other"
    )
  )

origin_group_counts <- participant_geo_base %>%
  count(origin_group, name = "n_participants") %>%
  arrange(desc(n_participants))
write_csv_both(origin_group_counts, "summary_origin_group_counts.csv")

participant_geo <- participant_geo_base %>%
  filter(origin_group == "D") %>%
  mutate(PLZ_2digit = str_extract(as.character(PLZ_origin_region), "\\d{2}"))

plz2_counts <- participant_geo %>%
  filter(!is.na(PLZ_2digit)) %>%
  count(PLZ_2digit, name = "n")

population_data <- read.table(
  system.file("extdata", "populationData.csv", package = "dePlzMap"),
  sep = ",",
  header = TRUE,
  colClasses = c("character", "integer", "character"),
  encoding = "UTF-8"
)

map_data <- population_data %>%
  transmute(
    plz = plz,
    PLZ_2digit = str_sub(plz, 1, 2)
  ) %>%
  left_join(plz2_counts, by = "PLZ_2digit") %>%
  mutate(n = replace_na(n, 0)) %>%
  select(plz, n)

log_info("Participants assigned to German origin map:", nrow(participant_geo))
log_info("Distinct 2-digit PLZ prefixes in sample:", dplyr::n_distinct(plz2_counts$PLZ_2digit))

save_base_plot_both(
  plot_fn = function() {
    dePlzMap::dePlzMap(
      data = map_data,
      legendTitle = "n (participants)"
    )
  },
  filename = "german_origin_plz_map.pdf",
  width = 9,
  height = 7
)

# ============================================================
# 7. Final Report and Logging
# ============================================================
log_stage("7. Final report")
log_info("Analysis complete.")
log_info("Summary directory (local):", summary_dir)
log_info("Summary directory (mirror):", summary_dir_presentation)
log_info("Plot directory (local):", plots_dir)
log_info("Plot directory (mirror):", plots_dir_presentation)
log_info("Log file (local):", log_file_local)
log_info("Log file (mirror):", log_file_presentation)

