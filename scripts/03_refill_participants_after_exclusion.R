# Lexical Decision Refill ID Mapping Script
# Maps excluded participants to global jsPsych IDs
# Date: 2025-12-XX

rm(list = ls())

library(tidyverse)
library(stringr)

script_start_time <- Sys.time()
run_timestamp_file <- format(script_start_time, "%Y-%m-%d_%H-%M-%S")

# ------------------------------------------------------------------
# Logging setup (same style as cleaning script)
# ------------------------------------------------------------------

log_dir <- file.path(getwd(), "output", "log")
dir.create(log_dir, recursive = TRUE, showWarnings = FALSE)

log_file <- file.path(log_dir,
                      paste0("refill_mapping_log_", run_timestamp_file, ".log"))

write_log_line <- function(x) {
  cat(x, "\n")
  cat(x, "\n", file = log_file, append = TRUE)
}

log_info <- function(...) {
  ts <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  write_log_line(sprintf("[%s] %s", ts, paste(..., collapse = " ")))
}

write_log_line("=== Refill ID Mapping Script ===")
log_info("Script started")

# ------------------------------------------------------------------
# DESIGN PARAMETERS (must match balancing script)
# ------------------------------------------------------------------

n_lists <- 30
n_participants_per_list <- 48
n_total_participants <- 1440

stopifnot(n_lists * n_participants_per_list == n_total_participants)

# ------------------------------------------------------------------
# Locate latest exclusions file
# ------------------------------------------------------------------

log_info("Searching for exclusions file in output/")

exclusion_files <- list.files(
  "output",
  pattern = "^exclusions_all_data_cleaned_.*\\.csv$",
  full.names = TRUE
)

if (length(exclusion_files) == 0) {
  stop("No exclusions_all_data_cleaned_ file found in output/")
}

latest_file <- exclusion_files[which.max(file.info(exclusion_files)$mtime)]
log_info("Using exclusions file:", latest_file)

# ------------------------------------------------------------------
# Read exclusions
# ------------------------------------------------------------------

exclusions <- read_csv(latest_file, show_col_types = FALSE)

if (!all(c("list", "participant_in_list") %in% names(exclusions))) {
  stop("Exclusions file must contain 'list' and 'participant_in_list'")
}

log_info("Total excluded trial rows:", nrow(exclusions))

# ------------------------------------------------------------------
# Extract unique participant slots
# ------------------------------------------------------------------

excluded_slots <- exclusions %>%
  distinct(list, participant_in_list) %>%
  mutate(
    list = as.integer(list),
    participant_in_list = as.integer(participant_in_list)
  )

log_info("Unique excluded participant slots:",
         nrow(excluded_slots))

# ------------------------------------------------------------------
# Compute global IDs
# ------------------------------------------------------------------

excluded_slots <- excluded_slots %>%
  mutate(
    global_id = (list - 1) * n_participants_per_list + participant_in_list
  )

# Safety checks
if (any(is.na(excluded_slots$global_id))) {
  stop("NA global IDs detected")
}

if (any(excluded_slots$global_id < 1 |
        excluded_slots$global_id > n_total_participants)) {
  stop("Computed global IDs outside valid range 1–1440")
}

# ------------------------------------------------------------------
# Ensure uniqueness (across refill waves)
# ------------------------------------------------------------------

ids_to_refill <- excluded_slots %>%
  distinct(global_id) %>%
  arrange(global_id)

log_info("Unique global IDs to refill:",
         nrow(ids_to_refill))

# ------------------------------------------------------------------
# Write output as .txt (one ID per line)
# ------------------------------------------------------------------

output_file <- file.path(
  "output",
  paste0("ids_to_refill_", run_timestamp_file, ".txt")
)

writeLines(
  as.character(ids_to_refill$global_id),
  output_file
)

log_info("Refill ID list written to:", output_file)
log_info("Script finished successfully")
