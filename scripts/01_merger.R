# Lexical Decision Data Cleaning and Merging Script
# Date: 2025-12-05

# This script reads, cleans, and merges lexical decision data exported as CSVs.
# Make sure this script is it the same folder with the 'data' folder containing the CSVs and you have created the 'ouput' folder for the cleaned data.

# Clear workspace
rm(list = ls())

# Load required libraries
library(tidyverse)
library(readr)
library(stringr)
library(purrr)
library(tidyr)
library(jsonlite)

script_start_time <- Sys.time()
stage_start_time <- Sys.time()
run_timestamp <- format(script_start_time, "%Y-%m-%d %H:%M:%S")
run_timestamp_file <- format(script_start_time, "%Y-%m-%d_%H-%M-%S")
time <- run_timestamp_file 

local_log_dir <- file.path(getwd(), "output", "log")
dir.create(local_log_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(file.path(getwd(), "raw_data"),   recursive = TRUE, showWarnings = FALSE)
dir.create(file.path(getwd(), "clean_data"), recursive = TRUE, showWarnings = FALSE)

local_log_file <- file.path(local_log_dir,
                            paste0("merger_log_", run_timestamp_file, ".log"))

write_log_line <- function(line_text) {
  cat(line_text, "\n", sep = "")
  cat(line_text,
      "\n",
      sep = "",
      file = local_log_file,
      append = TRUE)
}

format_duration <- function(start_time) {
  as.numeric(difftime(Sys.time(), start_time, units = "secs"))
}

log_info <- function(...) {
  ts <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  write_log_line(sprintf("[%s] [INFO] %s", ts, paste(..., collapse = " ")))
}

log_stage <- function(...) {
  ts <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  write_log_line("")
  write_log_line(sprintf("[%s] [STAGE] %s", ts, paste(..., collapse = " ")))
}

log_checkpoint <- function(label, start_time = stage_start_time) {
  log_info(label, sprintf("(elapsed: %.1fs)", format_duration(start_time)))
}

write_log_line("=== Lexical Decision Data Cleaning and Merging ===")
log_stage("Script started")
log_info("Evaluation start time:",
         format(script_start_time, "%Y-%m-%d %H:%M:%S"))
log_info("Working directory:", getwd())
log_info("Input directory:", file.path(getwd(), "data"))
log_info("Output prefix:", file.path(getwd(), "output"))
log_info("Local log file:", local_log_file)

# Function to extract participant ID from filename
extract_composite_id <- function(filename) {
  base <- basename(filename)
  pid  <- str_match(base, "_p(\\d+)_")[1, 2]
  uni  <- str_match(base, "glp_(.+?)_p\\d+_")[1, 2]
  if (!is.na(pid) && !is.na(uni))
    return(paste0(uni, "_", pid))
  return(NA_character_)
}

# Extract uni_pid without date — used to match block files to demo participants
extract_uni_pid <- function(filename) {
  base <- basename(filename)
  pid <- str_match(base, "_p(\\d+)_")[1, 2]
  uni <- str_match(base, "glp_(.+?)_p\\d+_")[1, 2]
  if (!is.na(pid) && !is.na(uni)) return(paste0(uni, "_", pid))
  return(NA_character_)
}

# Extract date component from filename timestamp
extract_file_date <- function(filename) {
  str_extract(basename(filename), "\\d{4}-\\d{2}-\\d{2}")
}

# Function to extract block number from filename
extract_block <- function(filename) {
  # match "_block<number>_" (note the trailing underscore!)
  m <- str_match(basename(filename), "_block(\\d+)_")
  if (is.na(m[1, 2])) {
    return(NA_integer_)
  } else {
    return(as.integer(m[1, 2]))
  }
}

# Function to extract timestamp from filename
extract_timestamp <- function(filename) {
  # Extract the part after the last underscore and before .csv
  timestamp <- str_extract(filename, "[^_]+(?=\\.csv$)")
  return(timestamp)
}

# Function to strip HTML tags from stimulus
strip_html <- function(text) {
  if (is.na(text) || is.null(text))
    return(text)
  str_remove_all(text, "<[^>]+>")
}


# Discover all CSV files in the current directory
stage_start_time <- Sys.time()
log_stage("Discovering CSV files")
all_files <- list.files("data",
                        pattern = "\\.csv$",
                        full.names = TRUE,
                        recursive = TRUE)

if (length(all_files) == 0) {
  stop("No CSV files found in the current directory!")
}

log_info("Found", length(all_files), "CSV files")

# CSVs with "_block<number>_" for experimental blocks 1–10
block_files_all <- all_files[str_detect(all_files, "_block(\\d+)_")]

# CSVs with block17demo for demographics
demo_files_all <- all_files[str_detect(all_files, "block11demo_")]

# Keep only parseable demo files and define the participant cohort from demos.
demo_raw_ids   <- map_chr(demo_files_all, extract_composite_id)
valid_demo_idx <- !is.na(demo_raw_ids)
demo_files_raw <- demo_files_all[valid_demo_idx]
demo_raw_ids   <- demo_raw_ids[valid_demo_idx]

# When the same uni_pid appears in multiple demo files, the participant_id
# column in the dataframe gets a _v1 / _v2 suffix (ordered by file date,
# earliest first). No files are renamed on disk.
# This handles genuine ID collisions (two different people at the same uni
# who received the same numeric ID) and restarts (same person, two demo
# files; the incomplete attempt is dropped at the missing-blocks step).
# Participants with a single demo file keep the plain uni_pid as their ID.
demo_id_tbl <- tibble(
  file    = demo_files_raw,
  uni_pid = demo_raw_ids
) %>%
  mutate(file_date = map_chr(file, extract_file_date)) %>%
  group_by(uni_pid) %>%
  arrange(file_date, .by_group = TRUE) %>%
  mutate(
    participant_id = if (n() > 1) paste0(uni_pid, "_v", row_number()) else uni_pid
  ) %>%
  ungroup()

demo_files           <- demo_id_tbl$file
demo_participant_ids <- demo_id_tbl$participant_id
demo_participants    <- unique(demo_participant_ids)

# Log and export duplicate uni_pid cases
dup_demo_details <- demo_id_tbl %>%
  group_by(uni_pid) %>%
  filter(n() > 1) %>%
  ungroup() %>%
  mutate(timestamp = map_chr(file, extract_timestamp)) %>%
  arrange(uni_pid, timestamp)

log_info("uni_pid values with multiple demo files:", n_distinct(dup_demo_details$uni_pid))
log_info("Affected uni_pids:", paste(unique(dup_demo_details$uni_pid), collapse = ", "))

write_csv(dup_demo_details, file.path("output", "duplicate_demo_files.csv"))

# Build demo lookup for date-proximity block matching.
# The composite ID (uni_pid_date) is anchored to the DEMO file's date.
# Block files are matched via uni_pid + closest date, so participants who
# completed blocks on a different day than their demo file are not lost.
demo_lookup <- tibble(
  participant_id = demo_participant_ids,
  file           = demo_files
) %>%
  mutate(
    uni_pid   = map_chr(file, extract_uni_pid),
    demo_date = map_chr(file, extract_file_date)
  ) %>%
  filter(!is.na(uni_pid))

block_file_tbl <- tibble(file = block_files_all) %>%
  mutate(
    uni_pid   = map_chr(file, extract_uni_pid),
    file_date = map_chr(file, extract_file_date)
  ) %>%
  filter(!is.na(uni_pid)) %>%
  left_join(
    demo_lookup %>% select(uni_pid, participant_id, demo_date),
    by = "uni_pid",
    relationship = "many-to-many"
  ) %>%
  filter(!is.na(participant_id)) %>%
  mutate(date_diff = abs(as.integer(
    as.Date(file_date) - as.Date(demo_date)
  ))) %>%
  group_by(file) %>%
  slice_min(date_diff, n = 1, with_ties = FALSE) %>%
  ungroup()

block_files     <- block_file_tbl$file
block_id_lookup <- setNames(block_file_tbl$participant_id, block_file_tbl$file)

log_info("All block files discovered:", length(block_files_all))
log_info("All demo files discovered:", length(demo_files_all))
log_info("Demo files with parseable participant IDs:", length(demo_files))
log_info("Participants defined by demo files:",
         length(demo_participants))
log_info("Block files linked to demo participants:", length(block_files))

# Documentation helper: block1 files with no matching demo participant.
block1_without_demo_files <- block_files_all[
  str_detect(block_files_all, "_block1_") &
  !block_files_all %in% block_file_tbl$file
]
block1_without_demo_ids <- unique(na.omit(
  map_chr(block1_without_demo_files, extract_uni_pid)
))
log_info("Participants with block1 file but no demo file:",
         length(block1_without_demo_ids))

log_checkpoint("File discovery complete")

# Participant cohort for all consistency checks and downstream processing.
all_participant_ids <- demo_participants

# cat("Participants found:",
#     paste(all_participant_ids, collapse = ", "),
#     "\n")
cat("Total participants:", length(all_participant_ids), "\n\n")

# Check for missing blocks per participant
stage_start_time <- Sys.time()
log_stage("Checking block completeness per participant")

# Vectorized approach - much faster
participant_block_summary <- block_file_tbl %>%
  mutate(block_num = map_int(file, extract_block)) %>%
  filter(!is.na(participant_id), !is.na(block_num)) %>%
  group_by(participant_id) %>%
  summarise(blocks_present = list(unique(block_num)), .groups = "drop") %>%
  mutate(
    missing_blocks = map(blocks_present, ~ setdiff(1:10, .x)),
    has_missing = map_lgl(missing_blocks, ~ length(.x) > 0)
  )

missing_participants <- participant_block_summary %>%
  filter(has_missing) %>%
  pull(participant_id)

missing_block_count <- length(missing_participants)

log_info("Participants checked:", length(all_participant_ids))
log_info("Participants missing >=1 block:", missing_block_count)
if (length(missing_participants) > 0) {
  log_info("Example participant IDs with missing blocks:",
           paste(head(missing_participants, 10), collapse = ", "))
}
log_checkpoint("Block completeness check complete")


# Exclude participants with demo but incomplete blocks from all downstream steps.
if (missing_block_count > 0) {
  warning(
    paste(
      "Detected",
      missing_block_count,
      "participants with incomplete blocks 1-10; these participants will be excluded from final output."
    )
  )
}

incomplete_participants <- unique(missing_participants)
complete_participants <- setdiff(all_participant_ids, incomplete_participants)
exclusions_ <- tibble()

log_stage("Participant exclusion summary")
log_info("Initial participants (demo-defined cohort):",
         length(all_participant_ids))
log_info("Excluded at step 1 (missing blocks 1-10):",
         length(incomplete_participants))
log_info("Retained after step 1:", length(complete_participants))

demo_file_participant_ids <- map_chr(demo_files, extract_composite_id)

# block_files are already pre-matched via block_file_tbl; no further filtering needed.
# Demo files: keep only those with a valid composite ID.
demo_files <- demo_files[!is.na(demo_file_participant_ids)]
log_info("Block files loaded before trial-level exclusions:",
         length(block_files))
log_info("Demo files loaded before trial-level exclusions:",
         length(demo_files))

stage_start_time <- Sys.time()
log_stage("Reading and combining block trial files")

trials_list <- vector("list", length(block_files))
processed_block_files <- 0L
skipped_block_files <- 0L
failed_block_files <- 0L
empty_block_files <- 0L

pb_blocks <- txtProgressBar(min = 0,
                            max = max(1, length(block_files)),
                            style = 3)

for (i in seq_along(block_files)) {
  file <- block_files[i]
  
  participant_id <- block_id_lookup[[file]]
  block_num      <- extract_block(file)
  timestamp      <- extract_timestamp(file)
  
  if (is.na(participant_id) || is.na(block_num)) {
    warning(paste("Skipping file due to parsing issues:", file))
    skipped_block_files <- skipped_block_files + 1L
    setTxtProgressBar(pb_blocks, i)
    next
  }
  
  data <- tryCatch(
    read_csv(
      file,
      show_col_types = FALSE,
      na = c("", "NA", "null", "NaN"),
      col_types = cols(sonaID = col_double(), .default = col_guess())
    ),
    error = function(e) {
      warning(paste("Error reading file", file, ":", e$message))
      failed_block_files <- failed_block_files + 1L
      return(NULL)
    }
  )
  
  if (is.null(data) || nrow(data) == 0) {
    empty_block_files <- empty_block_files + 1L
    setTxtProgressBar(pb_blocks, i)
    next
  }
  
  data$participant_id <- participant_id
  data$block <- block_num
  data$timestamp <- timestamp
  
  trials_list[[i]] <- data
  processed_block_files <- processed_block_files + 1L
  setTxtProgressBar(pb_blocks, i)
}
close(pb_blocks)
cat("\n")

# Bind once
trials_data <- bind_rows(trials_list)

log_info("Block files processed successfully:", processed_block_files)
log_info("Block files skipped (parse):", skipped_block_files)
log_info("Block files failed to read:", failed_block_files)
log_info("Block files empty:", empty_block_files)
log_info("Total trial rows before cleaning:", nrow(trials_data))
log_checkpoint("Block reading complete")

# Clean and process trial data
if (nrow(trials_data) > 0) {
  stage_start_time <- Sys.time()
  log_stage("Cleaning trial data")
  
  cols_present <- names(trials_data)
  
  # -----------------------------
  # 1. Strip HTML (vectorized)
  # -----------------------------
  if ("stimulus" %in% cols_present) {
    trials_data$stimulus <- stringr::str_remove_all(trials_data$stimulus, "<[^>]+>")
  }
  
  # -----------------------------
  # 2. Parse JSON only once
  # -----------------------------
  if ("log" %in% cols_present) {
    parsed_log <- purrr::map(trials_data$log, function(x) {
      if (is.na(x) || x == "")
        return(NULL)
      tryCatch(
        jsonlite::fromJSON(x),
        error = function(e)
          NULL
      )
    })
    
    `%||%` <- function(x, y)
      if (is.null(x))
        y
    else
      x
    
    trials_data$matched_word <- purrr::map_chr(parsed_log, ~ .x$matched_word %||% NA)
    trials_data$list <- purrr::map_chr(parsed_log, ~ .x$list %||% NA)
    trials_data$participant_in_list <- purrr::map_chr(parsed_log, ~ .x$participant_in_list %||% NA)
    
  } else {
    trials_data$matched_word <- NA_character_
    trials_data$list <- NA_character_
    trials_data$participant_in_list <- NA_character_
  }
  
  # -----------------------------
  # 3. Faster type conversions
  # -----------------------------
  
  if ("rt" %in% cols_present)
    trials_data$rt <- suppressWarnings(as.numeric(trials_data$rt))
  
  if ("time_elapsed" %in% cols_present)
    trials_data$time_elapsed <- suppressWarnings(as.numeric(trials_data$time_elapsed))
  
  if ("correct" %in% cols_present) {
    trials_data$correct <- tolower(as.character(trials_data$correct))
    trials_data$correct <- trials_data$correct == "true"
  }
  
  if ("trial_index" %in% cols_present)
    trials_data$trial_index <- as.integer(trials_data$trial_index)
  
  # -----------------------------
  # 4. Trim character columns (vectorized)
  # -----------------------------
  
  char_cols <- intersect(
    c(
      "word",
      "type",
      "response",
      "task",
      "trial_type",
      "key_mapping",
      "plugin_version"
    ),
    cols_present
  )
  
  if (length(char_cols) > 0) {
    trials_data[char_cols] <-
      lapply(trials_data[char_cols], stringr::str_trim)
  }
  
  # -----------------------------
  # 5. Remove duplicated rows
  # -----------------------------
  
  n_before <- nrow(trials_data)
  trials_data <- dplyr::distinct(trials_data, .keep_all = TRUE)
  n_after <- nrow(trials_data)
  
  if (n_before > n_after) {
    log_info("Removed", n_before - n_after, "duplicate trial rows")
  }
  log_info("Trial rows after cleaning:", nrow(trials_data))
  log_checkpoint("Trial data cleaning complete")
}

# Step 1 exclusions (missing blocks) are applied here so all excluded trials are retained in exclusions_.
stage_start_time <- Sys.time()
log_stage("Applying step 1 exclusion to trials")
if (length(incomplete_participants) > 0 && nrow(trials_data) > 0) {
  step1_exclusions <- trials_data %>%
    filter(participant_id %in% incomplete_participants) %>%
    mutate(
      exclusion_step = "step1_missing_blocks",
      exclusion_reason = "Participant has demo file but missing one or more blocks 1-10",
      exclusion_timestamp = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
      script_run_timestamp = run_timestamp
    )
  
  exclusions_ <- bind_rows(exclusions_, step1_exclusions)
  trials_data <- trials_data %>%
    filter(!participant_id %in% incomplete_participants)
}
log_info(
  "Step 1 excluded trial rows:",
  sum(exclusions_$exclusion_step == "step1_missing_blocks", na.rm = TRUE)
)
log_info("Trial rows retained after step 1:", nrow(trials_data))
log_checkpoint("Step 1 trial exclusion complete")

# Final participant exclusion step: keep only participants with >= 1250 trial rows.
stage_start_time <- Sys.time()
log_stage("Final participant exclusion by minimum trial count")

if (!("participant_id" %in% names(trials_data)) ||
    nrow(trials_data) == 0) {
  participant_trial_counts <- tibble(participant_id = character(), n_trials = integer())
} else {
  participant_trial_counts <- trials_data %>%
    count(participant_id, name = "n_trials")
}

low_trial_participants <- participant_trial_counts %>%
  filter(n_trials < 1250) %>%
  pull(participant_id)

if (length(low_trial_participants) > 0) {
  step2_exclusions <- trials_data %>%
    filter(participant_id %in% low_trial_participants) %>%
    mutate(
      exclusion_step = "step2_low_trial_count",
      exclusion_reason = "Participant has fewer than 1250 total trial rows",
      exclusion_timestamp = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
      script_run_timestamp = run_timestamp
    )
  
  exclusions_ <- bind_rows(exclusions_, step2_exclusions)
  
  trials_data <- trials_data %>%
    filter(!participant_id %in% low_trial_participants)
  
  demo_files <- demo_files[map_chr(demo_files, extract_composite_id) %in% unique(trials_data$participant_id)]
}

log_info("Excluded at step 2 (<1250 total trials):",
         length(low_trial_participants))
log_info("Retained after step 2:",
         dplyr::n_distinct(trials_data$participant_id))
log_info("Trial rows after step 2 exclusion:", nrow(trials_data))
log_info(
  "Step 2 excluded trial rows:",
  sum(exclusions_$exclusion_step == "step2_low_trial_count", na.rm = TRUE)
)
log_info("Total excluded trial rows (all steps):", nrow(exclusions_))
if (length(low_trial_participants) > 0) {
  log_info("Example excluded participant IDs (<1250 trials):",
           paste(head(low_trial_participants, 10), collapse = ", "))
}
log_checkpoint("Trial-count exclusion complete")

# Process demographics data
stage_start_time <- Sys.time()
log_stage("Reading and combining demographics files")

demo_list <- vector("list", length(demo_files))
processed_demo_files <- 0L
skipped_demo_files <- 0L
failed_demo_files <- 0L
empty_demo_files <- 0L

pb_demo <- txtProgressBar(min = 0,
                          max = max(1, length(demo_files)),
                          style = 3)

for (i in seq_along(demo_files)) {
  file <- demo_files[i]
  
  participant_id <- extract_composite_id(file)
  
  if (is.na(participant_id)) {
    warning(paste("Skipping demo file due to parsing issues:", file))
    skipped_demo_files <- skipped_demo_files + 1L
    setTxtProgressBar(pb_demo, i)
    next
  }
  
  data <- tryCatch(
    read_csv(
      file,
      show_col_types = FALSE,
      na = c("", "NA", "null", "NaN"),
      col_types = cols(sonaID = col_double(), .default = col_guess())
    ),
    error = function(e) {
      warning(paste("Error reading demo file", file, ":", e$message))
      failed_demo_files <- failed_demo_files + 1L
      return(NULL)
    }
  )
  
  if (is.null(data) || nrow(data) == 0) {
    empty_demo_files <- empty_demo_files + 1L
    setTxtProgressBar(pb_demo, i)
    next
  }
  
  data$participant_id <- participant_id
  
  # -----------------------------
  # Parse JSON only ONCE per row
  # -----------------------------
  if ("response" %in% names(data)) {
    parsed_resp <- purrr::map(data$response, function(x) {
      if (is.na(x) || x == "")
        return(NULL)
      tryCatch(
        jsonlite::fromJSON(x),
        error = function(e)
          NULL
      )
    })
    
    `%||%` <- function(x, y)
      if (is.null(x))
        y
    else
      x
    
    demo_cols <- c(
      "gender",
      "education",
      "handedness",
      "age",
      "origin_region",
      "PLZ_origin_region",
      "site",
      "PLZ_site",
      "native_german",
      "other_lang_early",
      "multilingual",
      "languages_proficiency",
      "lang_variety"
    )
    
    for (col in demo_cols) {
      data[[col]] <- purrr::map_chr(parsed_resp, ~ .x[[col]] %||% NA)
    }
  }
  
  demo_list[[i]] <- data
  processed_demo_files <- processed_demo_files + 1L
  setTxtProgressBar(pb_demo, i)
}
close(pb_demo)
cat("\n")

# Bind once
demo_data <- dplyr::bind_rows(demo_list)
log_info("Demo files processed successfully:", processed_demo_files)
log_info("Demo files skipped (parse):", skipped_demo_files)
log_info("Demo files failed to read:", failed_demo_files)
log_info("Demo files empty:", empty_demo_files)
log_info("Total demo rows before summarising:", nrow(demo_data))
log_checkpoint("Demographics reading complete")

# Process demographics to get one row per participant (last occurrence)
if (nrow(demo_data) > 0) {
  stage_start_time <- Sys.time()
  log_stage("Summarising demographics to one row per participant")
  
  # Get the last occurrence of each demographic variable per participant
  demo_summary <- demo_data %>%
    group_by(participant_id) %>%
    summarise(
      gender = last(gender[!is.na(gender)]),
      education = last(education[!is.na(education)]),
      handedness = last(handedness[!is.na(handedness)]),
      age = last(age[!is.na(age)]),
      origin_region = last(origin_region[!is.na(origin_region)]),
      PLZ_origin_region = last(PLZ_origin_region[!is.na(PLZ_origin_region)]),
      site = last(site[!is.na(site)]),
      PLZ_site = last(PLZ_site[!is.na(PLZ_site)]),
      native_german = last(native_german[!is.na(native_german)]),
      other_lang_early = last(other_lang_early[!is.na(other_lang_early)]),
      multilingual = last(multilingual[!is.na(multilingual)]),
      languages_proficiency = last(languages_proficiency[!is.na(languages_proficiency)]),
      lang_variety = last(lang_variety[!is.na(lang_variety)]),
      .groups = "drop"
    )
  
  log_info("Demographics summarised for",
           nrow(demo_summary),
           "participants")
  log_checkpoint("Demographics summarisation complete")
} else {
  # Create empty demographics if no demo files
  demo_summary <- tibble(
    participant_id = character(),
    gender = character(),
    education = character(),
    handedness = character(),
    age = character(),
    origin_region = character(),
    PLZ_origin_region = character(),
    site = character(),
    PLZ_site = character(),
    native_german = character(),
    other_lang_early = character(),
    multilingual = character(),
    languages_proficiency = character(),
    lang_variety = character()
  )
}

# Merge trial data with demographics
stage_start_time <- Sys.time()
log_stage("Merging trial and demographics")

if (nrow(trials_data) > 0) {
  final_data <- trials_data %>%
    left_join(demo_summary, by = "participant_id")
} else {
  final_data <- tibble()
}
log_info("Rows after merge:", nrow(final_data))
log_info("Columns after merge:", ncol(final_data))
log_checkpoint("Merge complete")

# Reorder columns according to specifications
if (nrow(final_data) > 0) {
  stage_start_time <- Sys.time()
  log_stage("Applying final column ordering and derived variables")
  desired_order <- c(
    "participant_id",
    "university",
    "sonaID",
    "list",
    "participant_in_list",
    "subject_id",
    "timestamp",
    "block",
    "trial_index",
    "word",
    "type",
    "matched_word",
    "response",
    "correct_response",
    "correct",
    "rt",
    "time_elapsed",
    "key_mapping",
    "gender",
    "age",
    "education",
    "handedness",
    "origin_region",
    "site",
    "native_german",
    "other_lang_early",
    "multilingual",
    "languages_proficiency",
    "lang_variety"
  )
  
  # Keep only columns that exist in the data
  existing_cols <- intersect(desired_order, names(final_data))
  other_cols <- setdiff(names(final_data), desired_order)
  
  final_data <- final_data %>%
    select(all_of(c(existing_cols, other_cols)))
}

# create accuracy column
final_data <- final_data %>%
  rename(accuracy = correct)
final_data$accuracy <- ifelse(final_data$accuracy == TRUE, "correct", "incorrect")

# reshape trial index
final_data <- final_data %>%
  group_by(subject_id) %>%
  mutate(trial_index = seq_along(trial_index)) %>%
  ungroup()

# keep only relevant columns

final_data <- final_data[, c(
  "participant_id",
  "university",
  "sonaID",
  "list",
  "participant_in_list",
  "subject_id",
  "timestamp",
  "block",
  "trial_index",
  "word",
  "type",
  "matched_word",
  "response",
  "correct_response",
  "accuracy",
  "rt",
  "time_elapsed",
  "key_mapping",
  "gender",
  "age",
  "education",
  "handedness",
  "origin_region",
  "PLZ_origin_region",
  "site",
  "PLZ_site",
  "native_german",
  "other_lang_early",
  "multilingual",
  "languages_proficiency",
  "lang_variety"
)]
log_info("Final dataset rows:", nrow(final_data))
log_info("Final dataset columns:", ncol(final_data))
log_checkpoint("Final transformations complete")


# Step 3 participant exclusion: non-native German speakers.
stage_start_time <- Sys.time()
log_stage("Participant exclusion by native German status")

if (nrow(final_data) > 0 &&
    "native_german" %in% names(final_data)) {
  native_german_by_participant <- final_data %>%
    group_by(participant_id) %>%
    summarise(
      native_german_value = ifelse(any(!is.na(native_german)), dplyr::last(native_german[!is.na(native_german)]), NA_character_),
      .groups = "drop"
    ) %>%
    mutate(native_german_value = str_to_lower(str_trim(as.character(
      native_german_value
    ))))
  
  non_native_participants <- native_german_by_participant %>%
    filter(!is.na(native_german_value) &
             native_german_value == "nein") %>%
    pull(participant_id)
} else {
  non_native_participants <- character()
}

if (length(non_native_participants) > 0) {
  step3_exclusions <- final_data %>%
    filter(participant_id %in% non_native_participants) %>%
    mutate(
      exclusion_step = "step3_non_native_german",
      exclusion_reason = "Participant marked as non-native German speaker (native_german == 'nein')",
      exclusion_timestamp = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
      script_run_timestamp = run_timestamp
    )
  
  exclusions_ <- bind_rows(exclusions_, step3_exclusions)
  
  final_data <- final_data %>%
    filter(!participant_id %in% non_native_participants)
}

log_info("Excluded at step 3 (non-native German):",
         length(non_native_participants))
log_info(
  "Step 3 excluded trial rows:",
  sum(
    exclusions_$exclusion_step == "step3_non_native_german",
    na.rm = TRUE
  )
)
log_info("Retained after step 3:",
         dplyr::n_distinct(final_data$participant_id))
if (length(non_native_participants) > 0) {
  log_info("Example excluded participant IDs (non-native German):",
           paste(head(non_native_participants, 10), collapse = ", "))
}
log_checkpoint("Native German exclusion complete")

# Snapshot after step 3 (before step 4 accuracy exclusion).
snapshot_after_step3 <- final_data

# Step 4 participant exclusion: mean participant accuracy below 0.75.
stage_start_time <- Sys.time()
log_stage("Participant exclusion by minimum accuracy threshold")

if (nrow(final_data) > 0 &&
    all(c("participant_id", "accuracy") %in% names(final_data))) {
  participant_accuracy <- final_data %>%
    mutate(accuracy_flag = case_when(
      accuracy == "correct" ~ 1,
      accuracy == "incorrect" ~ 0,
      TRUE ~ NA_real_
    )) %>%
    group_by(participant_id) %>%
    summarise(mean_accuracy = mean(accuracy_flag, na.rm = TRUE),
              .groups = "drop")
  
  low_accuracy_participants <- participant_accuracy %>%
    filter(!is.na(mean_accuracy) & mean_accuracy < 0.75) %>%
    pull(participant_id)
} else {
  low_accuracy_participants <- character()
}

if (length(low_accuracy_participants) > 0) {
  step4_exclusions <- final_data %>%
    filter(participant_id %in% low_accuracy_participants) %>%
    mutate(
      exclusion_step = "step4_low_accuracy",
      exclusion_reason = "Participant mean accuracy is below 0.75",
      exclusion_timestamp = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
      script_run_timestamp = run_timestamp
    )
  
  exclusions_ <- bind_rows(exclusions_, step4_exclusions)
  
  final_data <- final_data %>%
    filter(!participant_id %in% low_accuracy_participants)
}

log_info("Excluded at step 4 (accuracy < 0.75):",
         length(low_accuracy_participants))
log_info(
  "Step 4 excluded trial rows:",
  sum(exclusions_$exclusion_step == "step4_low_accuracy", na.rm = TRUE)
)
log_info("Retained after step 4:",
         dplyr::n_distinct(final_data$participant_id))
if (length(low_accuracy_participants) > 0) {
  log_info("Example excluded participant IDs (accuracy < 0.75):",
           paste(head(low_accuracy_participants, 10), collapse = ", "))
}
log_info("Total excluded trial rows (all steps):", nrow(exclusions_))
log_checkpoint("Accuracy-threshold exclusion complete")


#Step 5 participant exclusion: participants under 18 years old.
stage_start_time <- Sys.time()
log_stage("Participant exclusion by minimum age threshold")

if (nrow(final_data) > 0 && "age" %in% names(final_data)) {
  participant_age <- final_data %>%
    mutate(age_numeric = suppressWarnings(as.numeric(age))) %>%
    group_by(participant_id) %>%
    summarise(age_value = last(age_numeric[!is.na(age_numeric)]), .groups = "drop")
  
  underage_participants <- participant_age %>%
    filter(!is.na(age_value) & age_value < 18) %>%
    pull(participant_id)
  
  # Also flag participants with unparseable or missing age
  missing_age_participants <- participant_age %>%
    filter(is.na(age_value)) %>%
    pull(participant_id)
  
  if (length(missing_age_participants) > 0) {
    log_info(
      "Participants with missing/unparseable age (not excluded):",
      length(missing_age_participants)
    )
    log_info("Missing age IDs:", paste(head(missing_age_participants, 10), collapse = ", "))
  }
  
} else {
  underage_participants <- character()
}

if (length(underage_participants) > 0) {
  step5_exclusions <- final_data %>%
    filter(participant_id %in% underage_participants) %>%
    mutate(
      exclusion_step = "step5_underage",
      exclusion_reason = "Participant age is below 18",
      exclusion_timestamp = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
      script_run_timestamp = run_timestamp
    )
  
  exclusions_ <- bind_rows(exclusions_, step5_exclusions)
  
  final_data <- final_data %>%
    filter(!participant_id %in% underage_participants)
}

log_info("Excluded at step 5 (age < 18):", length(underage_participants))
log_info(
  "Step 5 excluded trial rows:",
  sum(exclusions_$exclusion_step == "step5_underage", na.rm = TRUE)
)
log_info("Retained after step 5:",
         dplyr::n_distinct(final_data$participant_id))
if (length(underage_participants) > 0) {
  log_info("Example excluded participant IDs (underage):",
           paste(head(underage_participants, 10), collapse = ", "))
}
log_checkpoint("Age exclusion complete")

# Write output
stage_start_time <- Sys.time()
log_stage("Writing output file")
output_path <- "output/"

output_file_final_precleaned <- paste0("raw_data/", "final_data_precleaned_", time, ".csv")
output_file_anonymized       <- paste0("clean_data/", "anonymized_final_data_precleaned_", time, ".csv")

output_file_snapshot_step3 <- paste0(output_path, "snapshot_after_step3_", time, ".csv")
exclusions_file_all <- paste0(output_path, "exclusions_all_data_cleaned_", time, ".csv")

log_info("Writing final precleaned output to:",
         output_file_final_precleaned)
log_info("Writing snapshot after step 3 to:", output_file_snapshot_step3)
log_info("Writing exclusions (all steps) to:", exclusions_file_all)

if (nrow(snapshot_after_step3) > 0) {
  write_csv(snapshot_after_step3, output_file_snapshot_step3, na = "")
  log_info(
    "Successfully wrote",
    nrow(snapshot_after_step3),
    "rows to",
    output_file_snapshot_step3
  )
} else {
  log_info("No snapshot data after step 3 - creating empty file")
  write_csv(tibble(), output_file_snapshot_step3)
}

if (nrow(final_data) > 0) {
  write_csv(final_data, output_file_final_precleaned, na = "")
  log_info("Successfully wrote",
           nrow(final_data),
           "rows to",
           output_file_final_precleaned)
  
  anonymized_data <- final_data %>% select(-sonaID)
  write_csv(anonymized_data, output_file_anonymized, na = "")
  log_info("Successfully wrote",
           nrow(anonymized_data),
           "anonymized rows to",
           output_file_anonymized)
} else {
  log_info("No final precleaned data to write - creating empty files")
  write_csv(tibble(), output_file_final_precleaned)
  write_csv(tibble(), output_file_anonymized)
}

if (nrow(exclusions_) > 0) {
  write_csv(exclusions_, exclusions_file_all, na = "")
  
  log_info(
    "Successfully wrote",
    nrow(exclusions_),
    "excluded trial rows to",
    exclusions_file_all
  )
} else {
  log_info("No excluded trials found - creating empty exclusions file")
  empty_exclusions <- tibble(
    exclusion_step = character(),
    exclusion_reason = character(),
    exclusion_timestamp = character(),
    script_run_timestamp = character()
  )
  write_csv(empty_exclusions, exclusions_file_all)
}
log_checkpoint("Output writing complete")
log_stage("Cleaning data finished")
log_info(sprintf(
  "Total runtime: %.1f seconds",
  format_duration(script_start_time)
))


# ------------------------------------------------------------
# FINAL PARTICIPANT EXCLUSION SUMMARY (console + log file)
# ------------------------------------------------------------
stage_start_time <- Sys.time()
log_stage("Final participant exclusion summary")

initial_n <- length(all_participant_ids)

excluded_step1 <- unique(incomplete_participants)
excluded_step2 <- unique(low_trial_participants)
excluded_step3 <- unique(non_native_participants)
excluded_step4 <- unique(low_accuracy_participants)
excluded_step5 <- unique(underage_participants)

all_excluded_participants <- unique(c(
  excluded_step1,
  excluded_step2,
  excluded_step3,
  excluded_step4,
  excluded_step5
))

final_participants <- unique(final_data$participant_id)

n_excluded_total <- length(all_excluded_participants)
n_final <- length(final_participants)

log_info("--------------------------------------------------")
log_info("INITIAL participants:", initial_n)
log_info("Excluded step 1 (missing blocks):", length(excluded_step1))
log_info("Excluded step 2 (<1250 trials):", length(excluded_step2))
log_info("Excluded step 3 (non-native German):", length(excluded_step3))
log_info("Excluded step 4 (accuracy < 0.75):", length(excluded_step4))
log_info("Excluded step 5 (underage):", length(excluded_step5))
log_info("--------------------------------------------------")
log_info("TOTAL excluded participants (unique):", n_excluded_total)
log_info("FINAL participants retained:", n_final)
log_info("--------------------------------------------------")

# sanity check
if ((n_excluded_total + n_final) != initial_n) {
  warning("Mismatch in participant accounting! Check exclusion overlap.")
}

log_checkpoint("Final exclusion summary complete")

# ------------------------------------------------------------------
# CONFIGURATION: List structure (used in diagnostics and refill mapping)
# ------------------------------------------------------------------
list_config <- tibble::tribble(
  ~ list_start,
  ~ list_end,
  ~ participants_per_list,
  ~ id_offset,
  1,
  30,
  48,
  0,
  31,
  35,
  48,
  1440,
  36,
  70,
  14,
  1680
)

n_total_participants <- 2170

# ------------------------------------------------------------------
# HELPER FUNCTION: Vectorized list-to-config mapping (for performance)
# ------------------------------------------------------------------
map_list_to_config <- function(list_values, config) {
  n <- length(list_values)
  config_row <- integer(n)
  participants_per_list <- integer(n)
  id_offset <- integer(n)
  list_base <- integer(n)
  
  for (i in 1:nrow(config)) {
    mask <- list_values >= config$list_start[i] &
      list_values <= config$list_end[i]
    config_row[mask] <- i
    participants_per_list[mask] <- config$participants_per_list[i]
    id_offset[mask] <- config$id_offset[i]
    list_base[mask] <- config$list_start[i]
  }
  
  tibble(
    config_row = config_row,
    participants_per_list = participants_per_list,
    id_offset = id_offset,
    list_base = list_base
  )
}

# ------------------------------------------------------------------
# Sanity checks
# ------------------------------------------------------------------

# Group rt by type
final_data %>%
  group_by(type) %>%
  summarise(
    mean_rt = mean(rt, na.rm = TRUE),
    sd_rt   = sd(rt, na.rm = TRUE),
    n       = n(),
    .groups = "drop"
  )

final_data %>%
  group_by(accuracy) %>%
  summarise(
    mean_rt = mean(rt, na.rm = TRUE),
    sd_rt   = sd(rt, na.rm = TRUE),
    n       = n(),
    .groups = "drop"
  )

# Track number of participants per block
final_data %>%
  group_by(block) %>%
  summarise(n_participants = n_distinct(participant_id),
            .groups = "drop")

# Track number of participants per list
final_data %>%
  group_by(list) %>%
  summarise(n_participants = n_distinct(participant_id),
            .groups = "drop")

# Track number of participants in list per list
final_data %>%
  group_by(participant_in_list) %>%
  summarise(n_participants = n_distinct(participant_id),
            .groups = "drop") %>%
  print(n = Inf)

table(table(final_data$word))
table(table(final_data$participant_in_list))

# ------------------------------------------------------------------
# Refill ID mapping
# ------------------------------------------------------------------

log_stage("Refill ID mapping (full reconstruction approach)")

# Build full set of global IDs
all_possible_ids <- tibble(global_id = 1:n_total_participants)
log_info("Total possible global IDs:", nrow(all_possible_ids))

# Reconstruct global IDs from final data (valid participants — should NOT be resampled)
log_stage("Reconstructing valid global IDs")

valid_ids <- final_data %>%
  distinct(participant_id, list, participant_in_list) %>%
  mutate(list = as.integer(list),
         participant_in_list = as.integer(participant_in_list)) %>%
  bind_cols(map_list_to_config(.$list, list_config)) %>%
  mutate(global_id = id_offset + (list - list_base) * participants_per_list + participant_in_list) %>%
  distinct(global_id)

log_info("Valid global IDs:", nrow(valid_ids))

# Reconstruct global IDs from exclusions (diagnostic only)
log_stage("Reconstructing excluded global IDs")

excluded_slots <- exclusions_ %>%
  distinct(list, participant_in_list) %>%
  filter(!is.na(list), !is.na(participant_in_list)) %>%
  mutate(list = as.integer(list),
         participant_in_list = as.integer(participant_in_list)) %>%
  bind_cols(map_list_to_config(.$list, list_config)) %>%
  mutate(global_id = id_offset + (list - list_base) * participants_per_list + participant_in_list) %>%
  distinct(global_id)

log_info("Excluded global IDs:", nrow(excluded_slots))

# Compute IDs that still need sampling
log_stage("Computing refill IDs")

ids_to_refill <- all_possible_ids %>%
  filter(!global_id %in% valid_ids$global_id)

log_info("Total IDs needing sampling:", nrow(ids_to_refill))

# Diagnostics
log_stage("Diagnostics")

n_from_exclusions <- length(intersect(ids_to_refill$global_id, excluded_slots$global_id))
n_never_sampled   <- length(setdiff(ids_to_refill$global_id, excluded_slots$global_id))

log_info("IDs from exclusions:", n_from_exclusions)
log_info("IDs never sampled:", n_never_sampled)

# Final safety checks
if (any(ids_to_refill$global_id < 1 |
        ids_to_refill$global_id > n_total_participants)) {
  stop("Invalid global IDs detected")
}
if (any(duplicated(ids_to_refill$global_id))) {
  stop("Duplicates detected in refill IDs")
}
if (nrow(ids_to_refill) == 0) {
  warning("No IDs need to be sampled — dataset already complete")
}

# Write refill ID list
output_file <- file.path("output", paste0("ids_to_refill_", time, ".txt"))

writeLines(as.character(sort(ids_to_refill$global_id)), output_file)

log_info("Refill ID list written to:", output_file)


# ------------------------------------------------------------------
# Diagnose duplicated global IDs and overused lists
# ------------------------------------------------------------------

log_stage("Diagnosing duplicated IDs and overused lists")

# Ensure numeric
final_data <- final_data %>%
  mutate(list                = as.integer(list),
         participant_in_list = as.integer(participant_in_list))

# Map list config to each row - VECTORIZED for performance
final_data_mapped <- final_data %>%
  bind_cols(map_list_to_config(.$list, list_config)) %>%
  select(-config_row)

# Recompute global IDs
final_data_mapped <- final_data_mapped %>%
  mutate(global_id = id_offset +
           (list - list_base) * participants_per_list +
           participant_in_list)

# 1. Find duplicated global IDs
dup_ids <- final_data_mapped %>%
  distinct(participant_id, global_id) %>%
  count(global_id) %>%
  filter(n > 1) %>%
  arrange(desc(n))

log_info("Number of duplicated global IDs:", nrow(dup_ids))
if (nrow(dup_ids) > 0) {
  log_info("Top duplicated IDs:", paste(head(dup_ids$global_id, 20), collapse = ", "))
}

# Which participants share those IDs
dup_id_details <- final_data_mapped %>%
  filter(global_id %in% dup_ids$global_id) %>%
  distinct(participant_id, global_id, list, participant_in_list) %>%
  arrange(global_id)

# 2. Check list overuse
actual_per_list_diag <- final_data_mapped %>%
  distinct(participant_id, list) %>%
  count(list, name = "actual_n")

expected_per_list_diag <- tibble(list = 1:70) %>%
  rowwise() %>%
  mutate(
    config_row = which(list >= list_config$list_start &
                         list <= list_config$list_end),
    expected_n = list_config$participants_per_list[config_row]
  ) %>%
  ungroup()

list_check <- actual_per_list_diag %>%
  left_join(expected_per_list_diag, by = "list") %>%
  mutate(diff = actual_n - expected_n)

overused_lists <- list_check %>%
  filter(diff > 0) %>%
  arrange(desc(diff))

log_info("Number of overused lists:", nrow(overused_lists))
if (nrow(overused_lists) > 0) {
  log_info("Overused lists (list: excess participants):",
           paste(paste0(overused_lists$list, ":", overused_lists$diff), collapse = ", "))
}

# 3. Check duplicated participant slots within list
dup_slots <- final_data_mapped %>%
  distinct(participant_id, list, participant_in_list) %>%
  count(list, participant_in_list) %>%
  filter(n > 1) %>%
  arrange(desc(n))

log_info("Duplicated participant slots within lists:", nrow(dup_slots))

# Write diagnostic files
write_csv(dup_ids,        file.path("output", "duplicate_global_ids.csv"))
write_csv(dup_id_details, file.path("output", "duplicate_global_id_details.csv"))
write_csv(overused_lists, file.path("output", "overused_lists.csv"))
write_csv(dup_slots,      file.path("output", "duplicate_slots.csv"))

log_info("Diagnostic files written to output/")

# ------------------------------------------------------------------
# SIMPLE PARTICIPANT COUNT PER LIST (EXPECTED vs ACTUAL + DEVIATIONS)
# ------------------------------------------------------------------

log_stage("Computing participant counts per list")

# 1. Build expected participants per list from config - VECTORIZED
expected_per_list <- tibble(list = 1:70) %>%
  bind_cols(map_list_to_config(1:70, list_config)) %>%
  select(
    list,
    expected_n = participants_per_list,
    id_offset_col = id_offset,
    list_base_col = list_base
  )

# 2. Actual participants per list from final data
actual_per_list <- final_data %>%
  distinct(participant_id, list) %>%
  count(list, name = "actual_n")

# 3. Get actual global IDs present per list — keep duplicates so we can detect them
actual_global_ids_per_list <- final_data_mapped %>%
  distinct(participant_id, list, global_id) %>%   # one row per participant, not per trial
  group_by(list) %>%
  summarise(actual_global_ids = list(sort(global_id)), .groups = "drop")

# 4. Generate expected global IDs per list - VECTORIZED

expected_global_ids_per_list <- expected_per_list %>%
  mutate(
    expected_global_ids = purrr::map2(
      id_offset_col + (list - list_base_col) * expected_n + 1,
      id_offset_col + (list - list_base_col) * expected_n + expected_n,
      ~ .x:.y
    )
  ) %>%
  select(list, expected_global_ids)

list_summary_with_ids <- expected_per_list %>%
  left_join(actual_per_list, by = "list") %>%
  left_join(expected_global_ids_per_list, by = "list") %>%
  left_join(actual_global_ids_per_list, by = "list") %>%
  mutate(
    actual_n = tidyr::replace_na(actual_n, 0),
    actual_global_ids = purrr::map(actual_global_ids, 
                                   ~ if (is.null(.x)) integer(0) else .x),
    
    # Missing: expected slots with no participant
    missing_global_ids = map2(expected_global_ids, actual_global_ids,
                              ~ setdiff(.x, .y)),
    
    # Additional: participants in slots that are DUPLICATED
    # i.e. same global ID appears more than once
    additional_global_ids = map(actual_global_ids, function(ids) {
      dup_ids <- ids[duplicated(ids)]
      unique(dup_ids)
    }),
    
    missing_n    = map_int(missing_global_ids, length),
    additional_n = map_int(additional_global_ids, length),
    
    missing_ids_str    = map_chr(missing_global_ids, 
                                 ~ if (length(.x) > 0) paste(.x, collapse = ", ") else ""),
    additional_ids_str = map_chr(additional_global_ids, 
                                 ~ if (length(.x) > 0) paste(.x, collapse = ", ") else "")
  ) %>%
  select(list, expected_n, actual_n, missing_n, additional_n,
         missing_ids_str, additional_ids_str) %>%
  rename(
    missing_global_ids    = missing_ids_str,
    additional_global_ids = additional_ids_str
  ) %>%
  arrange(list)

# Create a cleaner version for display (only show ID columns when relevant)
list_summary <- list_summary_with_ids %>%
  mutate(
    missing_global_ids = if_else(missing_n > 0, missing_global_ids, ""),
    additional_global_ids = if_else(additional_n > 0, additional_global_ids, "")
  )

# 6. Output summary
log_info("List-level participant summary:")
list_summary %>% print(n = Inf)

log_info("Lists with deviations from expected:")
deviations_summary <- list_summary %>%
  filter(missing_n != 0 | additional_n != 0) %>%
  arrange(desc(additional_n), desc(missing_n))

deviations_summary %>% print(n = Inf)

# For very long ID lists, also create a detailed version
if (nrow(deviations_summary) > 0) {
  log_info("Sample of lists with missing participants:")
  list_summary %>%
    filter(missing_n > 0) %>%
    select(list, missing_n, missing_global_ids) %>%
    head(10) %>%
    print()
  
  log_info("Sample of lists with additional participants:")
  list_summary %>%
    filter(additional_n > 0) %>%
    select(list, additional_n, additional_global_ids) %>%
    head(10) %>%
    print()
}

total_missing <- sum(list_summary$missing_n, na.rm = TRUE)
total_additional <- sum(list_summary$additional_n, na.rm = TRUE)

log_info("Total missing participants across all lists:", total_missing)
log_info("Total additional participants across all lists:",
         total_additional)
log_info("Net deviation (additional - missing):",
         total_additional - total_missing)

# Sanity check: this should match the refill count
expected_refill_count <- nrow(ids_to_refill)
log_info("Expected refill IDs from earlier calculation:",
         expected_refill_count)
log_info("Total missing from list summary:", total_missing)

if (total_missing != expected_refill_count) {
  warning(
    sprintf(
      "INCONSISTENCY: List summary shows %d missing but refill calculation shows %d IDs needed",
      total_missing,
      expected_refill_count
    )
  )
} else {
  log_info("✓ Consistency check passed: missing count matches refill IDs")
}

# Write list summary with global IDs
output_file_summary <- file.path("output", paste0("list_summary_", time, ".csv"))
write_csv(list_summary, output_file_summary)
log_info("List summary written to:", output_file_summary)

# Also write a detailed version with one row per missing/additional ID for easier analysis
detailed_missing <- list_summary_with_ids %>%
  filter(missing_n > 0) %>%
  select(list, missing_global_ids) %>%
  separate_rows(missing_global_ids, sep = ", ") %>%
  filter(missing_global_ids != "") %>%
  mutate(global_id = as.integer(missing_global_ids)) %>%
  select(list, global_id) %>%
  arrange(list, global_id)

detailed_additional <- list_summary_with_ids %>%
  filter(additional_n > 0) %>%
  select(list, additional_global_ids) %>%
  separate_rows(additional_global_ids, sep = ", ") %>%
  filter(additional_global_ids != "") %>%
  mutate(global_id = as.integer(additional_global_ids)) %>%
  select(list, global_id) %>%
  arrange(list, global_id)

if (nrow(detailed_missing) > 0) {
  output_file_missing_detail <- file.path("output", paste0("missing_ids_detail_", time, ".csv"))
  write_csv(detailed_missing, output_file_missing_detail)
  log_info("Detailed missing IDs written to:",
           output_file_missing_detail)
}

if (nrow(detailed_additional) > 0) {
  output_file_additional_detail <- file.path("output", paste0("additional_ids_detail_", time, ".csv"))
  write_csv(detailed_additional, output_file_additional_detail)
  log_info("Detailed additional IDs written to:",
           output_file_additional_detail)
}

log_info("Script finished successfully")

