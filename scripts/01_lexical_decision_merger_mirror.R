# Lexical Decision Data Cleaning and Merging Script
# Date: 2025-12-05

# This script reads, cleans, and merges lexical decision data exported as CSVs.

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

local_log_dir <- file.path(getwd(), "output", "log")
mirror_log_dir <- "/Users/louis/Desktop/HU_Berlin_Computational_Modelling/03_LaTeX/02_Presentations/TRUST_GLP_data/context/log"
dir.create(local_log_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(mirror_log_dir, recursive = TRUE, showWarnings = FALSE)

local_log_file <- file.path(local_log_dir, paste0("merger_log_", run_timestamp_file, ".log"))
mirror_log_file <- file.path(mirror_log_dir, paste0("merger_log_", run_timestamp_file, ".log"))

write_log_line <- function(line_text) {
  cat(line_text, "\n", sep = "")
  cat(line_text, "\n", sep = "", file = local_log_file, append = TRUE)
  cat(line_text, "\n", sep = "", file = mirror_log_file, append = TRUE)
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
log_info("Evaluation start time:", format(script_start_time, "%Y-%m-%d %H:%M:%S"))
log_info("Working directory:", getwd())
log_info("Input directory:", file.path(getwd(), "data"))
log_info("Output prefix:", file.path(getwd(), "output"))
log_info("Local log file:", local_log_file)
log_info("Mirror log file:", mirror_log_file)

# Function to extract participant ID from filename
extract_participant_id <- function(filename) {
  base <- basename(filename)
  match <- str_match(base, "_p(\\d+)_")
  if (!is.na(match[1, 2])) {
    return(match[1, 2])
  }
  return(NA_character_)
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

# Function to safely parse JSON and extract specific keys
safe_json_extract <- function(json_string, key) {
  if (is.na(json_string) ||
      is.null(json_string) || json_string == "") {
    return(NA)
  }
  
  tryCatch({
    parsed <- fromJSON(json_string)
    if (key %in% names(parsed)) {
      return(parsed[[key]])
    } else {
      return(NA)
    }
  }, error = function(e) {
    return(NA)
  })
}

# Discover all CSV files in the current directory
stage_start_time <- Sys.time()
log_stage("Discovering CSV files")
all_files <- list.files("data", pattern = "\\.csv$", full.names = TRUE, recursive = TRUE)

if (length(all_files) == 0) {
  stop("No CSV files found in the current directory!")
}

log_info("Found", length(all_files), "CSV files")

# CSVs with "_block<number>_" for experimental blocks 1–10
block_files_all <- all_files[str_detect(all_files, "_block(\\d+)_")]

# CSVs with block17demo for demographics
demo_files_all <- all_files[str_detect(all_files, "block11demo_")]

# Keep only parseable demo files and define the participant cohort from demos.
demo_participant_ids <- map_chr(demo_files_all, extract_participant_id)
valid_demo_idx <- !is.na(demo_participant_ids)
demo_files <- demo_files_all[valid_demo_idx]
demo_participant_ids <- demo_participant_ids[valid_demo_idx]
demo_participants <- unique(demo_participant_ids)

# Keep only block files associated with participants who have a demo file.
block_participant_ids <- map_chr(block_files_all, extract_participant_id)
valid_block_idx <- !is.na(block_participant_ids) & block_participant_ids %in% demo_participants
block_files <- block_files_all[valid_block_idx]

log_info("All block files discovered:", length(block_files_all))
log_info("All demo files discovered:", length(demo_files_all))
log_info("Demo files with parseable participant IDs:", length(demo_files))
log_info("Participants defined by demo files:", length(demo_participants))
log_info("Block files linked to demo participants:", length(block_files))

# Documentation helper: participants with block1 data but no demo file.
block1_files_all <- block_files_all[str_detect(block_files_all, "_block1_")]
block1_participant_ids <- map_chr(block1_files_all, extract_participant_id)
block1_participants <- unique(block1_participant_ids[!is.na(block1_participant_ids)])
block1_without_demo <- setdiff(block1_participants, demo_participants)
log_info("Participants with block1 file but no demo file:", length(block1_without_demo))

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
missing_block_count <- 0
missing_participants <- character(0)

pb_participants <- txtProgressBar(min = 0, max = max(1, length(all_participant_ids)), style = 3)
for (pid in all_participant_ids) {
  participant_blocks <- block_files[str_detect(block_files, paste0("_p", pid, "_"))]
  blocks_present <- map_int(participant_blocks, extract_block)
  blocks_present <- blocks_present[!is.na(blocks_present)]
  
  missing_blocks <- setdiff(1:10, blocks_present)
  if (length(missing_blocks) > 0) {
    missing_block_count <- missing_block_count + 1
    missing_participants <- c(missing_participants, pid)
  }
  setTxtProgressBar(pb_participants, which(all_participant_ids == pid))
}
close(pb_participants)
cat("\n")
log_info("Participants checked:", length(all_participant_ids))
log_info("Participants missing >=1 block:", missing_block_count)
if (length(missing_participants) > 0) {
  log_info("Example participant IDs with missing blocks:", paste(head(missing_participants, 10), collapse = ", "))
}
log_checkpoint("Block completeness check complete")

# Exclude participants with demo but incomplete blocks from all downstream steps.
if (missing_block_count > 0) {
  warning(paste(
    "Detected", missing_block_count,
    "participants with incomplete blocks 1-10; these participants will be excluded from final output."
  ))
}

incomplete_participants <- unique(missing_participants)
complete_participants <- setdiff(all_participant_ids, incomplete_participants)
exclusions_ <- tibble()

log_stage("Participant exclusion summary")
log_info("Initial participants (demo-defined cohort):", length(all_participant_ids))
log_info("Excluded at step 1 (missing blocks 1-10):", length(incomplete_participants))
log_info("Retained after step 1:", length(complete_participants))

block_file_participant_ids <- map_chr(block_files, extract_participant_id)
demo_file_participant_ids <- map_chr(demo_files, extract_participant_id)

# Keep all demo-cohort files at this stage so excluded trials can be exported later.
block_files <- block_files[!is.na(block_file_participant_ids)]
demo_files <- demo_files[!is.na(demo_file_participant_ids)]
log_info("Block files loaded before trial-level exclusions:", length(block_files))
log_info("Demo files loaded before trial-level exclusions:", length(demo_files))

stage_start_time <- Sys.time()
log_stage("Reading and combining block trial files")

trials_list <- vector("list", length(block_files))
processed_block_files <- 0L
skipped_block_files <- 0L
failed_block_files <- 0L
empty_block_files <- 0L

pb_blocks <- txtProgressBar(min = 0, max = max(1, length(block_files)), style = 3)

for (i in seq_along(block_files)) {
  
  file <- block_files[i]
  
  participant_id <- extract_participant_id(file)
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
      col_types = cols(
        sonaID = col_double(),
        .default = col_guess()
      )
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
      trials_data$stimulus <- stringr::str_remove_all(
        trials_data$stimulus,
        "<[^>]+>"
      )
    }
    
    # -----------------------------
    # 2. Parse JSON only once
    # -----------------------------
    if ("log" %in% cols_present) {
      
      parsed_log <- purrr::map(trials_data$log, function(x) {
        if (is.na(x) || x == "") return(NULL)
        tryCatch(jsonlite::fromJSON(x), error = function(e) NULL)
      })
      
      `%||%` <- function(x, y) if (is.null(x)) y else x
      
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
      c("word","type","response","task",
        "trial_type","key_mapping","plugin_version"),
      cols_present
    )
    
    if (length(char_cols) > 0) {
      trials_data[char_cols] <-
        lapply(trials_data[char_cols], stringr::str_trim)
    }
    
    # -----------------------------
    # 5. Remove duplicates
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
log_info("Step 1 excluded trial rows:", sum(exclusions_$exclusion_step == "step1_missing_blocks", na.rm = TRUE))
log_info("Trial rows retained after step 1:", nrow(trials_data))
log_checkpoint("Step 1 trial exclusion complete")

# Final participant exclusion step: keep only participants with >= 1250 trial rows.
stage_start_time <- Sys.time()
log_stage("Final participant exclusion by minimum trial count")

if (!("participant_id" %in% names(trials_data)) || nrow(trials_data) == 0) {
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
  
  demo_files <- demo_files[map_chr(demo_files, extract_participant_id) %in% unique(trials_data$participant_id)]
}

log_info("Excluded at step 2 (<1250 total trials):", length(low_trial_participants))
log_info("Retained after step 2:", dplyr::n_distinct(trials_data$participant_id))
log_info("Trial rows after step 2 exclusion:", nrow(trials_data))
log_info("Step 2 excluded trial rows:", sum(exclusions_$exclusion_step == "step2_low_trial_count", na.rm = TRUE))
log_info("Total excluded trial rows (all steps):", nrow(exclusions_))
if (length(low_trial_participants) > 0) {
  log_info("Example excluded participant IDs (<1250 trials):", paste(head(low_trial_participants, 10), collapse = ", "))
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

pb_demo <- txtProgressBar(min = 0, max = max(1, length(demo_files)), style = 3)

for (i in seq_along(demo_files)) {
  
  file <- demo_files[i]
  
  participant_id <- extract_participant_id(file)
  
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
      col_types = cols(
        sonaID = col_double(),
        .default = col_guess()
      )
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
      if (is.na(x) || x == "") return(NULL)
      tryCatch(jsonlite::fromJSON(x), error = function(e) NULL)
    })
    
    `%||%` <- function(x, y) if (is.null(x)) y else x
    
    demo_cols <- c(
      "gender","education","handedness","age",
      "origin_region","PLZ_origin_region",
      "site","PLZ_site","native_german",
      "other_lang_early","multilingual",
      "languages_proficiency","lang_variety"
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
  
  log_info("Demographics summarised for", nrow(demo_summary), "participants")
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

if (nrow(final_data) > 0 && "native_german" %in% names(final_data)) {
  native_german_by_participant <- final_data %>%
    group_by(participant_id) %>%
    summarise(
      native_german_value = ifelse(
        any(!is.na(native_german)),
        dplyr::last(native_german[!is.na(native_german)]),
        NA_character_
      ),
      .groups = "drop"
    ) %>%
    mutate(native_german_value = str_to_lower(str_trim(as.character(native_german_value))))
  
  non_native_participants <- native_german_by_participant %>%
    filter(!is.na(native_german_value) & native_german_value == "nein") %>%
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

log_info("Excluded at step 3 (non-native German):", length(non_native_participants))
log_info("Step 3 excluded trial rows:", sum(exclusions_$exclusion_step == "step3_non_native_german", na.rm = TRUE))
log_info("Retained after step 3:", dplyr::n_distinct(final_data$participant_id))
if (length(non_native_participants) > 0) {
  log_info("Example excluded participant IDs (non-native German):", paste(head(non_native_participants, 10), collapse = ", "))
}
log_checkpoint("Native German exclusion complete")

# Snapshot after step 3 (before step 4 accuracy exclusion).
snapshot_after_step3 <- final_data

# Step 4 participant exclusion: mean participant accuracy below 0.75.
stage_start_time <- Sys.time()
log_stage("Participant exclusion by minimum accuracy threshold")

if (nrow(final_data) > 0 && all(c("participant_id", "accuracy") %in% names(final_data))) {
  participant_accuracy <- final_data %>%
    mutate(
      accuracy_flag = case_when(
        accuracy == "correct" ~ 1,
        accuracy == "incorrect" ~ 0,
        TRUE ~ NA_real_
      )
    ) %>%
    group_by(participant_id) %>%
    summarise(
      mean_accuracy = mean(accuracy_flag, na.rm = TRUE),
      .groups = "drop"
    )
  
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

log_info("Excluded at step 4 (accuracy < 0.75):", length(low_accuracy_participants))
log_info("Step 4 excluded trial rows:", sum(exclusions_$exclusion_step == "step4_low_accuracy", na.rm = TRUE))
log_info("Retained after step 4:", dplyr::n_distinct(final_data$participant_id))
if (length(low_accuracy_participants) > 0) {
  log_info("Example excluded participant IDs (accuracy < 0.75):", paste(head(low_accuracy_participants, 10), collapse = ", "))
}
log_info("Total excluded trial rows (all steps):", nrow(exclusions_))
log_checkpoint("Accuracy-threshold exclusion complete")

# Write output
stage_start_time <- Sys.time()
log_stage("Writing output file")
output_path <- "output/"
time <- gsub(pattern = " ", "_", Sys.time())
time <- gsub(pattern = ":", "-", time)
time <- gsub(pattern = "\\..*$", "", time)

output_file_final_precleaned <- paste0(output_path, "final_data_precleaned_", time, ".csv")
output_file_snapshot_step3 <- paste0(output_path, "snapshot_after_step3_", time, ".csv")
exclusions_file_all <- paste0(output_path, "exclusions_all_data_cleaned_", time, ".csv")

log_info("Writing final precleaned output to:", output_file_final_precleaned)
log_info("Writing snapshot after step 3 to:", output_file_snapshot_step3)
log_info("Writing exclusions (all steps) to:", exclusions_file_all)

if (nrow(snapshot_after_step3) > 0) {
  write_csv(snapshot_after_step3, output_file_snapshot_step3, na = "")
  log_info("Successfully wrote", nrow(snapshot_after_step3), "rows to", output_file_snapshot_step3)
} else {
  log_info("No snapshot data after step 3 - creating empty file")
  write_csv(tibble(), output_file_snapshot_step3)
}

if (nrow(final_data) > 0) {
  write_csv(final_data, output_file_final_precleaned, na = "")
  log_info("Successfully wrote", nrow(final_data), "rows to", output_file_final_precleaned)
} else {
  log_info("No final precleaned data to write - creating empty file")
  write_csv(tibble(), output_file_final_precleaned)
}

if (nrow(exclusions_) > 0) {
  write_csv(exclusions_, exclusions_file_all, na = "")
  
  log_info("Successfully wrote", nrow(exclusions_), "excluded trial rows to", exclusions_file_all)
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
log_stage("Script finished")
log_info(sprintf("Total runtime: %.1f seconds", format_duration(script_start_time)))