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

setwd(here::here())

# 1.2 Run metadata
run_timestamp <- format(Sys.time(), "%Y-%m-%d_%H-%M-%S")

# ============================================================
# 2. Paths and Logging
# ============================================================
# 2.1 Paths
data_dir <- "data/02_precleaned_data"


# ============================================================
# 3. Load Data
# ============================================================
# 3.1 Locate latest merged dataset (final precleaned for all analyses)
input_files <- list.files(data_dir, pattern = "anonymized_final_data_precleaned_.*\\.csv$", full.names = TRUE)
if (length(input_files) == 0) stop("No cleaned data file found in data/02_precleaned_data/")

latest_file <- input_files[which.max(file.info(input_files)$mtime)]

# 3.2 Read
df <- read_csv(latest_file, show_col_types = FALSE)


#
colnames(df)
table(df$word)
table(df$type)
table(df$accuracy)

# get the word with the highest accuracy
df$accuracy <- ifelse(df$accuracy == "correct", 1, 0)
df_sort <- df %>%
  filter(type == "word") %>%
  group_by(word) %>%
  summarise(m_accuracy = mean(accuracy, na.rm = TRUE)) %>%
  arrange(desc(m_accuracy))

df_sort_pw <- df %>%
  filter(type == "pseudoword") %>%
  group_by(word) %>%
  summarise(m_accuracy = mean(accuracy, na.rm = TRUE)) %>%
  arrange(desc(m_accuracy))


participant_demo <- df %>% distinct(participant_id, .keep_all = TRUE)


table(participant_demo$origin_region)


participant_demo <- participant_demo %>%
  mutate(
    origin_region_clean = str_to_lower(origin_region),
    origin_group = case_when(
      # Germany (D)
      str_detect(
        origin_region_clean,
        "deutsch|german|\\bde\\b|\\bd\\b|brd|ddr|bundesrep|ostdeutsch"
      ) ~ "D",

      # Austria (A)
      str_detect(
        origin_region_clean,
        "österreich|austria"
      ) ~ "A",

      # Switzerland (CH)
      str_detect(
        origin_region_clean,
        "schweiz"
      ) ~ "CH",

      # Everything else
      TRUE ~ "other"
    )
  )


table(participant_demo$origin_region_clean)

table(participant_demo$origin_group)

participant_d <- participant_demo %>%
  filter(origin_group == "D")

participant_d <- participant_d %>%
  mutate(
    # extract first two consecutive digits found anywhere in the string
    PLZ_2digit = str_extract(PLZ_origin_region, "\\d{2}")
  )


sort(table(participant_d$PLZ_2digit))

# most frequent
# 1. Köln
# 2. Tübingen-Reutlingen Area
# 3. Darmstadt Area
# 4. Münster Area
# 5. Berlin-Mitte

# 1) Your counts by 2-digit prefix
plz2_counts <- participant_d %>%
  mutate(PLZ_2digit = str_extract(PLZ_origin_region, "\\d{2}")) %>%
  filter(!is.na(PLZ_2digit)) %>%
  count(PLZ_2digit, name = "n")

# 2) Load the full PLZ reference table shipped with dePlzMap
populationData <- read.table(
  system.file("extdata", "populationData.csv", package = "dePlzMap"),
  sep = ",", header = TRUE,
  colClasses = c("character", "integer", "character"),
  encoding = "UTF-8"
)

# 3) Expand your 2-digit counts to ALL 5-digit PLZs
map_data <- populationData %>%
  transmute(
    plz = plz,
    PLZ_2digit = str_sub(plz, 1, 2)
  ) %>%
  left_join(plz2_counts, by = "PLZ_2digit") %>%
  mutate(n = replace_na(n, 0)) %>% # prefixes not in your sample -> 0
  select(plz, n)

# 4) Plot choropleth by (prefix-count assigned to each 5-digit PLZ)
dePlzMap(
  data = map_data,
  legendTitle = "n (participants)"
)
