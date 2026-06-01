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

input_file <- "/Users/louis/Desktop/HU_Berlin_Computational_Modelling/German_Lexicon_Project/00_data_project/output/final_data_precleaned_2026-03-03_09-51-51.csv"

df<-read_csv(input_file)

colnames(df)

# vars to exclude: sonaID, origin_region, subject_id, PLZ_origin_region, site, PLZ_site
df <- df %>%
  select(-sonaID, -origin_region, -subject_id, -PLZ_origin_region, -site, -PLZ_site)

colnames(df)

replace_participant_id <- function(df, id_col = "participant_id", 
                                   new_col = "anon_id", 
                                   prefix = "P") {
  # Ensure column exists
  if (!id_col %in% colnames(df)) {
    stop(paste("Column", id_col, "not found in dataframe."))
  }
  # Create lookup table with unique IDs
  id_lookup <- df %>%
    distinct(.data[[id_col]]) %>%
    mutate(
      !!new_col := paste0(
        prefix,
        sprintf("%04d", sample(1:n(), n()))
      )
    )

  # Join back to original dataframe
  df_anon <- df %>%
    left_join(id_lookup, by = id_col) %>%
    select(-all_of(id_col)) %>%              # remove original ID
    relocate(all_of(new_col), .before = 1)   # put anon_id first
  
  return(df_anon)
}

df <- replace_participant_id(df)

colnames(df)



compute_split_half_reliability <- function(df,
                                           item_col = "word",
                                           participant_col = "anon_id",
                                           rt_col = "log_rt",
                                           n_iter = 1000,
                                           seed = 1234) {
  
  set.seed(seed)
  
  results <- numeric(n_iter)
  
  participants <- unique(df[[participant_col]])
  
  for (i in 1:n_iter) {
    
    # Random split of participants
    half1 <- sample(participants, length(participants)/2)
    half2 <- setdiff(participants, half1)
    
    df1 <- df[df[[participant_col]] %in% half1, ]
    df2 <- df[df[[participant_col]] %in% half2, ]
    
    # Item means
    m1 <- aggregate(df1[[rt_col]], 
                    by = list(df1[[item_col]]), 
                    mean)
    m2 <- aggregate(df2[[rt_col]], 
                    by = list(df2[[item_col]]), 
                    mean)
    
    colnames(m1) <- c("item", "mean1")
    colnames(m2) <- c("item", "mean2")
    
    merged <- merge(m1, m2, by = "item")
    
    r <- cor(merged$mean1, merged$mean2, method = "spearman")
    
    # Spearman–Brown correction
    results[i] <- (2 * r) / (1 + r)
  }
  
  return(list(
    mean_reliability = mean(results, na.rm = TRUE),
    sd_reliability = sd(results, na.rm = TRUE),
    distribution = results
  ))
}



write_csv(df, "output/shared_anonymized_data_precleaned_2026-03-03_09-51-51.csv")