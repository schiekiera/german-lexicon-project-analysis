rm(list=ls())

library(nlme)
library(multilevel)
library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(lme4)
library(here)

data_dir <- "clean_data"

input_files <- list.files(data_dir, pattern = "anonymized_final_data_precleaned_.*\\.csv$", full.names = TRUE)
if (length(input_files) == 0) stop("No cleaned data file found in data_dir")
latest_file <- input_files[which.max(file.info(input_files)$mtime)]

glp <- read_csv(latest_file, show_col_types = FALSE) |>
  dplyr::select(subject_id, word, rt, type) |>
  filter(!is.na(subject_id), !is.na(word), !is.na(rt), !is.na(type)) |>
  filter(rt >= 200, rt <= 4000) |>
  mutate(log_rt = log(rt))




# the same as psychometric::ICC2.lme, but using the optim optimiser
ICC2.lme2 <- function (dv, grp, data, weighted = FALSE) 
{
  dv <- data %>% dplyr::select({
    {
      dv
    }
  }) %>% purrr::reduce(c)
  grp <- data %>% dplyr::select({
    {
      grp
    }
  }) %>% purrr::reduce(c) %>% factor()
  mod <- lme(
    dv ~ 1, random = list(~1 | grp),
    na.action = na.omit,
    control = lmeControl(maxIter=100, msMaxIter=100, msMaxEval=400, opt="optim")
  )
  if (!weighted) {
    icc2 <- mean(gmeanrel(mod)$MeanRel)
  }
  else {
    icc2 <- weighted.mean(gmeanrel(mod)$MeanRel, gmeanrel(mod)$GrpSize)
  }
  return(icc2)
}

# observed spearman-brown values
sb_res <- tribble(
  ~type, ~sb_obs,
  "overall", ICC2.lme2(log_rt, word, data=glp),
  "word", ICC2.lme2(log_rt, word, data=filter(glp, type=="word")),
  "pseudoword", ICC2.lme2(log_rt, word, data=filter(glp, type=="pseudoword"))
) |>
  mutate( n_obs = length(unique(glp$subject_id)) )

# predicted spearman brown values
sb_pred <- expand_grid(
  sb_res,
  sb_des = seq(0.7, 0.95, length.out=5000)
) |>
  mutate(
    n_req = (sb_des * (1 - sb_obs)) / (sb_obs * (1 - sb_des)) * n_obs
  )

# plot
pl <- sb_pred |>
  ggplot(aes(n_req, sb_des, colour=type)) +
  geom_line() +
  geom_vline(xintercept = sb_res$n_obs, linetype="dashed") +
  scale_colour_brewer(palette = "Dark2") +
  labs(x = "N", y = "Item Spearman-Brown", colour=NULL) +
  theme_classic() +
  theme(legend.position = "top")
pl
ggsave("sb_plot.png", pl, width=3, height=3)

# fewer examples
sb_pred_small <- expand_grid(
  sb_res,
  sb_des = c(0.83, 0.85, 0.87, 0.89, 0.9)
) |>
  mutate(
    n_req = (sb_des * (1 - sb_obs)) / (sb_obs * (1 - sb_des)) * n_obs
  )


sb_pred_small
