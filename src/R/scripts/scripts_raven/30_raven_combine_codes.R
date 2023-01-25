## ------------------------------------------------------------------
## 30_raven_combine_codes_dprime
## 
## Project: 
## Purpose: 
## Author: Corrado Caudek
## Date: "Mon Jan 11 15:35:45 2021"
## ------------------------------------------------------------------

library("here")
library("tidyverse")


controls_look_up_table <- readRDS(
  here("data", "processed", "raven", "controls_look_up_table.rds")
)

patients_look_up_table <- readRDS(
  here("data", "processed", "raven", "patients_look_up_table.rds")
)

controls_raven_scores <- readRDS(
  here("data", "processed", "raven", "raven_controls_scores.rds")
)

patients_raven_scores <- readRDS(
  here("data", "processed", "raven", "raven_patients_scores.rds")
)


controls_dat <- left_join(
  controls_raven_scores, controls_look_up_table, by = "code_psytoolkit")
controls_dat$code_psytoolkit <- NULL
controls_dat$group <- "controls"

patients_dat <- left_join(
  patients_raven_scores, patients_look_up_table, by = "code_psytoolkit")
patients_dat$code_psytoolkit <- NULL
patients_dat$group <- "patients"

raven_dat <- rbind(
  controls_dat, patients_dat
)


saveRDS(
  raven_dat,
  here("data", "processed", "raven", "raven_scores_both_groups.rds")
)

# e  n  d  ----
