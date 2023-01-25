# Script name: 01_read_wcst_data.R
# Project: Eating disorders Montecatini
# Script purpose: Read raw data and save summary descriptive statistics.
# @author: Corrado Caudek <corrado.caudek@unifi.it>
# Date Created: Tue May 24 16:49:19 2022
# Last Modified Date: Tue May 24 16:49:19 2022
#
# 👉 


# Script: 01_read_data.R
#
# Project: Eating disorders Montecatini
# Version: Tue May 24 16:49:19 2022
#-------------------------------------------------------------------- 


# Prelims
library("here")
library("tidyverse")
# source(here("scripts", "01_prelims.R"))
library("stringi")


#dir <- here("data", "raw", "wcst", "patients")
dir <- here("data", "raw", "wcst", "controls")

# file_names <- as.character(list.files(path=dir, pattern="wcst_pazienti"))
file_names <- as.character(list.files(path=dir, pattern="wcst_eds1"))

n_files <- length(file_names)
n_files

d_list <- list()

for (i in 1:n_files) {
  
  # d  <- read.table(here("data", "raw", "wcst", "patients", file_names[i]), header = FALSE)
  d  <- read.table(here("data", "raw", "wcst", "controls", file_names[i]), header = FALSE)
  
  d$subj_name <- file_names[i]
  d$block <- rep(1:6, each = 10)
  
  d$card_shown <- d$V1
  d$correct_card <- d$V2
  d$card_chosen_if_perseveration <- d$V3
  d$trial_in_a_sequence <- d$V4 
  d$name_of_task <- d$V5
  d$card_shape <- d$V6
  d$card_number_of_symbols <- d$V7
  d$card_color <- d$V8
  d$rt <- d$V9
  d$is_correct <- d$V10 # 1=correct, 2=wrong card, 3=too slow
  d$chosen_card <- d$V11 # a number between 1 and 4, or 0 if none clicked
  d$is_error <- d$V12 
  # If 1, this trial was an error (otherwise 0); 0 or 1
  d$is_perseverative_error <- d$V13 
  # If 1, this trial was a perseveration error (otherwise 0); 0, 1
  d$is_non_perseverative_error <- d$V14
  # If 1, this trial was not a perseveration error (otherwise 0); 0, 1
  
  d_list[[i]] <- d
  
}

# convert list into data.frame
df <- do.call(rbind.data.frame, d_list)

df$correct <- ifelse(
  df$is_correct == 1, 1, 
  ifelse(df$is_correct == 2, 0, NA)
)


mydat <- tibble(
  subjID = as.numeric(factor(as.character(df$subj_name))),
  choice = df$chosen_card,
  outcome = df$correct
)

saveRDS(
  mydat, 
  here("data", "processed", "wcst", "wcst_input_for_hBayesDM_controls.rds")
)


wcst_descript <- df %>% 
  group_by(subj_name) %>% 
  summarise(
    prop_pers_err = sum(is_perseverative_error) / n(),
    prop_non_pers_err = sum(is_non_perseverative_error) / n(),
    prop_err = sum(is_error) / n(),
    prop_cor = mean(correct, na.rm = TRUE)
  ) %>% 
  as.data.frame()

# hist(wcst_descript$prop_cor)
# hist(wcst_descript$prop_err)
# hist(wcst_descript$prop_non_pers_err)
# hist(wcst_descript$prop_pers_err)


# save data in RDS files
# saveRDS(
#   wcst_descript, 
#   here("data", "processed", "wcst", "wcst_descript_patients.rds")
# )

saveRDS(
  wcst_descript, 
  here("data", "processed", "wcst", "wcst_descript_controls.rds")
)


# E N D 

