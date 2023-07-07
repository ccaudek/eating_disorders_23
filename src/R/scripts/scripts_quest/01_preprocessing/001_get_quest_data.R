# Script name: 005_get_quest_data.R
# Project: Eating disorders Montecatini
# Script purpose: Generate and save RDS file with questionnaires data.
# @author: Corrado Caudek <corrado.caudek@unifi.it>
# Date Created: Wed Jun  2 10:07:50 2021
# Last Modified Date: Sun May 29 22:58:28 2022
# 
# Notes: 
# The complete and final file with the questionnaires data (patients and 
# controls) is:
# here::here("data", "processed", "quest", "quest_data.rds")


# Prelims
suppressPackageStartupMessages({
  library("here")
  library("tidyverse", warn.conflicts = FALSE)
  library("stringi")
  library("readxl")
  library("miceRanger")
})

# Increase max print
options(max.print = .Machine$integer.max)

# source(here("code", "functions", "funs_prl.R"))
source(here("scripts", "functions", "funs_quest.R"))



# 1. Get single file with questionnaire raw data --------------------------

# Combine multiple Excel Worksheets into a single RDS file containing the raw 
# questionnaires data of both patients and controls.

gen_complete_raw_quest_data()

# if (!file.exists(
#   here::here(
#     "data", "processed", "quest", "complete_raw_quest_pat_and_cont.rds"
#   )
# )) {
#   gen_complete_raw_quest_data()
# }


# 2. Correct participants identifiers -------------------------------------

# Correct participants identifiers and replace questionnaires file
# here::here("data", "processed", "quest", "complete_raw_quest_pat_and_cont.rds")

correct_subj_identifiers_in_quest_data()


# 3.  Clean questionnaires data -----------------------------------------

# (1) Change columns names. 
# (2) Remove rows of subjects who have redone the questionnaires multiple times. 
# (3) Cleans subj_code vector. 
# (4) Remove participants with wrong code.

clean_quest_data()


# 4. Scoring questionnaires data ------------------------------------------

# Save scored complete questionnaires data (patients and controls) in 
# here::here("data", "processed", "quest", "quest_data.rds")

save_scored_quest_data()


# 5. Get demographic information ------------------------------------------

# Read corrected raw questionnaires data.

raw_quest <- readRDS(
  here::here("data", "processed", "quest", "complete_raw_quest_pat_and_cont.rds")
)

subj_info <- get_subj_info(raw_quest)


# 6. Join questionnaires data with demographic information ----------------

recoded_quest_data <- readRDS(
  here::here("data", "processed", "quest", "quest_data.rds")
)

all_quest_data <- left_join(recoded_quest_data, subj_info, by = "subj_code")

saveRDS(
  all_quest_data,
  here::here("data", "processed", "quest", "quest_data_and_subj_info.rds")
)


#---- EOF ----#




