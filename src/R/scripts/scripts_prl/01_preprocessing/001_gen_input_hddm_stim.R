# Script name: 001_gen_data_for_hddmrl_food_neutral.R
# Project: eating disorders, Montecatini
# Script purpose: create single file with all raw PRL data.
# @author: Corrado Caudek <corrado.caudek@unifi.it>
# Date Created: Wed Jun  2 10:07:50 2021
# Last Modified Date: Sat Jun 18 10:04:13 2022
# 
# Notes: 
#   1. Identify at-risk controls (EAT-26) and exclude them.

# Prelims
suppressPackageStartupMessages({
  library("here")
  library("tidyverse")
  library("stringi")
  library("readxl")
})

# Increase max print
options(max.print = .Machine$integer.max)

# source(here("code", "functions", "funs_gen_data_for_hddm.R"))
source(here::here("src", "R", "functions", "funs_prl.R"))


# 1. Merge psytoolkit PRL files  ------------------------------------------

# Read individual psytoolkit PRL files and generate 4 RDS files.
# E.g. here("data", "processed", "prl", "data_social", 
# "controls_with_psychtoolkit_code.rds")

load_psychtoolkit_files()


# 2.  Create data list ----------------------------------------------------

# Read the RDS files files with raw PRL data for each group and condition and 
# create a data_list. We also add the participant's identifier (es., 
# ca_po_2002_05_25_700_f).

# The data_list includes one data.frame for each condition:
#
# data_list[[1]] : df_food_patients
# data_list[[2]] : df_food_controls
# data_list[[3]] : df_social_patients
# data_list[[4]] : df_social_controls
data_list <- write_prl_raw_data_list()


# 3. Corrected subj_name --------------------------------------------------

# For each element of the list, we correct the subject's identifier.

d_list <- correct_subj_names(data_list)
# unique(d_list[[1]]$subj_name)
# unique(d_list[[2]]$subj_name)
# unique(d_list[[3]]$subj_name)
# unique(d_list[[4]]$subj_name)
# The returned list has the same structure as data_list. 


# 4. Clean and save single file --------------------------------------------

# Remove PRL sessions with too many NAs, bind the data frames in list and save 
# cleaned file in 
# here("data", "processed", "prl", "complete_cleaned_raw_data", 
# "raw_data_prl_both_groups.rds")
binding_cleaned_data_frames(d_list)


# 5.  Add diagnostic category ---------------------------------------------

# here::here("data", "processed", "prl", "raw_prl_data", "prl_tot_raw_data.rds")
add_diagnostic_category()


# 6. Create input for HDDMrl ----------------------------------------------

# Integrating multiple careless responding criteria for flagging careless 
# responding participants of control group.
# here("data", "processed", "prl", "input_for_hddmrl", "hddm_input_v3.csv")
# here("data", "processed", "prl", "input_for_hddmrl", "hddm_look_up_table_v3.csv")
write_input_for_hddmrl()



# ----- eof -------

