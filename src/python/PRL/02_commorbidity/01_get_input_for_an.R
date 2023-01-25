# Script name: 01_get_input_for_an.R
# Project: eating disorders
# Script purpose: from the data used in the main analysis, get only
#  the data of AN patiens. Add comorbidity column.
# @author: Corrado Caudek <corrado.caudek@unifi.it>
# Date Created: Tue Nov 29 15:32:36 2022
# Last Modified Date: Tue Nov 29 15:32:36 2022
#
# 👉 
library("here")
library("tidyverse")


subjects_305_tbl <- rio::import(
  here("data", "processed", "prl", "input_for_hddmrl", "hddm_look_up_table_v3.csv")
)

subjects_305_data <- rio::import(
  here("data", "processed", "prl", "input_for_hddmrl", "hddm_input_v3.csv")
)

comorbidity <- rio::import(
  here("data", "raw", "diagnostic_categories", "diagnostic_categories.xlsx")
)

anorexic_raw_data <- subjects_305_data |> 
  dplyr::filter(diag_cat == "AN")

# 41 anorexic patiens.
anorexic_comorb$comorbidity <- ifelse(anorexic_comorb$comorbidity_1 == "no", 0, 1)

anorexic_comorb_df <- anorexic_comorb |> 
  dplyr::select(subj_code, comorbidity) 

length(anorexic_comorb_df$subj_code)
length(unique(anorexic_raw_data$subj_code))

setdiff(anorexic_comorb_df$subj_code, unique(anorexic_raw_data$subj_code))
setdiff(unique(anorexic_raw_data$subj_code), anorexic_comorb_df$subj_code)

d <- left_join(anorexic_raw_data, anorexic_comorb_df, by = "subj_code")
d$split_by <- d$comorbidity

grpid = function(x) match(x, unique(x))
# then
d <- d %>% 
  mutate(subj_idx = group_indices(., subj_code) %>% grpid)
# In this manner, the variable subj_idx assigns an integer to each subject;
# this integer is ordered according to the sequence in which the subjects are 
# present in the data.frame.

unique(d$subj_idx)

tbl_anorexics <- comorbidity |> 
  dplyr::filter(category == "AN")

tbl_anorexics$comorbidity <- ifelse(
  tbl_anorexics$comorbidity_1 == "no", 0, 1
)

tbl_anorexics$medication <- ifelse(tbl_anorexics$therapy == "si", 1, 0)

cor(
  tbl_anorexics$comorbidity, 
  tbl_anorexics$medication,
  method = "spearman"
)
# 0.8081744

rio::export(
  d, 
  here::here(
    "data", "processed", "prl", "input_for_hddmrl", "comorbidity", 
    "an_comorbidity.csv"
  )
)

# eof ---
