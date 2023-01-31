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

to_be_deleted = c(
  "lu_mu_1997_03_18_059_f",
  "de_sc_1992_07_02_116_f",
  "fr_ro_1982_08_15_048_f",
  "el_li_1999_09_08_687_f",
  "ma_pi_2001_05_11_566_f",
  "sa_sa_2000_11_24_418_m"
)

subjects_296_data <- 
  subjects_305_data[!(subjects_305_data$subj_code %in%to_be_deleted), ]

length(unique(subjects_296_data$subj_code))

anorexic_raw_data <- subjects_305_data |> 
  dplyr::filter(diag_cat == "AN")

length(unique(anorexic_raw_data$subj_code))

# 37 anorexic patiens.
comorbidity$comorbidity <- ifelse(comorbidity$comorbidity_1 == "no", 0, 1)
comorbidity$medication <- ifelse(comorbidity$therapy == "si", 1, 0)

anorexic_comorb_df <- comorbidity |> 
  dplyr::filter(category == "AN") |> 
  dplyr::select(subj_code, comorbidity, medication) 

# length(anorexic_comorb_df$subj_code)
# length(unique(anorexic_raw_data$subj_code))
# 
# setdiff(anorexic_comorb_df$subj_code, unique(anorexic_raw_data$subj_code))
# setdiff(unique(anorexic_raw_data$subj_code), anorexic_comorb_df$subj_code)

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
# 
# tbl_anorexics <- comorbidity |> 
#   dplyr::filter(category == "AN")
# 
# tbl_anorexics$comorbidity <- ifelse(
#   tbl_anorexics$comorbidity_1 == "no", 0, 1
# )
# 
# tbl_anorexics$medication <- ifelse(tbl_anorexics$therapy == "si", 1, 0)
# 
# cor(
#   tbl_anorexics$comorbidity, 
#   tbl_anorexics$medication,
#   method = "spearman"
# )
# 0.8081744

rio::export(
  d, 
  here::here(
    "data", "processed", "prl", "input_for_hddmrl", "comorbidity", 
    "an_comorbidity_2023_01_31.csv"
  )
)

# Comorbidity 
temp <- d |> 
  group_by(subj_code, comorbidity) |> 
  summarise(
    avg_fdbk = mean(feedback),
  ) 

table(temp$comorbidity)
# 0  1 
# 21 16 

comorbidity <- rio::import(
  here("data", "raw", "diagnostic_categories", "diagnostic_categories.xlsx")
)

temp <- comorbidity |> 
  group_by(subj_code, diagnosis) |> 
  summarise(
    avg_fdbk = mean(feedback, diag_cat),
  ) 


# Summary statistics

not_include = c("HC", "RI")

temp <- left_join(subjects_296_data, comorbidity, by = "subj_code")
temp1 <- temp |> 
  # dplyr::filter(!(diag_cat %in% not_include)) |> 
  #dplyr::filter(diag_cat == "AN") |> 
  dplyr::filter(diag_cat == "BN") |> 
  group_by(subj_code, diag_cat, comorbidity_1, medication) |> 
  summarise(
    avg_rt = mean(rt)
  ) 


table(temp1$comorbidity_1)


# eof ---

# table(d$comorbidity, d$medication)
