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

d <- left_join(subjects_296_data, comorbidity, by = "subj_code")

keep <- c("AN", "BN")
d1 <- d[d$diag_cat %in% keep, ]

length(unique(d1$subj_code))

d1$comorbidity <- ifelse(d1$comorbidity_1 == "no", 0, 1)
d1$split_by <- d1$comorbidity

grpid = function(x) match(x, unique(x))
# then
d1 <- d1 %>% 
  mutate(subj_idx = group_indices(., subj_code) %>% grpid)
# In this manner, the variable subj_idx assigns an integer to each subject;
# this integer is ordered according to the sequence in which the subjects are 
# present in the data.frame.

d1$medication <- ifelse(d1$therapy == "si", 1, 0)

unique(d1$subj_idx)

rio::export(
  d1, 
  here::here(
    "data", "processed", "prl", "input_for_hddmrl", "comorbidity", 
    "an_bn_comorbidity.csv"
  )
)

# Comorbidity 
temp <- d1 |> 
  group_by(subj_code, diag_cat, comorbidity) |> 
  summarise(
    avg_fdbk = mean(feedback),
  ) 

table(temp$comorbidity, temp$diag_cat)
#   AN BN
# 0 21  7
# 1 16  5


# eof ---
