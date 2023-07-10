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
library("rio")


an_data <- rio::import(
  here("data", "processed", "prl", "input_for_hddmrl", "three_groups", 
       "ed_prl_data.csv")
) |> 
  dplyr::filter(diag_cat=="AN")
length(unique(an_data$subj_code))


comorbidity <- rio::import(
  here("data", "raw", "diagnostic_categories", "diagnostic_categories.xlsx")
)

d <- left_join(an_data, comorbidity, by = "subj_code")

d$comorbidity <- ifelse(d$comorbidity_1 == "no", 0, 1)

d$split_by <- as.numeric(as.factor(d$stim)) - 1  # 0: food; 1: neutral

# The match() function is used to find the position of each unique 
# value of subj_code within the vector of all unique values. This 
# effectively assigns an integer to each subject based on their order 
# in the data frame.
d <- d %>%
  mutate(subj_idx = match(subj_code, unique(subj_code)))

d$medication <- ifelse(d$therapy == "si", 1, 0)

d <- d |> 
  dplyr::select(
    -c(therapy, diagnosis, category, comorbidity_1, comorbidity_2, is_recovered)
  )

df_sorted <- d[order(d$subj_idx, d$stim, d$trial), ]

rio::export(
  df_sorted, 
  here::here(
    "data", "processed", "prl", "input_for_hddmrl", "comorbidity", 
    "an_comorbidity.csv"
  )
)

# Checking. 
temp <- df_sorted |> 
  group_by(subj_code, diag_cat, comorbidity) |> 
  summarise(
    avg_fdbk = mean(feedback),
  ) 

table(temp$comorbidity, temp$diag_cat)
# AN
# 0 20
# 1 16


# eof ---
