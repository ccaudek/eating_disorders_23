# Script name: 005_write_input_for_hddm.R
# Project: eating disorders Montecatini
# Script purpose: write csv file for HDDM
# @author: Corrado Caudek <corrado.caudek@unifi.it>
# Date Created: Thu Jun  2 09:08:33 2022
# Last Modified Date: Thu Jun  2 09:08:33 2022
#
# 👉 

# Prelims
library("here")
library("tidyverse")
library("stringi")

# Increase max print
options(max.print = .Machine$integer.max)


# Read and combine the two data sets.
d1 <- readRDS(
  here::here("data", "processed", "task_switching", "controls_task_switching_data.rds")
)
d1$is_patient <- 0

d2 <- readRDS(
  here::here("data", "processed", "task_switching", "patients_task_switching_data.rds")
)
d2$is_patient <- 1

df <- rbind(d1, d2)


# Correct participants' identifiers ---------------------------------------

# quest_data$subj_code %>% sort()
# unique(df$subj_id) %>% sort()

# Correct subjects identifiers.
df$subj_id <- forcats::fct_recode(
  df$subj_id,
  "as_mi_1999_03_27_854_f" = "asia_milani_1999_03_27_854_f",
  "ch_be_1990_12_20_153_f" = "chiara_benazzi_1990_12_20_153_f", 
  "cr_ci_1999_08_21_931_f" = "cristina_cinque_1999_08_21_931_f", 
  "fe_sa_2002_05_09_008_f" = "fe_sa_2002_05_09_08_f", 
  "gi_po_2001_05_17_867_f" = "giulia_polichetti_2001_05_17_867_f", 
  "gi_to_1997_07_30_762_f" = "giulia_toma_1997_07_30_762_f",
  "la_sc_2001_11_22_662_f" = "laura_scanu_2001_11_22_662_f", 
  "ma_te_2001_05_31_333_m" = "ma a_te_2001_05_31_333_m",
  "ni_pr_1999_10_16_006_f" = "ni_pr_1999_10_16_06_f",
  "ar_co_1996_12_27_348_f" = "ar_co_1996_12_27_767_f", 
  "ch_ma_2001_10_27_332_f" = "ch_ma_2001_10_27_331_f",
  "cr_gi_1994_10_14_347_f" = "cr_gi_1994_10_14_378_f"
)

df <- df %>% 
  dplyr::rename(
    subj_code = subj_id
  )

# Change group membership -------------------------------------------------

# 6 subj_code in patients group are NOT patients!
not_patients <- c(
  "is_ie_1986_08_18_291_f",
  "la_al_1996_06_14_190_f",
  "gr_ma_1995_09_05_060_f",
  "fr_ma_1996_02_16_959_f",
  "st_sa_1996_06_05_556_f",
  "al_pe_1996_09_02_886_m"
)

df$is_patient <- ifelse(
  df$subj_code %in% not_patients, 0, df$is_patient
)

# only_patients <- df %>% 
#   dplyr::filter(is_patient == 1)
# unique(only_patients$subj_code) %>% sort()

# task_type: at the trial level, 
# is 1 when the task is food
# is 2 when the task is plants
df <- df %>% 
  dplyr::rename(
    trial_task_type = task_type
  )

df <- df %>% 
  dplyr::rename(
    stim = stim_category
  )

df$stim <- df$stim %>% 
  dplyr::recode(
    "letters" = "food", 
    "numbers" = "plants"
  )

# table(df$stim)

df$block_type <- NULL

df$is_correct <- ifelse(df$is_correct == 3, NA, df$is_correct)
df$is_correct <- ifelse(df$is_correct == 2, 0, df$is_correct)


# Add diagnostic categories -----------------------------------------------

diagn_categories <- readRDS(
  here::here("data", "processed", "quest", "quest_diagn_data.rds")
) %>%
  dplyr::select(
    subj_code, is_patient, diag_cat
  )
unique(diagn_categories$subj_code) %>% length()
# [1] 332

diagn_categories$diag_cat <- as.character(diagn_categories$diag_cat)

df1 <- left_join(df, diagn_categories)
# summary(factor(df1$diag_cat))

# "ch_ri_1993_05_05_564_f" "gi_za_1992_09_07_575_f"
df1$diag_cat <- ifelse(
  (is.na(df1$diag_cat) == 1 & df1$subj_code == "ch_ri_1993_05_05_564_f"), 
  "AN", df1$diag_cat
)

df1$diag_cat <- ifelse(
  (is.na(df1$diag_cat) == 1 & df1$subj_code == "gi_za_1992_09_07_575_f"), 
  "AN", df1$diag_cat
)

df1$diag_cat <- ifelse(is.na(df1$diag_cat) == 1, "HC", df1$diag_cat)
table(df1$diag_cat, df1$is_patient)

df1$rt <- ifelse((df1$rt < 200 | df1$rt >  4900), NA, df$rt)


# Imputation on rt and is_correct -----------------------------------------

set.seed(123)
miceObj <- miceRanger::miceRanger(
  df1
  , m = 1
  , returnModels = TRUE
  , verbose = FALSE
)

dataList <- miceRanger::completeData(miceObj)

df2 <- dataList[[1]]


# Save task-switching data for descriptive statistics ---------------------

saveRDS(
  df2, 
  here::here("data", "processed", "task_switching", "data_for_descript_stats", 
             "task_switch_data_for_descript_stats.rds")
)


# Create names for HDDM input ---------------------------------------------

df2$subj_idx <- df2$subj_code 
df2$subj_idx <- as.numeric(factor(as.character(df2$subj_idx))) - 1
# unique(df2$subj_idx ) %>% sort()

# Accuracy coding for response.
df2$response <- df2$is_correct

df2 <- df2 %>% 
  mutate(
    rt = rt / 1000
  )

# tibble(
#   df2$stim, df2$trial_task_type, df2$is_task_switch
# )[1:240, ] %>% as.data.frame()


df2 <- df2 %>% 
  dplyr::rename(
    task = trial_task_type,
    switch = is_task_switch
  )

df2$task <- df2$task
df2$switch <- df2$switch

df2$task <- df2$task %>% 
  dplyr::recode(
    "1" = "food", 
    "2" = "plants"
  )

df2$switch <- df2$switch %>% 
  dplyr::recode(
    "1" = "switch", 
    "0" = "repeat"
  )


df3 <- df2 %>% 
  dplyr::select(
    subj_code, subj_idx, trial, task, switch, response, rt, diag_cat, is_patient
  )

for_python <- df3 %>% 
  arrange(subj_idx, trial)

bysubj_n <- for_python %>% 
  group_by(subj_code) %>% 
  summarise(
    n = n()
  )
unique(bysubj_n$n)
length(unique(for_python$subj_code))
120*355 == nrow(for_python)

# Save HDDM input
rio::export(
  for_python, 
  here::here("data", "processed", "task_switching", "input_for_hddm", 
             "input_hddm_task_switching.csv")
)



# Look-up table -----------------------------------------------------------

lookup_table <- for_python %>% 
  group_by(subj_code, subj_idx, diag_cat) %>% 
  summarise(
    n = n()
  )
# nrow(lookup_table)
# length(unique(lookup_table$subj_code))
lookup_table$n <- NULL

saveRDS(
  lookup_table,
  here::here("data", "processed", "task_switching", "input_for_hddm", 
             "lookup_table_task_switching.rds")
)


# End of file -------------------------------------------------------------










#------------------------------------------------------------------

# 
# 
# table(df$is_correct)
# 
# bysubj <- df %>% 
#   dplyr::filter(is_correct == 1) %>% 
#   group_by(subj_id, is_task_switch) %>% 
#   summarise(
#     avg_rt = mean(rt, na.rm = TRUE, trim = 0.1)
#   )
# 
# 
# bysubj_wide <- bysubj %>% 
#   pivot_wider(names_from = is_task_switch, values_from = avg_rt)
# 
# bysubj_switch_cost <- bysubj_wide
# bysubj_switch_cost$cost <- bysubj_switch_cost$`1` - bysubj_switch_cost$`0`
# 
# 
# boxplot(bysubj$avg_rt ~ bysubj$is_task_switch)
# 
# hist(bysubj_switch_cost$cost)
# 
# 
# 
# t.test(bysubj$avg_rt ~ bysubj$is_task_switch)
# 
# 
# 
# # data for python analysis ----
# 
# foo <- unique(df$subj_id)
# as.numeric(factor(foo))
# 
# length(unique(df$subj_id))
# 
# 
# 
# df1 <- df[complete.cases(df), ]
# 
# df1 <- df1 %>% 
#   mutate(
#     rt = rt / 1000
#   )
# 
# table(df1$is_correct)
# 
# df2 <- df1 %>% 
#   dplyr::filter(is_correct != 3)
# 
# 
# hist(df2$rt)
# 
# df1 %>% 
#   group_by(subj_id) %>% 
#   summarise(
#     n = n()
#   ) %>% 
#   as.data.frame()
# 
# 
# for_python <- df2 %>% 
#   dplyr::rename(
#     subj_idx = subj_id,
#     response = is_correct
#   ) %>% 
#   select(subj_idx, rt, response, stim_category)
# 
# 
# for_python$subj_idx <- as.numeric(factor(for_python$subj_idx)) - 1
# for_python$response <- ifelse(for_python$response == 2, 0, for_python$response)
# 
# foo <- for_python %>% 
#   arrange(subj_idx)
# 
# 
# rio::export(foo, "task_switching.csv")
# 


# E N D 


