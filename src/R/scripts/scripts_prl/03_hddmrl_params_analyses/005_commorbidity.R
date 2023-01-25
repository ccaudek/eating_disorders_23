# Script name: 005_commorbidity.R
# Project: eating disorders
# Script purpose: check differences on hddmrl parameters between
#   patients with and without commorbidity
# @author: Corrado Caudek <corrado.caudek@unifi.it>
# Date Created: Wed Nov 23 16:31:02 2022
# Last Modified Date: Wed Nov 23 16:31:02 2022
#
# 👉 There is no evidence of any difference in mean hddmrl parameters
#    values between patients with or without commorbidity.

library("here")
library("tidyverse")
library("stringr")
library("brms")

# hddmrl parameters.
params_prl_ddm <- rio::import(
  here::here(
    "src", "R", "scripts", "scripts_prl", "03_hddmrl_params_analyses", 
    "hddm_params_by_subj.txt")
)

# look-up table for the PRL task.
lookup_tbl <- rio::import(
  here::here(
    "data", "processed", "prl", "input_for_hddmrl", 
    "hddm_look_up_table_v3.csv")
)

  # get the first two characters from params_prl_ddm:
  params_prl_ddm$param <- substr(params_prl_ddm$id, start = 1, stop = 2)
  # params_prl_ddm$param[1:10]
  # [1] "a_" "a_" "a_" "a_" "a_" "a_" "a_" "a_" "a_" "a_"
  
  # get integers
  params_prl_ddm$id_param_string <- gsub(".*\\).", "", params_prl_ddm$id) 
  # params_prl_ddm$id_param_string <- stringr::str_remove(params_prl_ddm$ii, "[).]")
  params_prl_ddm$subj_idx <- as.integer(readr::parse_number(params_prl_ddm$id_param_string))
  # params_prl_ddm$subj_idx[1:10]
  # [1] 0 1 2 3 4 5 6 7 8 9
  
  # Change the levels names of the PRL parameters factor  
  # summary(factor(params_prl_ddm$param))
  params_prl_ddm$is_food <- stringr::str_detect(params_prl_ddm$id, "food")
  params_prl_ddm$is_neutral <- stringr::str_detect(params_prl_ddm$id, "neutral")
  # summary(factor(params_prl_ddm$param))
  
  # Create a single columns with the names of the PRL parameters
  params_prl_ddm <- params_prl_ddm %>% 
    mutate(stim = case_when(
      is_food == TRUE  & is_neutral == FALSE ~ "food",      
      is_food == FALSE & is_neutral == TRUE  ~ "neutral",  
      TRUE                                  ~ "neither")) 
  # data.frame(params_prl_ddm$ii, params_prl_ddm$stim)[500:1000, ]
  params_prl_ddm$param <- factor(params_prl_ddm$param)
  summary(params_prl_ddm$param)
  
  params_prl_ddm$params <- params_prl_ddm$param %>% 
    dplyr::recode(
      "a_" = "a",
      "al" = "alpha_neg",
      "po" = "alpha_pos",
      "t_" = "t",
      "v_" = "v",
      "z_" = "z"
    )
  # summary(factor(params_prl_ddm$params))
  
  params_prl_ddm_clean <- params_prl_ddm %>% 
    dplyr::select(
      subj_idx, stim, params, mean
    ) %>% 
    dplyr::rename(
      value = mean
    )
  
  params_prl_ddm_clean$pp <- 
    paste(params_prl_ddm_clean$params, params_prl_ddm_clean$stim, sep="_")
  
  params_prl_ddm_clean$stim <- NULL
  params_prl_ddm_clean$params <- NULL
  # summary(factor(params_prl_ddm_clean$pp))
  # a_neither alpha_neg_food alpha_neg_social alpha_pos_food ...
  params_prl_ddm_clean$pp <- factor(params_prl_ddm_clean$pp)
  
  # d <- left_join(params_prl_ddm_clean, lookup_tbl, by = "subj_idx") 
  
  t_df <- wrangle_params(params_prl_ddm_clean, lookup_tbl, 
                         "t_neutral", "t_food")
  t_df <- t_df %>% 
    dplyr::rename(
      t = value
    )
  
  a_df <- wrangle_params(params_prl_ddm_clean, lookup_tbl, 
                         "a_neutral", "a_food")
  a_df <- a_df %>% 
    dplyr::rename(
      a = value
    )
  
  v_df <- wrangle_params(params_prl_ddm_clean, lookup_tbl, 
                         "v_neutral", "v_food")
  v_df <- v_df %>% 
    dplyr::rename(
      v = value
    )
  
  alpha_neg_df <- wrangle_params(params_prl_ddm_clean, lookup_tbl, 
                                 "alpha_neg_neutral", "alpha_neg_food")
  alpha_neg_df <- alpha_neg_df %>% 
    dplyr::rename(
      alpha_neg = value
    )
  
  alpha_pos_df <- wrangle_params(params_prl_ddm_clean, lookup_tbl, 
                                 "alpha_pos_neutral", "alpha_pos_food")
  alpha_pos_df <- alpha_pos_df %>% 
    dplyr::rename(
      alpha_pos = value
    )
  
  z_df <- wrangle_param_z(params_prl_ddm_clean, lookup_tbl)
  z_df$subj_idx <- as.integer(z_df$subj_idx)
  
  
  # Create a df with all the parameters in wide format.
  
  temp1 <- inner_join(t_df, a_df, by = c("subj_code", "subj_idx", "stim"))
  temp2 <- inner_join(temp1, v_df, by = c("subj_code", "subj_idx", "stim"))
  temp3 <- inner_join(temp2, alpha_neg_df, by = c("subj_code", "subj_idx", "stim"))
  param_final <- inner_join(temp3, alpha_pos_df, by = c("subj_code", "subj_idx", "stim"))

  only_patients <- rio::import(
    here::here(
      "data", "raw", "diagnostic_categories", "diagnostic_categories.xlsx")
  )
  
  only_patients$has_commorbidity <- ifelse(
    only_patients$comorbidity_1 == "no" & 
      only_patients$comorbidity_2 == "no", 0, 1
  )
  
  patients_code <- only_patients$subj_code

params_patients <- param_final[param_final$subj_code %in% patients_code, ]

df <- data.frame(
  subj_code = only_patients$subj_code,
  has_commorbidity = only_patients$has_commorbidity
)

patients_df <- left_join(params_patients, df, by = "subj_code")

hist(patients_df$t)
plot(density(patients_df$v))
hist(patients_df$a)
hist(patients_df$alpha_neg)
plot(density(patients_df$alpha_pos))

t.test(t ~ has_commorbidity, patients_df)
t.test(v ~ has_commorbidity, patients_df)
t.test(a ~ has_commorbidity, patients_df)
t.test(alpha_neg ~ has_commorbidity, patients_df)
t.test(alpha_pos ~ has_commorbidity, patients_df)


m1 <- brm(
  v ~ has_commorbidity * stim + (stim | subj_code),
  data = patients_df, 
  family = skew_normal(),
  control = list(adapt_delta = 0.98),
  iter = 4000,
  cores = 6,
  backend = "cmdstan"
)

pp_check(m1)
summary(m1)
#                              Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS
# Intercept                        2.03      0.25     1.54     2.50 1.00     2560
# has_commorbidity                -0.55      0.37    -1.28     0.17 1.00     2158
# stimneutral                     -0.08      0.22    -0.51     0.37 1.00     5546
# has_commorbidity:stimneutral     0.07      0.32    -0.56     0.70 1.00     5542
# Tail_ESS
# Intercept                        4164
# has_commorbidity                 3290
# stimneutral                      4799
# has_commorbidity:stimneutral     5352

m2 <- brm(
  alpha_pos ~ has_commorbidity * stim + (stim | subj_code),
  data = patients_df, 
  family = skew_normal(),
  control = list(adapt_delta = 0.98),
  iter = 4000,
  cores = 6,
  backend = "cmdstan"
)

pp_check(m2)
summary(m2)
#                              Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS
# Intercept                       -0.77      0.44    -1.64     0.07 1.00     4194
# has_commorbidity                 0.24      0.66    -1.03     1.53 1.00     4338
# stimneutral                      1.72      0.54     0.65     2.78 1.00     4005
# has_commorbidity:stimneutral    -0.05      0.77    -1.57     1.46 1.00     4211
# Tail_ESS
# Intercept                        5271
# has_commorbidity                 5582
# stimneutral                      6078
# has_commorbidity:stimneutral     5407
