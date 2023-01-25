# Script name: funs_param_analyses.R
# Project: Eating disorders, Montecatini.
# Script purpose: Wrangling the parameters file created by hDDMrl for 
# classification of clinical status.
# @author: Corrado Caudek <corrado.caudek@unifi.it>
# Date Created: Mon Nov  1 06:53:31 2021
# Last Modified Date: Mon Nov  1 06:55:14 2021
# 
# Notes: 

#' @description Save RDS file with PRL params and subj_code.
#' @return 
#' NULL.
#' @save
#' RDS file.
add_subj_code_to_hddm_params_for_classification_and_save <- function() {
  
  # look-up table for the PRL task: integer IDs and subj_coe for all participants
  lookup_tbl <- rio::import(
    here::here("data", "processed", "prl", "data_for_hddm", 
               "hddm_look_up_table.csv")
  ) %>% 
    distinct(subj_idx, .keep_all = TRUE)
  
  
  # Edit the file containing the output of the Python hDDM analysis.
  # Correct the first line:
  # ii mean std 2.5q 25q 50q 75q 97.5q mc_err
  # Delete the last three lines and the lines with the estimates of the fixed
  # effects -- preserve only the estimates of the random effects
  params_prl_ddm <- rio::import(
    here::here("data", "processed", "prl", "output_hddm", 
               "params_hddm_no_split_by_group.txt")
  )
  
  # get the first two characters from params_prl_ddm:
  params_prl_ddm$param <- substr(params_prl_ddm$ii, start = 1, stop = 2)
  # unique(params_prl_ddm$param)
  # [1] "a(" "a_" "v(" "v_" "t(" "t_" "z"  "z_" "al" "po"
  
  # get integers
  params_prl_ddm$id_param_string <- gsub(".*\\).", "", params_prl_ddm$ii) 
  # params_prl_ddm$id_param_string <- stringr::str_remove(params_prl_ddm$ii, "[).]")
  params_prl_ddm$subj_idx <- as.integer(readr::parse_number(params_prl_ddm$id_param_string))
  # params_prl_ddm$subj_idx[1:10]
  # [1] 0 1 2 3 4 5 6 7 8 9
  
  # Change the levels names of the PRL parameters factor  
  # summary(factor(params_prl_ddm$param))
  params_prl_ddm$is_food <- stringr::str_detect(params_prl_ddm$ii, "food")
  params_prl_ddm$is_neutral <- stringr::str_detect(params_prl_ddm$ii, "neutral")
  # summary(factor(params_prl_ddm$param))
  
  # Create a single columns with the names of the PRL parameters
  params_prl_ddm <- params_prl_ddm %>% 
    mutate(stim = case_when(
      is_food == TRUE  & is_neutral == FALSE ~ "food",      
      is_food == FALSE & is_neutral == TRUE  ~ "neutral",  
      TRUE                                   ~ "neither")) 
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
  temp4 <- inner_join(temp3, alpha_pos_df, by = c("subj_code", "subj_idx", "stim"))
  param_final <- full_join(temp4, z_df, by = c("subj_code", "subj_idx"))
  
  saveRDS(
    param_final,
    here::here("data", "processed", "prl", "prl_params_cleaned", 
               "prl_params_for_classification_and_subj_code.rds")
  )
  
}



#' @description 
#' For each of the hDDM parameters (a, t, v, alpha_pos, alpha_neg), get a
#' Dataframe with the values of the parameter, for the food and neutral 
#' stimuli, and the subject code. 
#' @input params_prl_ddm_clean, lookup_tbl, and the name of the parameters:
#' "a_food", "a_neutral"; "alpha_neg_food", "alpha_neg_neutral"; 
#' "alpha_pos_food", "alpha_pos_neutral"; "t_food", "t_neutral"; 
#' "v_food", "v_neutral". 
#' @return 
#' Dataframe.
wrangle_params <- function(params_prl_ddm_clean, lookup_tbl,
                            PARAM_NEUTRAL, PARAM_FOOD) {

  neutral_df <- params_prl_ddm_clean %>%
    dplyr::filter(pp == PARAM_NEUTRAL)
  neutral_df$stim <- "neutral"
  neutral_df <- neutral_df %>%
    dplyr::select(subj_idx, stim, value)

  food_df <- params_prl_ddm_clean %>%
    dplyr::filter(pp == PARAM_FOOD)
  food_df$stim <- "food"
  food_df <- food_df %>%
    dplyr::select(subj_idx, stim, value)

  df <- bind_rows(neutral_df, food_df)
  df$stim <- factor(df$stim)

  # add subj_code
  param <- inner_join(df, lookup_tbl, by = "subj_idx") 
  
  param
}



#' @description 
#' Get the parameter z and subj_code 
#' @return 
#' Dataframe.
wrangle_param_z <- function(params_prl_ddm_clean, lookup_tbl) {
  
  # look-up table for the PRL task: integer IDs and subj_coe for all participants
  lookup_tbl <- rio::import(
    here::here("data", "processed", "prl", "data_for_hddm", "hddm_look_up_table.csv")
  ) %>% 
    distinct(subj_idx, .keep_all = TRUE)
  
  # Edit the file containing the output of the Python hDDM analysis.
  # Correct the first line:
  # ii mean std 2.5q 25q 50q 75q 97.5q mc_err
  # Delete the last three lines and the lines with the estimates of the fixed
  # effects -- preserve only the estimates of the random effects
  params_prl_ddm <- rio::import(
    here::here("data", "processed", "prl", "output_hddm", "params_hddm.txt")
  )
  
  z1_df <- dplyr::filter(params_prl_ddm, grepl("z_subj", ii))
  
  z2_df <- tibble(
    param = z1_df$ii, 
    z = z1_df$mean
  )
  
  z2_df$param <- as.character(z2_df$param)
  
  z2_df$subj_idx <- as.numeric(unlist(lapply(strsplit(z2_df$param, '.', fixed = TRUE), '[', 2)))
  
  z2_df$param <- NULL
  
  z3_df <- inner_join(z2_df, lookup_tbl, by = "subj_idx") 
  
  z3_df
}




get_a_param <- function() {
  
  # look-up table for the PRL task: integer IDs and subj_coe for all participants
  lookup_tbl <- rio::import(
    here::here("data", "processed", "prl", "data_for_hddm", "hddm_look_up_table.csv")
  ) %>% 
    distinct(subj_idx, .keep_all = TRUE)
  
  # file with the output of the python hDDM analysis.
  # Correct the first line:
  # ii mean std 2.5q 25q 50q 75q 97.5q mc_err
  # Delete the last three lines and the lines with the estimates of the fixed
  # effects -- preserve only the estimates of the random effects
  params_prl_ddm <- rio::import(
    here::here("data", "processed", "prl", "output_hddm", "params_hddm.txt")
  )
  
  # get the first two characters from params_prl_ddm:
  params_prl_ddm$param <- substr(params_prl_ddm$ii, start = 1, stop = 2)
  # params_prl_ddm$param[1:10]
  # [1] "a_" "a_" "a_" "a_" "a_" "a_" "a_" "a_" "a_" "a_"
  
  # get integers
  params_prl_ddm$id_param_string <- gsub(".*\\).", "", params_prl_ddm$ii) 
  # params_prl_ddm$id_param_string <- stringr::str_remove(params_prl_ddm$ii, "[).]")
  params_prl_ddm$subj_idx <- as.integer(readr::parse_number(params_prl_ddm$id_param_string))
  # params_prl_ddm$subj_idx[1:10]
  # [1] 0 1 2 3 4 5 6 7 8 9
  
  # Change the levels names of the PRL parameters factor  
  # summary(factor(params_prl_ddm$param))
  params_prl_ddm$is_food <- stringr::str_detect(params_prl_ddm$ii, "food")
  params_prl_ddm$is_neutral <- stringr::str_detect(params_prl_ddm$ii, "neutral")
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
  
  d <- left_join(params_prl_ddm_clean, lookup_tbl, by = "subj_idx") 
  
  
  
  a_neutral_df <- params_prl_ddm_clean %>%
    dplyr::filter(pp == "a_neutral")
  a_neutral_df <- dplyr::rename(a_neutral_df, 
                                a = pp
  )
  a_neutral_df$stim <- "neutral"
  a_neutral_df <- a_neutral_df %>%
    dplyr::select(subj_idx, a, stim, value)
  
  
  a_food_df <- params_prl_ddm_clean %>%
    dplyr::filter(pp == "a_food")
  a_food_df <- dplyr::rename(a_food_df, 
                             a = pp
  )
  a_food_df$stim <- "food"
  a_food_df <- a_food_df %>%
    dplyr::select(subj_idx, a, stim, value)
  
  a_df <- bind_rows(a_neutral_df, a_food_df)
  a_df$a <- factor(a_df$a)
  a_df$stim <- factor(a_df$stim)
  
  # create a wide data.frame with one column for each PRL parameter
  # df_wide <- params_prl_ddm_clean %>%
  #   pivot_wider(names_from = pp, values_from = value) 
  
  # add subj_code
  a_param <- inner_join(a_df, lookup_tbl, by = "subj_idx") 
  
  a_param
}





