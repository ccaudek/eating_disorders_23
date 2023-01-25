# Script name: ed_rescorla_wagner_fnc.R
# Project: Eating disorders Montecatini
# Script purpose: Functions for obtaining the R-W parameters computed
# subject-by_subject
# @author: Corrado Caudek <corrado.caudek@unifi.it>
# Date Created: Thu Oct 21 11:27:17 2021
# Last Modified Date: Thu Oct 21 11:27:17 2021
# 
# Notes: 



# Compute Rescorla-Wagner parameters for each subject ---------------------


#' @description 
#' Generate CSV files to be used as input for Rescorla-Wagner fit on 
#' PRL data. The alpha_pos, alpha_neg and beta parameters are 
#' computed subject-by-subject (no hierarchical model)-- see Zorowitz, 
#' Niv, Bennett (2021).
#' @input
#' Address of file with raw PRL data. When subjects are removed, with the 
#' following functions, this same file will be updated and overwritten.
#' @return 
#' NULL.
#' @save
#' File "prl_data_for_rw.csv".
gen_data_for_stan_rescorla_wagner <- function(cleaned_raw_prl_data_file_address) {
  
  # Read complete data set (both patients and controls)
  raw_data <- readRDS(cleaned_raw_prl_data_file_address)

  # Variable response: which image has been chosen on each trial.
  # It must be 1 or 2 because these numbers will serve as integers in the Stan 
  # script.
  raw_data$response <- raw_data$is_target_img_chosen + 1
  
  raw_data$feedback <- 
    ifelse(raw_data$feedback == 2, 0, raw_data$feedback)
  raw_data$feedback <- 
    ifelse(raw_data$feedback == 3, NA, raw_data$feedback)
  
  raw_data$feedback <- 
    ifelse(raw_data$rt < 50 | raw_data$rt > 2499, NA, raw_data$feedback)
  
  raw_data$rt1 <- 
    ifelse(raw_data$rt < 50 | raw_data$rt > 2499, NA, raw_data$rt)
  
  raw_data$is_patient <- ifelse(raw_data$group == "patients", 1, 0)
  
  temp <- data.frame(
    rt1 = raw_data$rt1, 
    trial = raw_data$trial,
    feedback = raw_data$feedback, 
    raw_data$is_target_rewared_in_present_epoch,
    is_patient = raw_data$is_patient,
    response = raw_data$response
  )
  
  # Imputes the "best value" according to the linear regression model, also 
  # known as regression imputation.
  imp <- mice::mice(temp, method = "norm.predict", m = 1) 
  temp2 <- complete(imp)
  
  raw_data$feedback <- ifelse(temp2$feedback > 0.5, 1, 0)
  raw_data$rt <- temp2$rt1
  
  # select participants with 320 trials each
  by_subj_ntrials <- raw_data %>% 
    group_by(subj_name) %>% 
    summarise(
      n_trials = n()
    ) 
  
  d1 <- left_join(raw_data, by_subj_ntrials, by = "subj_name")
  
  # d1 <- d %>% 
  #   dplyr::filter(n_trials == 320)
  
  # I want to number each subject in the data.frame so that subjects are 
  # ordered sequentially, according to the order they appear in the data frame. 
  # https://community.rstudio.com/t/why-does-group-indices-use-alphabetical-ordering/5452
  # As suggested in the above link, I wrap group_indices in another function:
  grpid = function(x) match(x, unique(x))
  # then
  d2 <- d1 %>% 
    mutate(subj_idx = group_indices(., subj_name) %>% grpid)
  # In this manner, the variable subj_idx assigns an integer to each subject;
  # this integer is ordered according to the sequence in which the subjects are 
  # present in the data.frame.
  # table(d2$subj_idx)
  # unique(d2$subj_idx)
  
  # Create look-up table with correspondence between subj_idx and the string
  # containing subj_name (the subject's code).
  lookup_tbl <- d2 %>% 
    dplyr::select(subj_idx,	subj_name, is_patient, n_trials) 
  
  # Remove duplicate rows
  deduped_tbl <- unique(lookup_tbl[, 1:4])
  
  # Save deduped_tbl in csv format.
  rio::export(
    deduped_tbl, 
    here(
      "data", "processed", "prl", "data_for_rescorla_wagner", 
      "prl_look_up_table.csv"
    )
  )
  
  # In the Stan script rl_indiv.stan, the variable reward must be -1 or 1.
  d2$reward <- ifelse(d2$feedback == 1, 1, -1)
  
  # Select appropriate columns of d2 for Stan.
  mydat <- d2 %>% 
    dplyr::select(subj_idx, response,	reward, n_trials) %>% 
    dplyr::rename(
      choice = response
    )
  # table(mydat$subj_idx)
  
  # Save data in CSV files
  rio::export(
    mydat, 
    here(
      "data", "processed", "prl", "data_for_rescorla_wagner", 
      "prl_data_for_rw.csv")
  )
  
}



#' @description 
#' Generate input for Stan Rescorla-Wagner model.
#' @input
#' NULL.
#' @return 
#' Data list in the format necessary for running the Stan Rescorla-Wagner model.
get_list_for_stan <- function() {
  
  # Import raw data of all subjects and all sessions in the PRL task.
  dd <- rio::import(
    here("data", "processed", "prl", "data_for_rescorla_wagner", 
         "prl_data_for_rw.csv")
  )
  
  # Import the look-up table with subj_idx and the subject's code.
  prl_look_up_table <- rio::import(
    here(
      "data", "processed", "prl", "data_for_rescorla_wagner", 
      "prl_look_up_table.csv"
    )
  )
  
  # For the choice values, I need to create a matrix with number of rows equal to
  # the subject and number of columns equal to 320 (the maximum number of trials).
  # The same for the rewards. Creating these two matrices is required to create
  # the appropriate input for the Stan code.
  #
  # To generate the matrix of size n_subject x 320, I first create a list in 
  # in which each element is a subject. I want, for each element of the list, a
  # vector of 320 elements. If the i-th subject completed 320 trials (i.e., two 
  # blocks), then the size of the i-the vector will be 320. If the subject only 
  # completed one block (160 trials), then to the corresponding vector I will add 
  # 160 zeros.
  len <- length(unique(dd$subj_idx))
  
  choice_list <- vector(mode = "list", length = len)
  
  for (i in 1:len) {
    if (i == 1) {
      nini <- 1
      nend <- prl_look_up_table$n_trials[i]
    } else {
      nini <- nini + prl_look_up_table$n_trials[i-1]
      nend <- nend + prl_look_up_table$n_trials[i]
    }
    # print(c(nini, nend))
    if (prl_look_up_table$n_trials[i] == 320) {
      choice_list[[i]] <- dd$choice[nini:nend]
    } else {
      choice_list[[i]] <- c(dd$choice[nini:nend], rep(0, 160))
    }
  }
  # Once created the list containing a vector of 320 elements for each subject,
  # I convert this list to a vector.
  choice_vect <- unlist(choice_list)
  
  # The same procedure above is repeated to create a rewards matrix.
  reward_list <- vector(mode = "list", length = len)
  
  for (i in 1:len) {
    if (i == 1) {
      nini <- 1
      nend <- prl_look_up_table$n_trials[i]
    } else {
      nini <- nini + prl_look_up_table$n_trials[i-1]
      nend <- nend + prl_look_up_table$n_trials[i]
    }
    if (prl_look_up_table$n_trials[i] == 320) {
      reward_list[[i]] <- dd$reward[nini:nend]
    } else {
      reward_list[[i]] <- c(dd$reward[nini:nend], rep(0, 160))
    }
  }
  reward_vect <- unlist(reward_list)
  
  # Generate data list for Stan.
  data_list <- list(
    
    n_subjects = length(unique(dd$subj_idx)),
    n_trials = as.numeric(table(dd$subj_idx)), 
    
    choice = matrix(
      choice_vect,
      nrow = length(unique(dd$subj_idx)), 
      ncol = 320, 
      byrow = TRUE
    ), 
    
    reward = matrix(
      reward_vect,
      nrow = length(unique(dd$subj_idx)), 
      ncol = 320, 
      byrow = TRUE
    )
  )
  
  data_list
}



#' @description 
#' Estimate and save Rescorla-Wagner parameters and subj_name in a CSV file.
#' @input
#' Data list to be used as input for Stan model.
#' @return 
#' NULL.
#' @save
#' File "params_individual_rescorla_wagner.csv".
save_rescorla_wagner_params_and_subj_code <- function(data_list) {
  
  library("tidyverse")
  library("rio")
  library("rstan")
  library("cmdstanr")
  set_cmdstan_path("/Users/corrado/cmdstan")
  library("posterior")
  library("bayesplot")
  color_scheme_set("brightblue")
  
  # Read Rescorla-Wagner code.
  file <- file.path(
    here("R", "stan_models", "rl_indv.stan")
  )
  
  # Read look-up table
  prl_look_up_table <- rio::import(
    here(
      "data", "processed", "prl", "data_for_rescorla_wagner", 
      "prl_look_up_table.csv"
    )
  )
  
  # Use CmdStan for fitting.
  mod <- cmdstan_model(file)
  
  fit <- mod$sample(
    data = data_list,
    iter_sampling = 3000,
    iter_warmup = 1000,
    seed = 123456,
    chains = 4,
    parallel_chains = 4,
    refresh = 500,
    thin = 1
  )
  
  # Check the results.
  fit$cmdstan_diagnose()
  
  if(0) fit$summary(c("alpha_neg", "alpha_pos", "tau")) %>% 
    as.data.frame()
  
  # Save parameter estimages in out.
  out <- fit$summary(c("alpha_neg", "alpha_pos", "tau"))
  
  used_subjects <- data_list$n_subjects
  
  # Rearrange in columns.
  alpha_pos <- out[1:used_subjects, 2]
  alpha_neg <- out[(used_subjects + 1):(2*used_subjects), 2]
  tau <- out[(2*used_subjects +1):(3*used_subjects), 2]
  
  params <- data.frame(
    id = 1:used_subjects,
    alpha_pos = alpha_pos,
    alpha_neg = alpha_neg,
    tau = tau
  )
  # Add columns names.
  names(params) <- c("subj_idx", "alpha_pos", "alpha_neg", "tau")
  
  # Select appropriate subset of look-up table.
  temp <- prl_look_up_table[1:used_subjects, ]
  
  # Add subjects codes to parameters estimates
  param_df <- left_join(params, temp, by = "subj_idx")
  param_df %>% 
    as.data.frame()
  
  # subj_name of subj_idx == 268 is missing. Add NA to the empty cell.
  param_df[268, 5] <- NA
  
  # Save params and subj_idx in a CSV file
  rio::export(
    param_df, 
    here(
      "data", "processed", "prl", "data_for_rescorla_wagner", 
      "params_individual_rescorla_wagner.csv"
      )
    )
}



# Find subj_name of controls with bad R-W parameters ----------------------


#' @description 
#' Find control participants with very low alpha positive (R-W).
#' @input
#' NULL
#' @return 
#' Vector of subj_name elements.
find_subj_code_bad_alpha_pos_RW <- function() {
  
  # Get individual parameters of the Rescorla-Wagner model
  param_df <- rio::import(
    here(
      "data", "processed", "prl", "data_for_rescorla_wagner", 
      "params_individual_rescorla_wagner.csv"
    )
  )
  
  param_df$is_patient_fct <- factor(param_df$is_patient)
  
  if(0) {
    param_df %>%
      ggplot(
        aes(x = alpha_pos, colour = is_patient_fct, fill = is_patient_fct)
      ) +
      geom_histogram()
  }
  
  bad_alpha_pos_df <- param_df[
    (param_df$alpha_pos < 0.25) & param_df$is_patient == 0, 
  ]
  
  bad_alpha_pos_df$subj_name
  
}


#' @description 
#' Find control participants with very low alpha negative (R-W).
#' @input
#' NULL
#' @return 
#' Vector of subj_name elements.
find_subj_code_bad_alpha_neg_RW <- function() {
  
  # Get individual parameters of the Rescorla-Wagner model
  param_df <- rio::import(
    here(
      "data", "processed", "prl", "data_for_rescorla_wagner", 
      "params_individual_rescorla_wagner.csv"
    )
  )
  
  param_df$is_patient_fct <- factor(param_df$is_patient)
  
  if(0) {
    param_df %>%
      ggplot(
        aes(x = alpha_neg, colour = is_patient_fct, fill = is_patient_fct)
      ) +
      geom_histogram()
  }
  
  bad_alpha_neg_df <- param_df[
    (param_df$alpha_neg < 0.15) & param_df$is_patient == 0, 
  ]
  
  bad_alpha_neg_df$subj_name
  
}



#' @description 
#' Find control participants with very low tau (R-W).
#' @input
#' NULL
#' @return 
#' Vector of subj_name elements.
find_subj_code_bad_tau_RW <- function() {
  
  # Get individual parameters of the Rescorla-Wagner model
  param_df <- rio::import(
    here(
      "data", "processed", "prl", "data_for_rescorla_wagner", 
      "params_individual_rescorla_wagner.csv"
    )
  )
  
  param_df$is_patient_fct <- factor(param_df$is_patient)
  
  if(0) {
    param_df %>%
      ggplot(
        aes(x = tau, colour = is_patient_fct, fill = is_patient_fct)
      ) +
      geom_histogram()
  }
  
  bad_tau_df <- param_df[
    (param_df$tau < 0.3) & param_df$is_patient == 0, 
  ]
  
  bad_tau_df$subj_name
  
}

