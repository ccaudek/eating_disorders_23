# Script name: ed_careless_resp_fnc.R
# Project: Eating disorders, Montecatini
# Script purpose: Functions for computing the careless responding indices. 
# @author: Corrado Caudek <corrado.caudek@unifi.it>
# Date Created: Fri Oct 22 09:45:44 2021
# Last Modified Date: Fri Oct 22 09:45:44 2021
# 
# Notes: 


find_subj_code_bad_longstring <- function() {
  
  # Read complete raw data.
  quest_raw <- readRDS(
    here::here("data", "processed", "quest", "quest_data.rds")
  )
  
  # BSQ-14
  bsq_data <- quest_raw %>% 
    dplyr::select(num_range("bsq_", 1:14))
  
  # Rosenberg
  ros_data <- quest_raw %>% 
    dplyr::select(num_range("ros_", 1:10))
  
  # DASS-21
  dass_data <- quest_raw %>% 
    dplyr::select(num_range("dass_", 1:21))
  
  # SIAS
  sias_data <- quest_raw %>% 
    dplyr::select(num_range("sias_", 1:19))
  
  # MPS - Multidimensional Perfectionism Scale (MPS; Frost, Marten, Lahart e Rosenblate, 1990).
  mps_data <- quest_raw %>% 
    dplyr::select(num_range("mps_", 1:35))
  
  # ORTO
  orto_data <- quest_raw %>% 
    dplyr::select(num_range("orto_", 1:15))
  
  # EAT-26
  eat26_data <- quest_raw %>% 
    dplyr::select(num_range("eat26_", 1:26))
  
  # Select only numerical values for the questionnaires data.
  qdata <- bind_cols(
    bsq_data, ros_data, dass_data, sias_data, mps_data, orto_data, eat26_data
  ) 
  
  # list of data.frames
  qlist <- list(
    bsq_data, ros_data, dass_data, sias_data, mps_data, orto_data, eat26_data
  ) 
  
  # apply careless::longstring() function to the data.frame of each of the 
  # seven questionnaires
  out <- purrr::map(qlist, careless::longstring)
  
  # convert list to data.frame and transpose
  df <- do.call(rbind.data.frame, out) %>% 
    t() %>% 
    as.data.frame()
  # remove row names (they are not meaningful here)
  rownames(df) <- NULL
  # provide columns' names
  names_longstring_quest <- c(
    "bsq_longstring", "ros_longstring", "dass_longstring", "sias_longstring", 
    "mps_longstring", "orto_longstring", "eat26_longstring"
  )
  names(df) <- names_longstring_quest
  
  df$subj_code <- quest_raw$subj_code
  
  longstring_df <- df
  
  out <- boxplot(df[, 6])
  foo <- data.frame(
    subj_code = quest_raw$subj_code,
    y1 = df[, 3]
  ) 
  
  foo1 <- foo[, c(1, 2)] %>% 
    dplyr::filter(y1 > out$stats[5])
  
  bad_ids_longstring <- foo1$subj_code
  
  bad_ids_longstring
}




find_subj_code_bad_avgstring <- function() {
  
  # Read complete raw data.
  quest_raw <- readRDS(
    here::here("data", "processed", "quest", "quest_data.rds")
  )
  
  # BSQ-14
  bsq_data <- quest_raw %>% 
    dplyr::select(num_range("bsq_", 1:14))
  
  # Rosenberg
  ros_data <- quest_raw %>% 
    dplyr::select(num_range("ros_", 1:10))
  
  # DASS-21
  dass_data <- quest_raw %>% 
    dplyr::select(num_range("dass_", 1:21))
  
  # SIAS
  sias_data <- quest_raw %>% 
    dplyr::select(num_range("sias_", 1:19))
  
  # MPS - Multidimensional Perfectionism Scale (MPS; Frost, Marten, Lahart e Rosenblate, 1990).
  mps_data <- quest_raw %>% 
    dplyr::select(num_range("mps_", 1:35))
  
  # ORTO
  orto_data <- quest_raw %>% 
    dplyr::select(num_range("orto_", 1:15))
  
  # EAT-26
  eat26_data <- quest_raw %>% 
    dplyr::select(num_range("eat26_", 1:26))
  
  # Select only numerical values for the questionnaires data.
  qdata <- bind_cols(
    bsq_data, ros_data, dass_data, sias_data, mps_data, orto_data, eat26_data
  ) 
  
  # list of data.frames
  qlist <- list(
    bsq_data, ros_data, dass_data, sias_data, mps_data, orto_data, eat26_data
  ) 
  
  # The average length of consecutive identical responses: Averagestring.
  
  out <- map(qlist, careless::longstring, avg = TRUE)
  # keep only the avgstr column for each list
  for(i in 1:7) {
    out[[i]]$longstr <- NULL
  }
  
  # convert list to data.frame, transpose
  df <- data.frame(matrix(unlist(out), nrow=length(out), byrow=TRUE)) %>% 
    t() %>% 
    as.data.frame()
  # remove row names (they are not meaningful here)
  rownames(df) <- NULL
  # provide columns' names
  names(df) <- c(
    "bsq_averagestring", "ros_averagestring", "dass_laveragestring", 
    "sias_averagestring", "mps_averagestring", "orto_averagestring", 
    "eat26_averagestring"
  )
  
  # Here we consider only the 3-6 questionnaires
  out <- boxplot(df[, 3])
  foo <- data.frame(
    subj_code = quest_raw$subj_code,
    y1 = df[, 3]
  ) 
  foo1 <- foo[, c(1, 2)] %>% 
    dplyr::filter(y1 > out$stats[5])
  bad_ids_averagestring_3 <- foo1$subj_code
  
  out <- boxplot(df[, 4])
  foo <- data.frame(
    subj_code = quest_raw$subj_code,
    y1 = df[, 4]
  ) 
  foo1 <- foo[, c(1, 2)] %>% 
    dplyr::filter(y1 > out$stats[5])
  bad_ids_averagestring_4 <- foo1$subj_code
  
  out <- boxplot(df[, 5])
  foo <- data.frame(
    subj_code = quest_raw$subj_code,
    y1 = df[, 5]
  ) 
  foo1 <- foo[, c(1, 2)] %>% 
    dplyr::filter(y1 > out$stats[5])
  bad_ids_averagestring_5 <- foo1$subj_code
  
  out <- boxplot(df[, 6])
  foo <- data.frame(
    subj_code = quest_raw$subj_code,
    y1 = df[, 6]
  ) 
  foo1 <- foo[, c(1, 2)] %>% 
    dplyr::filter(y1 > out$stats[5])
  bad_ids_averagestring_6 <- foo1$subj_code
  
  u1 <- union(bad_ids_averagestring_3, bad_ids_averagestring_4)
  u2 <- union(u1, bad_ids_averagestring_5)
  u3 <- union(u2, bad_ids_averagestring_6)
  bad_ids_averagestring <- u3
  
  bad_ids_averagestring
}



find_subj_code_bad_irv <- function() {
  
  # Read complete raw data.
  quest_raw <- readRDS(
    here::here("data", "processed", "quest", "quest_data.rds")
  )
  
  # BSQ-14
  bsq_data <- quest_raw %>% 
    dplyr::select(num_range("bsq_", 1:14))
  
  # Rosenberg
  ros_data <- quest_raw %>% 
    dplyr::select(num_range("ros_", 1:10))
  
  # DASS-21
  dass_data <- quest_raw %>% 
    dplyr::select(num_range("dass_", 1:21))
  
  # SIAS
  sias_data <- quest_raw %>% 
    dplyr::select(num_range("sias_", 1:19))
  
  # MPS - Multidimensional Perfectionism Scale (MPS; Frost, Marten, Lahart e Rosenblate, 1990).
  mps_data <- quest_raw %>% 
    dplyr::select(num_range("mps_", 1:35))
  
  # ORTO
  orto_data <- quest_raw %>% 
    dplyr::select(num_range("orto_", 1:15))
  
  # EAT-26
  eat26_data <- quest_raw %>% 
    dplyr::select(num_range("eat26_", 1:26))
  
  # Select only numerical values for the questionnaires data.
  qdata <- bind_cols(
    bsq_data, ros_data, dass_data, sias_data, mps_data, orto_data, eat26_data
  ) 
  
  # list of data.frames
  qlist <- list(
    bsq_data, ros_data, dass_data, sias_data, mps_data, orto_data, eat26_data
  ) 
  
  # The intra-individual response variability (IRV) is defined as the 
  # "standard deviation of responses across a set of consecutive item responses 
  # for an individual" (Dunn et al. 2018).
  
  out <- map(qlist, careless::irv)
  
  irv_df <- do.call(rbind.data.frame, out) %>% 
    t() %>% 
    as.data.frame()
  # remove row names (they are not meaningful here)
  rownames(irv_df) <- NULL
  # provide columns' names
  names(irv_df) <- c(
    "bsq_irv", "ros_irv", "dass_irv", "sias_irv", "mps_irv", "orto_irv", 
    "eat26_irv"
  )
  
  irv_df$irv_flag <- irv_df %>% 
    mutate(
      across(ends_with("irv"), ~ifelse(.x < 0.2, 1, 0))
    )
  
  d <- irv_df %>% 
    select(contains('flag'))
  
  sum_flag <- rowSums(d)
  
  d1 <- tibble(
    sum_flag = sum_flag,
    subj_code = quest_raw$subj_code
  )
  
  bad_ids <- d1[d1$sum_flag > 0, ]$subj_code
  
  patients_ids <- find_patients_codes()
  
  bad_ids[!(bad_ids %in% patients_ids)]
}



find_subj_code_bad_mahalanobis_dist <- function() {
  
  # Read complete raw data.
  quest_raw <- readRDS(
    here::here("data", "processed", "quest", "quest_data.rds")
  )
  
  # BSQ-14
  bsq_data <- quest_raw %>% 
    dplyr::select(num_range("bsq_", 1:14))
  
  # Rosenberg
  ros_data <- quest_raw %>% 
    dplyr::select(num_range("ros_", 1:10))
  
  # DASS-21
  dass_data <- quest_raw %>% 
    dplyr::select(num_range("dass_", 1:21))
  
  # SIAS
  sias_data <- quest_raw %>% 
    dplyr::select(num_range("sias_", 1:19))
  
  # MPS - Multidimensional Perfectionism Scale (MPS; Frost, Marten, Lahart e Rosenblate, 1990).
  mps_data <- quest_raw %>% 
    dplyr::select(num_range("mps_", 1:35))
  
  # ORTO
  orto_data <- quest_raw %>% 
    dplyr::select(num_range("orto_", 1:15))
  
  # EAT-26
  eat26_data <- quest_raw %>% 
    dplyr::select(num_range("eat26_", 1:26))
  
  # Select only numerical values for the questionnaires data.
  qdata <- bind_cols(
    bsq_data, ros_data, dass_data, sias_data, mps_data, orto_data, eat26_data
  ) 
  
  # list of data.frames
  qlist <- list(
    bsq_data, ros_data, dass_data, sias_data, mps_data, orto_data, eat26_data
  ) 
  
  out <- purrr::map(qlist, careless::mahad, plot = FALSE, flag = FALSE)
  
  # convert list to data.frame, transpose
  mahad_df <- do.call(rbind.data.frame, out) %>% 
    t() %>% 
    as.data.frame()
  # remove row names (they are not meaningful here)
  rownames(mahad_df) <- NULL
  # provide columns' names
  names(mahad_df) <- c(
    "bsq_mahad", "ros_mahad", "dass_mahad", "sias_mahad", 
    "mps_mahad", "orto_mahad", "eat26_mahad"
  )
  
  mahad_df$subj_code <- quest_raw$subj_code
  
  out <- boxplot(mahad_df[, 1])
  foo <- data.frame(
    subj_code = quest_raw$subj_code,
    y1 = mahad_df[, 1]
  ) 
  foo1 <- foo[, c(1, 2)] %>% 
    dplyr::filter(y1 > out$stats[5])
  bad_ids_maha1 <- foo1$subj_code
  
  out <- boxplot(mahad_df[, 2])
  foo <- data.frame(
    subj_code = quest_raw$subj_code,
    y1 = mahad_df[, 2]
  ) 
  foo1 <- foo[, c(1, 2)] %>% 
    dplyr::filter(y1 > out$stats[5])
  bad_ids_maha2 <- foo1$subj_code
  
  out <- boxplot(mahad_df[, 3])
  foo <- data.frame(
    subj_code = quest_raw$subj_code,
    y1 = mahad_df[, 3]
  ) 
  foo1 <- foo[, c(1, 2)] %>% 
    dplyr::filter(y1 > out$stats[5])
  bad_ids_maha3 <- foo1$subj_code
  
  out <- boxplot(mahad_df[, 4])
  foo <- data.frame(
    subj_code = quest_raw$subj_code,
    y1 = mahad_df[, 4]
  ) 
  foo1 <- foo[, c(1, 2)] %>% 
    dplyr::filter(y1 > out$stats[5])
  bad_ids_maha4 <- foo1$subj_code
  
  out <- boxplot(mahad_df[, 5])
  foo <- data.frame(
    subj_code = quest_raw$subj_code,
    y1 = mahad_df[, 5]
  ) 
  foo1 <- foo[, c(1, 2)] %>% 
    dplyr::filter(y1 > out$stats[5])
  bad_ids_maha5 <- foo1$subj_code
  
  out <- boxplot(mahad_df[, 6])
  foo <- data.frame(
    subj_code = quest_raw$subj_code,
    y1 = mahad_df[, 6]
  ) 
  foo1 <- foo[, c(1, 2)] %>% 
    dplyr::filter(y1 > out$stats[5])
  bad_ids_maha6 <- foo1$subj_code
  
  u2 <- union(bad_ids_maha1, bad_ids_maha2)
  u3 <- union(u2, bad_ids_maha3)
  u4 <- union(u3, bad_ids_maha4)
  u5 <- union(u4, bad_ids_maha5)
  u6 <- union(u5, bad_ids_maha6)
  bad_ids_maha <- u6
  
  patients_ids <- find_patients_codes()
  
  bad_ids_maha[!(bad_ids_maha %in% patients_ids)]
  
}







