# Script name: funs_quest_input_cr.R
# Project: Eating disorders Montecatini
# Script purpose: Functions for finding input for careless responding 
# @author: Corrado Caudek <corrado.caudek@unifi.it>
# Date Created: Tue May 31 12:13:03 2022
# Last Modified Date: Tue May 31 12:13:03 2022
#
# Notes:




# save_recoded_quest_data() -----------------------------------------------

save_input_careless_resp <- function() {
  
  # Read complete raw 
  quest <- readRDS(
    here("data", "processed", "quest", "complete_raw_quest_pat_and_cont.rds")
  )
  
  # BSQ-14
  bsq14 <- bsq14_careless_resp(quest)
  
  # Rosenberg self-esteem
  rosenberg <- rosenberg_careless_resp(quest)
  
  # DASS-21
  dass21 <- dass21_careless_resp(quest)
  
  # Social Interaction Anxiety Scale
  sias <- sias_careless_resp(quest)
  
  # MPS
  mps <- mps_careless_resp(quest)
  
  # Orthorexia Nervosa
  orto15 <- orto15_careless_resp(quest)
  
  # EAT-26
  eat26 <- eat26_careless_resp(quest)
  
  # Combine questionnaires data.
  d1 <- full_join(bsq14, rosenberg, by = "subj_code")
  d2 <- full_join(d1, dass21, by = "subj_code")
  d3 <- full_join(d2, sias, by = "subj_code")
  d4 <- full_join(d3, mps, by = "subj_code")
  d5 <- full_join(d4, orto15, by = "subj_code")
  quest_data <- full_join(d5, eat26, by = "subj_code")
  
  # Add is_patient column.
  quest_data$is_patient <- quest$is_patient
  
  saveRDS(
    quest_data,
    here::here("data", "processed", "quest", "input_careless_resp.rds")
  )
  
}


# BSQ-14 -----------------------------------------------------------

bsq14_careless_resp <- function(d) {
  
  # change columns names
  bsq14_all <- d[, c(28:41)]
  col_names <- sprintf("bsq_%s", seq(1:14))
  colnames(bsq14_all) <- col_names
  
  # bsq14_all <- bsq14_all %>%
  #   mutate_all(list(rec = ~ recode(.,
  #     "Mai"           = 1L,
  #     "Quasi mai"     = 2L,
  #     "Qualche volta" = 3L,
  #     "Spesso"        = 4L,
  #     "Quasi sempre"  = 5L,
  #     "Sempre"        = 6L
  #   )))
  
  # Recode values from alphanumeric to numeric.
  bsq14_all <- bsq14_all %>%
    mutate_all(
      ~ recode(
        .,
        "Mai"           = 1L,
        "Quasi mai"     = 2L,
        "Qualche volta" = 3L,
        "Spesso"        = 4L,
        "Quasi sempre"  = 5L,
        "Sempre"        = 6L
      )
    )
  
  # Add subj_code.
  bsq14_all$subj_code = d$subj_code
  
  bsq14_all
}


# Rosenberg Self-Esteem Scale --------------------------------------

rosenberg_careless_resp <- function(d) {

  # change columns names
  ros_all <- d[, c(42:51)]
  col_names <- sprintf("ros_%s", seq(1:10))
  colnames(ros_all) <- col_names
  
  ros_all <- ros_all %>%
    mutate_all(
      ~ recode(
        .,
        "Fortemente in disaccordo" = 1L,
        "In disaccordo"            = 2L,
        "D'accordo"                = 3L,
        "Fortemente d'accordo"     = 4L
      )
    )

  # Add subj_code.
  ros_all$subj_code <- d$subj_code

  ros_all
}


# DASS-21 ----------------------------------------------------------

dass21_careless_resp <- function(d) {
  
  # change columns names
  dass21_all <- d[, c(52:72)]
  col_names <- sprintf("dass_%s", seq(1:21))
  colnames(dass21_all) <- col_names
  
  # Recode values from alphanumeric to numeric.
  dass21_all <- dass21_all %>%
    mutate_all(
      ~ recode(
        .,
        "Non mi è mai accaduto"                 = 0L,
        "Mi è capitato qualche volta"           = 1L,
        "Mi è capitato con una certa frequenza" = 2L,
        "Mi è capitato quasi sempre"            = 3L
      )
    )
  
  # Add subj_code.
  dass21_all$subj_code = d$subj_code
  
  dass21_all
}


# Social Interaction Anxiety Scale (SIAS) --------------------------

sias_careless_resp <- function(d) {

  # change columns names
  sias_all <- d[, c(73:91)]
  col_names <- sprintf("sias_%s", seq(1:19))
  colnames(sias_all) <- col_names
  
  # Recode values from alphanumeric to numeric.
  sias_all <- sias_all %>%
    mutate_all(
      ~ recode(
        .,
        "Per nulla"  = 0L,
        "Poco"       = 1L,
        "Abbastanza" = 2L,
        "Molto"      = 3L,
        "Moltissimo" = 4L
      )
    )

  # Add subj_code.
  sias_all$subj_code <- d$subj_code

  sias_all
}


# MPS --------------------------------------------------------------

mps_careless_resp <- function(d) {
  
  # change columns names
  mps_all <- d[, c(92:126)]
  col_names <- sprintf("mps_%s", seq(1:35))
  colnames(mps_all) <- col_names
  
  # Recode values from alphanumeric to numeric.
  mps_all <- mps_all %>%
    mutate_all(
      ~ recode(
        .,
        "Forte disaccordo"           = 1L,
        "Disaccordo"                 = 2L,
        "Né d'accordo né disaccordo" = 3L,
        "D'accordo"                  = 4L,
        "Forte accordo"              = 5L
      )
    )

  # Add subj_code.
  mps_all$subj_code = d$subj_code
  
  mps_all
}


# ORTO-15 -----------------------------------------------------------

orto15_careless_resp <- function(d) {
  
  # change columns names
  orto15_all <- d[, c(127:141)]
  col_names <- sprintf("orto_%s", seq(1:15))
  colnames(orto15_all) <- col_names
  
  orto15_all <- orto15_all %>%
    mutate_all(
      ~ recode(
        .,
        "Sempre" = 1L,
        "Spesso" = 2L,
        "Qualche volta" = 3L,
        "Mai" = 4L
      )
    )
  
  # Add subj_code.
  orto15_all$subj_code = d$subj_code
  
  orto15_all
}



# EAT-26 ------------------------------------------------------------

eat26_careless_resp <- function(d) {
  
  # change columns names
  eat26_all <- d[, c(147:172)]
  col_names <- sprintf("eat26_%s", seq(1:26))
  colnames(eat26_all) <- col_names
  
  eat26_all <- eat26_all %>%
    mutate_all(
      ~ recode(
        .,
        "Sempre"        = 5L,
        "Molto spesso"  = 4L,
        "Spesso"        = 3L,
        "Qualche volta" = 2L,
        "Raramente"     = 1L,
        "Mai"           = 0L
      )
    )
  
  # Add subj_code.
  eat26_all$subj_code = d$subj_code
  
  eat26_all
}



