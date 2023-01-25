# Script name: funs_quest.R
# Project: Eating disorders Montecatini
# Script purpose: Functions for recoding questionnaires
# @author: Corrado Caudek <corrado.caudek@unifi.it>
# Date Created: Thu Oct 21 08:19:00 2021
# Last Modified Date: Sun May 29 22:58:28 2022
#
# Notes:
# The final Excel files for the patients have been created on 
# Fri May 27 14:08:33 2022.



# gen_complete_raw_quest_data() -------------------------------------------

#' @description
#' Read multiple Excel files with the questionnaire data of both patients and  
#' controls and merge them into a single file.
#' @input
#' Excel files.
#' @return
#' NULL.
#' @save
#' File "complete_raw_quest_pat_and_cont.rds".
gen_complete_raw_quest_data <- function() {
  
  # patients
  p1 <- readxl::read_excel(
    here(
      "data", "raw", "quest", "patients",
      "Questionari EDs-pop clinica (Risposte).xlsx"
    )
  )
  
  p2 <- readxl::read_excel(
    here(
      "data", "raw", "quest", "patients",
      "EDs-pop clinica_ADOLESCENTI (Risposte).xlsx"
    )
  )
  
  colnames(p2) <- colnames(p1)
  
  # all patients questionnaires data
  p <- rbind(p1, p2)
  p[, 1:2] <- NULL
  names(p)[1] <- paste("subj_name")
  p$is_patient <- 1
  
  # controls psicometria
  c1 <- readxl::read_excel(
    here(
      "data", "raw", "quest", "controls",
      "Questionari EDs-psicometria  (Risposte).xlsx"
    )
  )
  c1[, 1:2] <- NULL
  names(c1)[1] <- paste("subj_name")
  
  # external control sample (not psicometria) without EAT-26
  c2 <- readxl::read_excel(
    here(
      "data", "raw", "quest", "controls",
      "Questionari EDs 2020_2021 (Risposte).xlsx"
    )
  )
  names(c2)[3] <- paste("subj_name")
  c2[, 1:2] <- NULL
  
  c2x_eat26 <- readxl::read_excel(
    here(
      "data", "raw", "quest", "controls",
      "EAT-26 2020_2021 (Risposte).xlsx"
    )
  )
  c2_eat26 <- c2x_eat26[, c(2, 7:37)]
  names(c2_eat26)[1] <- paste("subj_name")
  
  c2all <- inner_join(c2, c2_eat26, by = "subj_name")
  
  colnames(c2all) <- colnames(c1)
  c <- rbind(c1, c2all)
  c$is_patient <- 0
  
  colnames(c) <- colnames(p)
  
  quest_pat_and_cont <- rbind(c, p)
  
  saveRDS(
    quest_pat_and_cont,
    here(
      "data", "processed", "quest",
      "complete_raw_quest_pat_and_cont.rds"
    )
  )
}


# correct_subj_identifiers_in_quest_data() --------------------------------

#' @description
#' Correct subjects identifiers in questionnaires data.
#' @input
#' reads from stored RDS file.
#' @return
#' data.frame.
correct_subj_identifiers_in_quest_data <- function() {
  
  quest_raw <- readRDS(
    here(
      "data", "processed", "quest",
      "complete_raw_quest_pat_and_cont.rds"
    )
  )
  
  # Correct subjects identifiers.
  quest_raw$subj_name <- forcats::fct_recode(
    quest_raw$subj_name,
    "em_or_2003_01_02_101_f" = "em_or_2003_02_01_101_f",
    "gr_bo_1996_07_31_547_f" = "grbo1996_07_31_547_f",
    "ca_pa_2002_04_05_939_f" = "ca_pa_2002_04_05_939",
    "an_de_1998_11_10_289_f" = "a_dc_98",
    "ga_gi_2003_02_09_229_f" = "ga_gi_2003_02_09_g",
    "lu_mu_1997_03_18_059_f" =  "lu_mu_1997_03_059_f",
    "am_mo_1996_05_22_339_f" = "an_mo_1996_05_22_339_f",
    "ar_cr_1996_08_06_738_f" = "ar_cr_1996_08_06_738_f",
    "ro_pi_2000_09_30_736_f" = "ro_pi_2000_09_30_791_f",
    "so_ia_2000_03_21_569_f" = "so_ch_2000_03_21_569_f",
    "do_za_2002_03_14_283_f" = "do_za_2002_03_02_283_f",
    "em_sa_2001_09_21_707_f" = "em_sa_2001_09_21_346_f",
    "lu_le_2001_07_04_283_f" = "lu_le_2001_07_01_283_f",
    "ch_ma_2001_10_27_332_f" = "ch_ma__2001_10_27_332_f",
    "ch_pi_2004_02_25_126.f" = "ch.pi.2004.02.25.126.f",
    "to_zo_2001_01_30_089_m" = "to_zo_2001_01_30_089_m_",
    "gi_se_1999_07_09_402_f" = "gio_se_1999_07_09_402_f",
    "ar_co_1996_12_27_348_f" = "ar_co_1996_12_27_348_f_",
    "vi_bi_1996_01_17_478_f" = "Vi_bi_1996_01_17_478_f",
    "ma_te_2001_05_31_333_m" = "ma_a_te_2001_05_31_333_m",
    "cl_co_1996_03_16_886_f" = "cl_co_1996_03_16_f",
    "fe_ma_1998_06_29_257_f" = "fe_ ma_1998_06_29_257_f"
  )
  
  # Correct group attribution: 6 subj_code in patients group are NOT patients!
  not_patients <- c(
    "is_ie_1986_08_18_291_f",
    "la_al_1996_06_14_190_f",
    "gr_ma_1995_09_05_060_f",
    "fr_ma_1996_02_16_959_f",
    "st_sa_1996_06_05_556_f",
    "al_pe_1996_09_02_886_m"
  )
  
  quest_raw$is_patient <- ifelse(
    quest_raw$subj_name %in% not_patients, 0, quest_raw$is_patient
  )
  
  # Re-write file with complete questionnaires raw data.
  saveRDS(
    quest_raw, 
    here(
      "data", "processed", "quest",
      "complete_raw_quest_pat_and_cont.rds"
    )
  )
  
}


# clean_quest_data() ------------------------------------------------------

#' @description
#' (1) Change columns names. 
#' (2) Remove rows of subjects who have redone the questionnaires multiple times. 
#' (3) Cleans subj_code vector. 
#' (4) Remove participants with wrong code.
#' @return
#' NULL.
#' @save
#' complete_raw_quest_pat_and_cont.rds
clean_quest_data <- function() {
  
  # Read questionnaire data.
  quest_raw <- readRDS(
    here("data", "processed", "quest", "complete_raw_quest_pat_and_cont.rds")
  )

  # 1. Change columns names ----
  
  # New columns names.
  temp_names <- c(
    "subj_code",
    "age", "gender", "marital_status", "job", "weight", "height",
    "highest_weight", "when_highest_weight", "how_long_highest_weight",
    "lowest_weight", "when_lowest_weight", "how_long_lowest_weight",
    "typical_weight", "when_typical_weight", "want_go_back_to_desidred_weight",
    "desired_weight", "when_desidered_weight", "max_weigth_loss",
    "is_weight_loss_on_purpose", "weight_after_dieting",
    "when_weight_after_dieting", "possible_uncontrolled_weight",
    "wanted_weight", "when_weight_problems", "father_job",
    "mother_job"
  )
  
  # Rename columns.
  temp_colnames <- colnames(quest_raw)
  temp_colnames[1:27] <- temp_names
  colnames(quest_raw) <- temp_colnames
  # colnames(quest_raw)[1:35]
  
  # 2. Remove rows ----
  
  # Remove rows of subjects who have redone the questionnaires multiple times.
  temp <- distinct(quest_raw, subj_code, .keep_all = TRUE)
  
  # 3. Clean subj_code vector ----

  # The resulting strings are unique and consist only of 
  # the _ character, numbers, and letters. 
  temp$subj_code <- janitor::make_clean_names(temp$subj_code)
  
  # 4. Remove participants with wrong code ----
  
  quest <- temp %>%
    dplyr::filter(!(subj_code %in% c("frida_2001", "la87_ac98")))
  
  # Replace RDS file with complete raw questionnaire data.
  saveRDS(
    quest,
    here("data", "processed", "quest", "complete_raw_quest_pat_and_cont.rds")
  )
  
}


# save_recoded_quest_data() -----------------------------------------------

#' @description
#' Scoring complete questionnaires data (patients and controls) and save them in
#' an RDS file.
#' @return
#' NULL.
#' @save
#' quest_data.rds
save_scored_quest_data <- function() {
  
  # Read complete raw 
  quest <- readRDS(
    here("data", "processed", "quest", "complete_raw_quest_pat_and_cont.rds")
  )
  
  # Scoring questionnaire data: Compute total score and/or subscales scores.
  
  # BSQ-14
  bsq14 <- recode_bsq14(quest)
  
  # Rosenberg self-esteem
  rosenberg <- recode_rosenberg(quest)
  
  # DASS-21
  dass21 <- recode_dass21(quest)
  
  # Social Interaction Anxiety Scale
  sias <- recode_sias(quest)
  
  # MPS
  mps <- recode_mps(quest)
  
  # Orthorexia Nervosa
  orto15 <- recode_orto15(quest)
  
  # EAT-26
  eat26 <- recode_eat26(quest)
  
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
    here::here("data", "processed", "quest", "quest_data.rds")
  )
  
}


# Recode BSQ-14 -----------------------------------------------------------

#' @description recode BSQ-14
#' @return data.frame.
recode_bsq14 <- function(d) {
  
  # change columns names
  bsq14_all <- d[, c(28:41)]
  col_names <- sprintf("bsq_%s", seq(1:14))
  colnames(bsq14_all) <- col_names
  # Recode values from alphanumeric to numeric.
  
  bsq14_all <- bsq14_all %>%
    mutate_all(list(~ recode(.,
      "Mai"           = 1L,
      "Quasi mai"     = 2L,
      "Qualche volta" = 3L,
      "Spesso"        = 4L,
      "Quasi sempre"  = 5L,
      "Sempre"        = 6L
    )))
  
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
  
  # Compute total score.
  bsq14_all <- bsq14_all %>% 
    rowwise() %>%
    mutate(
      bsq14_tot = sum(c_across(bsq_1:bsq_14))
    )
  
  # Add subj_code.
  bsq14_all$subj_code = d$subj_code
  
  bsq14_all
}


# Recode Rosenberg Self-Esteem Scale --------------------------------------

#' @description scoring Rosenberg self-esteem scale.
#' A 10-item scale that measures global self-worth by measuring both positive
#' and negative feelings about the self.
#' The scale is believed to be uni-dimensional.
#' All items are answered using a 4-point Likert scale format ranging from
#' strongly agree to strongly disagree.
#' Scoring: Give “Strongly Disagree” 1 point, “Disagree” 2 points,
#' “Agree” 3 points, and “Strongly Agree” 4 points.
#' Items 2, 5, 6, 8, 9 are reverse scored.
#' Sum scores for all ten items.
#' Higher scores indicate higher self-esteem.
#' @param data.frame.
#' @return data.frame.
recode_rosenberg <- function(d) {

  # change columns names
  ros_items <- d[, c(42:51)]
  # the items are in a different order than in the original scale
  ros_correct_order <- ros_items[, c(7, 10, 2, 4, 5, 9, 1, 8, 3, 6)]
  # glimpse(rosenberg_correct_order)
  # change columns names
  col_names <- sprintf("ros_%s", seq(1:10))
  colnames(ros_correct_order) <- col_names

  rses <- ros_correct_order %>%
    mutate(
      across(
        c(ros_1, ros_3, ros_4, ros_7, ros_10),
        ~ recode(.,
          "Fortemente in disaccordo" = 1L,
          "In disaccordo"            = 2L,
          "D'accordo"                = 3L,
          "Fortemente d'accordo"     = 4L
        )
      )
    ) %>%
    mutate(
      across(
        c(ros_2, ros_5, ros_6, ros_8, ros_9),
        ~ recode(.,
          "Fortemente in disaccordo" = 4L,
          "In disaccordo"            = 3L,
          "D'accordo"                = 2L,
          "Fortemente d'accordo"     = 1L
        )
      )
    )

  # Compute total score.
  rses <- rses %>%
    rowwise() %>%
    mutate(
      ros_tot = sum(c_across(ros_1:ros_10))
    )

  # Add subj_code.
  rses$subj_code <- d$subj_code

  rses
}


# Recode DASS-21 ----------------------------------------------------------

#' @description recode DASS-21
#' cut-offs: https://maic.qld.gov.au/wp-content/uploads/2016/07/DASS-21.pdf
#' @param data.frame.
#' @return data.frame.
recode_dass21 <- function(d) {
  
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
  
  # Stress
  dass21_all$dass21_s <- 
    with(
      dass21_all,
      dass_1 + dass_6 + dass_8 + dass_11 + dass_12 + dass_14 + dass_18
    )
  
  # Anxiety
  dass21_all$dass21_a <- 
    with(
      dass21_all,
      dass_2 + dass_4 + dass_7 + dass_9 + dass_15 + dass_19 + dass_20
    )
  
  # Depression
  dass21_all$dass21_d <- 
    with(
      dass21_all,
      dass_3 + dass_5 + dass_10 + dass_13 + dass_16 + dass_17 + dass_21
    )
  
  # Add subj_code.
  dass21_all$subj_code = d$subj_code
  
  dass21_all
}


# Recode Social Interaction Anxiety Scale (SIAS) --------------------------

#' @description recode Social Interaction Anxiety Scale (SIAS)
#' @param data.frame.
#' @return data.frame.
recode_sias <- function(d) {

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

  # Reverse items 8 and 10.
  sias_all$sias_8 <- misty::item.reverse(
    sias_all$sias_8,
    min = 0,
    max = 4
  )

  sias_all$sias_10 <- misty::item.reverse(
    sias_all$sias_10,
    min = 0,
    max = 4
  )

  # Compute total score.
  sias_all <- sias_all %>%
    rowwise() %>%
    mutate(
      sias_tot = sum(c_across(sias_1:sias_19))
    )

  # Add subj_code.
  sias_all$subj_code <- d$subj_code

  sias_all
}


# Recode MPS --------------------------------------------------------------

#' @description recode MPS
#' @param data.frame.
#' @return data.frame.
#' The four subscales are:
#' Concern over mistakes and doubts about actions
#' (Questions 9,10,13,14, 17,18,21,23,25,28,32,33,34)
#' Excessive concern with parents’ expectations and evaluation
#' (Questions 1,3,5,11,15,20,22,26,35)
#' Excessively high personal standards
#' (Questions 4,6,12,16,19,24,30)
#' Concern with precision, order and organisation
#' (Questions, 2,7,8,27,29,31)
#' Frost, R. O., & Marten, P. A. (1990). Perfectionism and evaluative threat.
#' Cognitive Therapy and Research, 14, 559-572.

recode_mps <- function(d) {
  
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

  # CMD
  mps_all$mps_cmd <- with(
    mps_all,
    mps_9 + mps_10 + mps_13 + mps_14 + mps_17 + mps_18 + mps_21 +
    mps_23 + mps_25 + mps_28 + mps_32 + mps_33 + mps_34
  )

  # PS
  mps_all$mps_ps <- with(
    mps_all,
    mps_4 + mps_6 + mps_12 + mps_16 + mps_19 + mps_24 + mps_30
  )

  # PEPC
  mps_all$mps_pepc <- with(
    mps_all,
    mps_1 + mps_3 + mps_5 + mps_11 + mps_15 + mps_20 + mps_22 + mps_26 +
    mps_35
  )
  
  # OR
  mps_all$mps_or <- with(
    mps_all,
    mps_2 + mps_7 + mps_8 + mps_27 + mps_29 + mps_31
  )
  
  mps_all$mps_tot <- with(
    mps_all,
    mps_cmd + mps_ps + mps_pepc + mps_or
  )
  
  # Add subj_code.
  mps_all$subj_code = d$subj_code
  
  mps_all
}


# Score ORTO-15 -----------------------------------------------------------

#' @description recode ORTO-15 items. ORTO-15 is based on a combination of the
#' Minnesota Multiphasic Inventory and the Bratman test and aims at measuring
#' the interrelationship between
#' cognitive-rational (items 1, 5, 6, 11, 12, 14),
#' clinical (items 3,7, 8, 9, 15) and
#' emotional (items 2, 4, 10, 13)
#' aspects of eating behavior.
#' @param data.frame.
#' @return data.frame.
recode_orto15 <- function(d) {
  
  # change columns names
  orto15_all <- d[, c(127:141)]
  col_names <- sprintf("orto_%s", seq(1:15))
  colnames(orto15_all) <- col_names

  orto15_all <- orto15_all %>%
  mutate(across(
    c(
      orto_3, orto_4, orto_6, orto_7, orto_10, orto_11, orto_12, orto_14,
      orto_15
    ),
    ~ recode(.,
      "Sempre" = 1L,
      "Spesso" = 2L,
      "Qualche volta" = 3L,
      "Mai" = 4L
    )
  )) %>%
  mutate(across(
    c(orto_2, orto_5, orto_8, orto_9),
    ~ recode(.,
      "Sempre" = 4L,
      "Spesso" = 3L,
      "Qualche volta" = 2L,
      "Mai" = 1L
    )
  )) %>%
  mutate(across(
    c(orto_1, orto_13),
    ~ recode(.,
      "Sempre" = 2L,
      "Spesso" = 4L,
      "Qualche volta" = 3L,
      "Mai" = 1L
    )
  ))
  
  # Compute total score.
  orto15_all <- orto15_all %>%
    rowwise() %>%
    mutate(
      orto15_tot = sum(c_across(orto_1:orto_15))
    )
  
  # Add subj_code.
  orto15_all$subj_code = d$subj_code
  
  orto15_all
}



# Score EAT-26 ------------------------------------------------------------

#' @description recode EAT-26 items
#' @param data.frame.
#' @return data.frame.
#' # https://www.nyeatingdisorders.org/pdf/EAT-26IntpretScoring-Test-3-20-10.pdf
recode_eat26 <- function(d) {
  
  # change columns names
  eat26_all <- d[, c(147:172)]
  col_names <- sprintf("eat26_%s", seq(1:26))
  colnames(eat26_all) <- col_names
  
  item_from1_to25 <- eat26_all[, 1:25]
  item_26         <- eat26_all[, 26]
  
  # Recode values from alphanumeric to numeric.
  
  # Scoring for items 1:25.
  item_from1_to25 <- item_from1_to25 %>%
    mutate_all(
      ~ recode(
        .,
        "Sempre"        = 3L,
        "Molto spesso"  = 2L,
        "Spesso"        = 1L,
        "Qualche volta" = 0L,
        "Raramente"     = 0L,
        "Mai"           = 0L
      )
    )
  
  # Scoring for item 26.
  item_26 <- item_26 %>%
    mutate_all(
      ~ recode(
        .,
        "Sempre"        = 0L,
        "Molto spesso"  = 0L,
        "Spesso"        = 0L,
        "Qualche volta" = 1L,
        "Raramente"     = 2L,
        "Mai"           = 3L
      )
    )
  
  eat26_all <- bind_cols(item_from1_to25, item_26)
  
  # Compute total score.
  eat26_all <- eat26_all %>%
    rowwise() %>%
    mutate(
      eat26_tot = sum(c_across(eat26_1:eat26_26))
    )
  
  eat26_all$eat26_at_risk <- ifelse(eat26_all$eat26_tot > 19, 1, 0)
  
  # Pathological avoidance of fattening foods and shape preoccupations; individuals who
  # score high on this factor may be described as overestimators of their body size and
  # who are dissatisfied with their shape and desire to be smaller.
  eat26_all$dieting <- with(
    eat26_all,
    eat26_1 + eat26_6 + eat26_7 + eat26_10 + eat26_11 + eat26_12 + eat26_14,
    eat26_16 + eat26_17 + eat26_22 + eat26_23 + eat26_24 + eat26_26
  )
  
  # bulimia and food preoccupation: similar to the previous factor, but is positively
  # related to bulimia and heavier body weight. High scores on this factor may be
  # associated with poor outcome.
  eat26_all$bulimia <- with(
    eat26_all,
    eat26_3 + eat26_4 + eat26_9 + eat26_18 + eat26_21 + eat26_25
  )
  
  # factor largely comprised of items relecting self-control about food as well as
  # those who acknowledge social pressure to gain weight. High scores on this factor
  # are related to lower weight and absence of bulimia.
  eat26_all$oral_control <- with(
    eat26_all,
    eat26_2 + eat26_5 + eat26_8 + eat26_13 + eat26_15 + eat26_19 + eat26_20
  )
  
  # Add subj_code.
  eat26_all$subj_code = d$subj_code
  
  eat26_all
}




#' OLD VERSION -----------------------
#' #' @description recode EAT-26 items
#' #' @param data.frame.
#' #' @return data.frame.
#' recode_eat26 <- function(d) {
#'   eat26_all <- d[, c(147:172)]
#' 
#'   col_names <- sprintf("eat26_%s", seq(1:26))
#'   colnames(eat26_all) <- col_names
#'   eat26_all[] <- lapply(eat26_all, as.factor)
#' 
#'   # 49 subjects did not complete the EAT-26
#'   # eat26_all <- eat26_all[complete.cases(eat26_all), ]
#' 
#'   final_eat26 <- eat26_all %>%
#'     mutate_all(list(~ recode(.,
#'       "Sempre" = "3",
#'       "Molto spesso" = "2",
#'       "Spesso" = "1",
#'       "Qualche volta" = "0",
#'       "Raramente" = "0",
#'       "Mai" = "0",
#'       .default = NA_character_
#'     )))
#' 
#' 
#'   final_eat26 <- mutate_if(
#'     final_eat26, is.factor, ~ as.numeric(as.character(.x))
#'   )
#' 
#'   # final_eat26$eat26_25
#'   MAX <- 3
#'   reverse <- function(x, max) {
#'     abs(x - max)
#'   }
#' 
#'   # L’item 26 è invertito: mai=3, raramente=2, qualche volta=1.
#'   final_eat26$eat26_26 <- reverse(final_eat26$eat26_26, MAX)
#' 
#'   # scores 1-4, cut-off >19
#'   final_eat26$eat26_tot <- rowSums(final_eat26)
#' 
#'   final_eat26$eat26_at_risk <- ifelse(final_eat26$eat26_tot > 19, 1, 0)
#' 
#'   # pathological avoidance of fattening foods and shape preoccupations; individuals who
#'   # score high on this factor may be described as overestimators of their body size and
#'   # who are dissatisfied with their shape and desiere to be smaller
#'   final_eat26$dieting <- final_eat26$eat26_1 + final_eat26$eat26_6 +
#'     final_eat26$eat26_7 + final_eat26$eat26_10 + final_eat26$eat26_11 +
#'     final_eat26$eat26_12 + final_eat26$eat26_14 + final_eat26$eat26_16 +
#'     final_eat26$eat26_17 + final_eat26$eat26_22 + final_eat26$eat26_23 +
#'     final_eat26$eat26_24 + final_eat26$eat26_26
#' 
#'   # bulimia and food preoccupation: similar to the previous factor, but is positively
#'   # related to bulimia and heavier body weight. High scores on this factor may be
#'   # associated with poor outcome.
#'   final_eat26$bulimia <- final_eat26$eat26_3 + final_eat26$eat26_4 +
#'     final_eat26$eat26_9 + final_eat26$eat26_18 + final_eat26$eat26_21 +
#'     final_eat26$eat26_25
#'   
#'   
#'   
#'   final_eat26 <- final_eat26 %>%
#'     rowwise() %>%
#'     mutate(
#'       bulimia2 = sum(
#'         c(
#'           eat26_3, eat26_4, eat26_9, eat26_18, eat26_21, eat26_25
#'         )
#'       )
#'     )
#'   
#'   
#'   
#'   # factor largely comprised of items relecting self-control about food as well as
#'   # those who acknowledge social pressure to gain weight. High scores on this factor
#'   # are related to lower weight and absence of bulimia.
#'   final_eat26$oral_control <- final_eat26$eat26_2 + final_eat26$eat26_5 +
#'     final_eat26$eat26_8 + final_eat26$eat26_13 + final_eat26$eat26_15 +
#'     final_eat26$eat26_19 + final_eat26$eat26_20
#' 
#'   # identify rows with missing values on EAT-26
#'   # rows_with_NA <- which(is.na(d[, 147]))
#' 
#'   subj_id_at_risk <- final_eat26 %>%
#'     dplyr::filter(eat26_at_risk == 1)
#' 
#'   eat26 <- data.frame(
#'     # remove subjects who did not complete the EAT-26
#'     # subj_code = d[-rows_with_NA, ]$subj_code,
#'     subj_code = d$subj_code,
#'     eat26_tot = final_eat26$eat26_tot,
#'     dieting = final_eat26$dieting,
#'     bulimia = final_eat26$bulimia,
#'     bulimia2 = final_eat26$bulimia2,
#'     oral_control = final_eat26$oral_control,
#'     eat26_at_risk = final_eat26$eat26_at_risk,
#'     eat26_1 = final_eat26$eat26_1,
#'     eat26_2 = final_eat26$eat26_2,
#'     eat26_3 = final_eat26$eat26_3,
#'     eat26_4 = final_eat26$eat26_4,
#'     eat26_5 = final_eat26$eat26_5,
#'     eat26_6 = final_eat26$eat26_6,
#'     eat26_7 = final_eat26$eat26_7,
#'     eat26_8 = final_eat26$eat26_8,
#'     eat26_9 = final_eat26$eat26_9,
#'     eat26_10 = final_eat26$eat26_10,
#'     eat26_11 = final_eat26$eat26_11,
#'     eat26_12 = final_eat26$eat26_12,
#'     eat26_13 = final_eat26$eat26_13,
#'     eat26_14 = final_eat26$eat26_14,
#'     eat26_15 = final_eat26$eat26_15,
#'     eat26_16 = final_eat26$eat26_16,
#'     eat26_17 = final_eat26$eat26_17,
#'     eat26_18 = final_eat26$eat26_18,
#'     eat26_19 = final_eat26$eat26_19,
#'     eat26_20 = final_eat26$eat26_20,
#'     eat26_21 = final_eat26$eat26_21,
#'     eat26_22 = final_eat26$eat26_22,
#'     eat26_23 = final_eat26$eat26_23,
#'     eat26_24 = final_eat26$eat26_24,
#'     eat26_25 = final_eat26$eat26_25,
#'     eat26_26 = final_eat26$eat26_26
#'   )
#'   eat26
#' }


# get_subj_info() ---------------------------------------------------------


#' @description 
#' get subject information
#' @param NONE
#' @return data.frame. 
get_subj_info <- function(d) {
  
  temp <- d[, 1:27]
  
  names_demogr <- c(
    "code", "age", "sex", "is_married", "occupation", "present_weight", "height", 
    "highest_weight",
    "when_highest_weight", "how_long_highest_weight",
    "lowest_weight", "when_lowest_weight", "how_long_lowest_weight",
    "typical_weight", "when_typical_weight", "is_going_back_weight",
    "going_back_weight", "when_going_back_weight", "biggest_weight_loss",
    "is_weight_loss_on_purpose", "dieting_weight", "when_dieting_weight", 
    "predicted_weight", "desired_weight", "when_weight_problems",
    "father_job", "mother_job"
  )
  
  names(temp) <- names_demogr
  
  # present_weight
  temp$present_weight <- 
    readr::parse_number(temp$present_weight)
  
  temp$present_weight <- ifelse(temp$present_weight == 4870.00, 48.70, 
                                temp$present_weight)
  
  temp$present_weight <- ifelse(temp$present_weight == 404.00, 40.40, 
                                temp$present_weight)
  
  # height
  temp$height <- 
    readr::parse_number(temp$height)
  
  temp$height <- ifelse(
    temp$height < 2, temp$height*100, temp$height
  )
  
  temp$height <- ifelse(
    temp$height == 1565.0, 156.5, temp$height
  )
  
  # highest_weight
  temp$highest_weight <- 
    readr::parse_number(temp$highest_weight)
  
  temp$highest_weight <- ifelse(
    temp$highest_weight == 565.00, 56.5, temp$highest_weight
  )
  temp$highest_weight <- ifelse(
    temp$highest_weight == 563.00, 56.3, temp$highest_weight
  )
  
  # desired weight
  temp$desired_weight <- 
    readr::parse_number(temp$desired_weight)
  temp$desired_weight <- ifelse(
    temp$desired_weight == 415.0, 41.5, temp$desired_weight
  )
  
  # Imputation.
  set.seed(123)
  miceObj <- miceRanger::miceRanger(
    temp
    , m = 1
    , returnModels = TRUE
    , verbose = FALSE
  )
  
  dataList <- miceRanger::completeData(miceObj)
  temp2 <- dataList[[1]]
  
  # weight suppression
  temp2$ws <- temp2$highest_weight - temp2$present_weight 
  
  # relative weight suppression
  temp2$rws <- temp2$ws / temp2$highest_weight
  
  temp2$bmi <- temp2$present_weight / (temp2$height / 100)^2
  
  temp2 <- temp2 %>% 
    dplyr::rename(subj_code = code)
  
  temp2
}


# add_diagn_cat_to_quest() ------------------------------------------------

add_diagn_cat_to_quest <- function(all_quest_data, diagn_cat) {
  
  dd <- left_join(all_quest_data, diagn_cat, by = "subj_code")
  
  # table(dd$is_patient)

  # Here I attribute all the 7 new patients to the AN category. This must be 
  # CHANGED after examining the results of the PRL task!!!
  dd$diagnosis <- ifelse(
    ((dd$is_patient == 1) & (is.na(dd$diagnosis))), "AN", dd$diagnosis
  )
  
  dd$diagnosis <- ifelse(
    (dd$is_patient == 0), "HC", dd$diagnosis
  )
  
  dd$diagnosis <- ifelse(
    ((dd$diagnosis == "HC") & (dd$eat26_at_risk == 1)), "RI", dd$diagnosis
  )
  
  dd$diagnosis <- factor(dd$diagnosis)
  # table(dd$diagnosis)
  
  dd$is_recovered <- ifelse(
    ((dd$is_patient == 1) & (is.na(dd$is_recovered))), "no", dd$is_recovered
  )
  
  dd <- dd %>% 
    mutate(
      diag_cat = case_when(
        is_patient == 1 & diagnosis == "AN" & is_recovered == "no" ~ "AN",
        is_patient == 1 & diagnosis == "AN" & is_recovered == "si" ~ "AN_R",
        is_patient == 1 & diagnosis == "BN" & is_recovered == "no" ~ "BN",
        is_patient == 1 & diagnosis == "BN" & is_recovered == "si" ~ "BN_R",
        is_patient == 0 & diagnosis == "HC"                        ~ "HC",
        is_patient == 0 & diagnosis == "RI"                        ~ "RI"
      )
    )
  # table(dd$diag_cat)
  
  dd
  
}


