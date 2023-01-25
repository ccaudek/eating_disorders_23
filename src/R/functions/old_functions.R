
#' @description
#' Get demographic information.
#' @input
#' Reads saved RDS file.
#' @return
#' Dataframe.
get_demo_info <- function() {
  d <- readRDS(
    here::here("data", "processed", "quest", "bmi_age_weight.rds")
  )
  
  d$age <- as.numeric(as.character(d$age))
  d$age <- ifelse(d$age < 18, 18, d$age)
  
  d$weight_c <- forcats::fct_recode(
    d$weight,
    "48.0" = "48 kg",
    "82.0" = "82 kg",
    "60.0" = "60 kg",
    "48.7" = "48,70 kg",
    "60.0" = "60kg",
    "61.0" = "61 kg",
    "48.0" = "48kg",
    "65.0" = "65 kg",
    "45.0" = "45 kg",
    "67.0" = "67 kg",
    "53.0" = "53 kg",
    "50.0" = "50 kg",
    "55.0" = "55kg",
    "95.0" = "95 kg",
    "63.0" = "63 kg",
    "56.0" = "56 Kg",
    "92.0" = "92kg",
    "80.0" = "80kg",
    "67.0" = "67kg",
    "54.0" = "54 kg",
    "55.0" = "55 kg",
    "68.0" = "68 kg",
    "55.0" = "55 kg",
    "NA" = "non lo so",
    "NA" = "non so",
    "40.4" = "40,4kg",
    "65.0" = "65kg",
    "44.0" = "44kg"
  )
  
  d$weight <- as.numeric(as.character(d$weight_c))
  
  d$height_c <- forcats::fct_recode(
    d$height,
    "163" = "1.63",
    "159" = "159 cm",
    "163" = "1.63",
    "163" = "163 cm",
    "174" = "174 cm",
    "184" = "184cm",
    "188" =  "1.88",
    "155" = "1.55",
    "150" = "1.5",
    "169" = "1,69m",
    "158" = "1.58",
    "170" = "1.70cm",
    "170" = "1.70",
    "166" = "1.66",
    "171" = "1.71",
    "160" = "160cm",
    "176" = "176 cm",
    "178" = "1.78 cm",
    "162" = "1.62",
    "170" = "1.7",
    "159" = "159 cm",
    "180" = "1.80",
    "158" = "1.58",
    "165" = "1.65",
    "158" = "1.58",
    "161" = "161 cm",
    "175" = "1.75",
    "160" = "1.60",
    "165" = "1.65",
    "163" = "1.63",
    "158" = "1.58",
    "170" = "1.7",
    "170" = "1.70",
    "171" = "1.71",
    "170" = "170cm",
    "165" = "1.65",
    "175" = "1.75",
    "156" = "1.56",
    "176" = "1.76",
    "160" = "1.60",
    "170" = "170cm",
    "154" = "1.54",
    "150" = "1.50",
    "155" = "155 cm",
    "150" = "1.50",
    "176" = "1.76",
    "153" = "1.53",
    "156" = "1.56",
    "170" = "170 cm",
    "155" = "1.55",
    "170" = "1.7",
    "157" = "157cm",
    "168" = "168 cm",
    "1.68" = "1.68",
    "1.69" = "1.69",
    "175.0" = "175 cm",
    "165.0" = "165 cm",
    "162.0" = "162 cm",
    "167.0" = "167 cm",
    "164.0" = "164 cm",
    "169.0" = "1.69",
    "168.0" = "1.68"
  )
  
  d$height <- as.numeric(as.character(d$height_c))
  
  foo <- data.frame(
    age = d$age,
    is_female <- ifelse(d$gender == "Femmina", 1, 0),
    weight = d$weight,
    height = d$height
  )
  
  imp <- mice::mice(foo, method = "norm.predict", m = 1)
  temp <- complete(imp)
  
  d$weight <- temp$weight
  
  d$bmi <- d$weight / (d$height / 100)^2
  # hist(d$bmi)
  # sort(d$subj_code)
  
  # Correct subj_code
  temp <- correct_subj_code_quest(d$subj_code)
  d$subj_code <- temp
  
  d <- d %>%
    mutate_if(is.character, as.factor)
  
  # Correct gender.
  d$gender <- forcats::fct_recode(
    d$gender,
    "Femmina" = "Altro"
  )
  
  # Change labels.
  d$gender <- forcats::fct_recode(
    d$gender,
    "Female" = "Femmina",
    "Male" = "Maschio"
  )
  # summary(d$gender)
  
  d$weight_c <- NULL
  d$height_c <- NULL
  
  # Read subj_code of anorexic patients.
  anorexics_subj_code <- readRDS(
    here("data", "processed", "prl", "anorexics_subj_codes.rds")
  )
  
  d$is_anorexic <- ifelse(
    d$subj_code %in% anorexics_subj_code, 1, 0
  )
  # table(d$is_anorexic, d$is_patient, d$gender)
  
  demo_df <- d %>%
    dplyr::select(
      subj_code, age, gender, bmi, is_patient, is_anorexic
    )
  
  # demo_df %>%
  #   group_by(is_patient, is_anorexic) %>%
  #   summarise(
  #     bmi = mean(bmi),
  #     age = mean(age)
  #   )
  
  # Forse escludere solo i maschi dal gruppo di controllo. Tra i pazienti,
  # l'unico maschio sembra anoressico.
  demo_df
}



#' @description
#' Correct subj_code for questionnaires.
#' @input
#' Vector of subj_code.
#' @return
#' Vector of subj_code.
correct_subj_code_quest <- function(old_subj_codes) {
  
  # Correct subj_code for quest data.
  new_subj_codes <- forcats::fct_recode(
    old_subj_codes,
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
    "cr_gi_1994_10_14_378_f" = "cr_gi_1994_10_14_347_f",
    "be_ma_1999_06_15_475_f" = "be_ma_1999_06_15_331_f",
    "em_or_2003_02_01_101_f" = "em_or_2003_01_02_101_f",
    "ro_pi_2000_09_30_791_f" = "ro_pi_2000_09_30_736_f",
    "li_va_2001_05_29_471_m" = "li_va_2001_05_29_471_f",
    "cr_gi_1994_10_14_378_f" = "cr_gi_1994_10_14_347_f",
    "ro_pi_2000_09_30_791_f" = "ro_pi_2000_09_30_736_f"
  )
  
  new_subj_codes
}




#' @description
#' Get BMI, age, weight (patients and controls)
#' @return
#' NULL.
#' @save
#' File quest_data.rds.
save_bmi_age_weight <- function() {
  
  # Read questionnaire data
  quest_raw <- readRDS(
    here("data", "processed", "quest", "complete_raw_quest_pat_and_cont.rds")
  )
  # colnames(quest_raw)[1:28]
  
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
  
  colnames(quest_raw)[1:27] <- temp_names
  
  # Correct subj_code.
  quest_raw$subj_code <-
    correct_subj_code_quest(quest_raw$subj_code)
  
  # quest_raw$subj_code <- fct_recode(
  #   quest_raw$subj_code,
  #   "em_or_2003_01_02_101_f" = "em_or_2003_02_01_101_f",
  #   "gr_bo_1996_07_31_547_f" = "grbo1996_07_31_547_f",
  #   "ca_pa_2002_04_05_939_f" = "ca_pa_2002_04_05_939",
  #   "an_de_1998_11_10_289_f" = "a_dc_98",
  #   "ga_gi_2003_02_09_229_f" = "ga_gi_2003_02_09_g",
  #   "lu_mu_1997_03_18_059_f" =  "lu_mu_1997_03_059_f",
  #   "am_mo_1996_05_22_339_f" = "an_mo_1996_05_22_339_f",
  #   "ar_cr_1996_08_06_738_f" = "ar_cr_1996_08_06_738_f",
  #   "ro_pi_2000_09_30_736_f" = "ro_pi_2000_09_30_791_f",
  #   "so_ia_2000_03_21_569_f" = "so_ch_2000_03_21_569_f",
  #   "do_za_2002_03_14_283_f" = "do_za_2002_03_02_283_f",
  #   "em_sa_2001_09_21_707_f" = "em_sa_2001_09_21_346_f",
  #   "lu_le_2001_07_04_283_f" = "lu_le_2001_07_01_283_f",
  #   "ch_ma_2001_10_27_332_f" = "ch_ma__2001_10_27_332_f",
  #   "ch_pi_2004_02_25_126.f" = "ch.pi.2004.02.25.126.f",
  #   "to_zo_2001_01_30_089_m" = "to_zo_2001_01_30_089_m_",
  #   "gi_se_1999_07_09_402_f" = "gio_se_1999_07_09_402_f",
  #   "ar_co_1996_12_27_348_f" = "ar_co_1996_12_27_348_f_",
  #   "vi_bi_1996_01_17_478_f" = "Vi_bi_1996_01_17_478_f"
  # )
  
  # table(temp$subj_code)
  
  # remove rows when the same subject has redone the questionnaires a second or
  # third time.
  quest <- distinct(quest_raw, subj_code, .keep_all = TRUE)
  
  quest$subj_code <- janitor::make_clean_names(quest$subj_code)
  
  # correct subj_code for patients (6 of them are NOT patients)
  not_patients <- c(
    "is_ie_1986_08_18_291_f",
    "la_al_1996_06_14_190_f",
    "gr_ma_1995_09_05_060_f",
    "fr_ma_1996_02_16_959_f",
    "st_sa_1996_06_05_556_f",
    "al_pe_1996_09_02_886_m"
  )
  
  quest$is_patient <- ifelse(
    quest$subj_code %in% not_patients, 0, quest$is_patient
  )
  
  last <- quest %>%
    dplyr::select(all_of(temp_names), is_patient)
  
  saveRDS(
    last,
    here::here("data", "processed", "quest", "bmi_age_weight.rds")
  )
}








#' @description
#' Generate RDS file with complete questionnaire data
#' TODO: fix coding errors for subject codes
save_rds_file_raw_quest_data <- function() {
  # Read patients questionnaires data
  patients_data <- read_excel(
    here("data", "raw", "prl", "quest", "Questionari EDs-pop clinica (Risposte).xlsx")
  )
  
  names_cols <- names(patients_data)
  
  # Read psicometria data
  students_data <- read_excel(
    here("data", "raw", "prl", "quest", "Questionari EDs-psicometria  (Risposte).xlsx")
  )
  
  # To merge the file use the same columns' names as for the patients.
  # There are small differences in the columns' names ** TODO check!
  names(students_data) <- names_cols
  
  # Combine patients' and students' data
  temp1 <- bind_rows(patients_data, students_data)
  
  # Read community sample data
  community_data <- read_excel(
    here("data", "raw", "prl", "quest", "Questionari EDs 2020_2021 (Risposte).xlsx")
  )
  
  # Read EAT-26 data (not included in community_data)
  temp <- read_excel(
    here("data", "raw", "prl", "quest", "EAT-26 2020_2021 (Risposte).xlsx")
  )
  
  # Select only the EAT-26 and the subject code columns
  eat26_community_data <- temp[, -c(1, 3, 4, 5, 6)]
  
  # Change column name with subject code
  temp_names <- names(eat26_community_data)
  temp_names[1] <- "Inserisci il tuo codice anonimo (esempio: ma_ro_1997_05_04_174_m)"
  names(eat26_community_data) <- temp_names
  
  complete_community_data <-
    left_join(
      community_data, eat26_community_data,
      by = "Inserisci il tuo codice anonimo (esempio: ma_ro_1997_05_04_174_m)"
    )
  
  stud_pat_names <- names(temp1)
  com_samp_names <- names(complete_community_data)
  
  # setdiff(
  #   names(temp1), names(complete_community_data)
  # )
  
  # Change the columns names of complete_community_data
  com_samp_names[3] <- stud_pat_names[3]
  com_samp_names[10:27] <- stud_pat_names[10:27]
  names(complete_community_data) <- com_samp_names
  
  # Fix coding errors for variable age
  complete_community_data$`Età (anni)` <-
    forcats::fct_recode(
      complete_community_data$`Età (anni)`,
      "23.0" = "23 anni",
      "25.0" = "25 anni"
    )
  # complete_community_data$`Età (anni)`
  complete_community_data$`Età (anni)` <- as.numeric(
    as.character(complete_community_data$`Età (anni)`)
  )
  # complete_community_data$`Età (anni)`
  
  # data.frame including all raw questionnaires data
  raw_quest_data <- bind_rows(temp1, complete_community_data)
  
  saveRDS(
    raw_quest_data,
    here("data", "processed", "quest", "raw_quest_data.rds")
  )
}





find_subj_code_at_risk_eat26 <- function() {
  
  # Read complete raw data.
  quest_raw <- readRDS(
    here::here("data", "processed", "quest", "quest_data.rds")
  )
  
  # EAT-26
  eat26_data <- quest_raw %>%
    dplyr::select(num_range("eat26_", 1:26))
  
  eat26_tot_score <- rowSums(eat26_data)
  
  patients_ids <- find_patients_codes()
  
  bad_ids_maha[!(bad_ids_maha %in% patients_ids)]
  
  d <- tibble(
    eat26_tot_score = eat26_tot_score,
    subj_code = quest_raw$subj_code
  )
  
  d$at_risk <- ifelse(d$eat26_tot_score > 19, 1, 0)
  
  only_at_risk_df <- d %>%
    dplyr::filter(at_risk == 1)
  
  at_risk_codes <- only_at_risk_df$subj_code
  
  at_risk_codes[!(at_risk_codes %in% patients_ids)]
}


get_eat26 <- function() {
  patients_codes <- find_patients_codes()
  
  quest <- readRDS(
    here::here("data", "processed", "quest", "quest_data.rds")
  )
  
  quest_patients_eat26 <- quest %>%
    # dplyr::filter(is_patient == 1) %>%
    dplyr::select(subj_code, is_patient, dieting, bulimia, oral_control)
  
  quest_patients_eat26
}
