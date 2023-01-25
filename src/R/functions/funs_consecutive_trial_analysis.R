#' This version removes all participants with Gelman-Rubin convergence 
#' problems. The excluded participants all belong to the HC group, apart from 
#' "al_ro_1989_04_25_160_f", who is AN. The same procedure has been used for
#' creating the input for the dDDMrl analysis.

gen_data_for_consecutive_trial_analysis <- function() {
  
  prl_data <- readRDS(
    here::here("data", "processed", "prl", "raw_prl_data", "prl_tot_raw_data.rds")
  )
  
  prl_data$feedback <- ifelse(
    prl_data$feedback == 2, 0, prl_data$feedback
  )
  prl_data$feedback <- ifelse(prl_data$feedback == 3, NA, prl_data$feedback)
  
  prl_data$feedback <- 
    ifelse(prl_data$rt < 150 | prl_data$rt > 2499, NA, prl_data$feedback)
  
  prl_data$rt1 <- 
    ifelse(prl_data$rt < 150 | prl_data$rt > 2499, NA, prl_data$rt)
  
  prl_data$rt1 <- ifelse(prl_data$trial == 1, NA, prl_data$rt1)
  
  # For 37 control participants we don't have the questionnaires. 
  # They are all attributed to the HC category.
  prl_data$diag_cat <- ifelse(is.na(prl_data$diag_cat), "HC", prl_data$diag_cat)
  
  data_for_imp <- prl_data %>% 
    dplyr::select(
      subj_name, stimulus_type, epoch, target_position, which_image_is_rewarded_more,
      keypress, rt1, feedback, is_target_img_rewarded_in_first_epoch, 
      is_target_rewared_in_present_epoch, position_target_img, resp, 
      is_target_img_chosen, trial, group, diag_cat
    )
  
  set.seed(123)
  miceObj <- miceRanger::miceRanger(
    data_for_imp
    , m = 1
    , returnModels = TRUE
    , verbose = TRUE
  )
  
  dataList <- miceRanger::completeData(miceObj)
  
  d <- dataList[[1]]
  
  accuracy_df <- d %>% 
    dplyr::filter(group == "controls") %>% 
    group_by(subj_name) %>% 
    summarise(
      avg_acc = mean(feedback, na.rm = TRUE),
      avg_rt = median(rt1, na.rm = TRUE)
    ) %>% 
    arrange(avg_rt)
  
  # accuracy_df %>% as.data.frame()
  
  ids_bad_cntr_acc_rt <- 
    accuracy_df[(accuracy_df$avg_acc < 0.45 | accuracy_df$avg_rt < 300), ]$subj_name
  
  # CORRECTION. Keep gi_pe_2001_08_20_230_f because it was used in the hDDMrl 
  # analysis.
  ids2_bad_cntr_acc_rt <- ids_bad_cntr_acc_rt[
    !ids_bad_cntr_acc_rt %in% c("gi_pe_2001_08_20_230_f")
  ]
  
  prl_data2 <- d[!(d$subj_name %in% ids2_bad_cntr_acc_rt), ]
  length(unique(prl_data2$subj_name))
  
  # Read list of subj_name for participants flagged for careless responding on
  # the questionnaires data.
  careless_resp_ids <- readRDS(
    here::here(
      "src", "R", "scripts", "scripts_quest", "02_flag_careless_resp", 
      "bad_ids_from_careless_resp_indices.rds"
    )
  )
  # CORRECTION. Keep "ni_pr_1999_10_16_006_f" because it was used in the 
  # hDDMrl analysis.
  careless_resp_ids2 <- careless_resp_ids[
    !careless_resp_ids %in% c("ni_pr_1999_10_16_006_f")
  ]
  
  # Both the ids found with the traditional careless responding methods and the
  # ids used with the method_1 procedure.
  bad_ids_method1 <- c(
    "gi_ma_2001_05_02_922_f", "an_si_1996_01_03_744_m", "to_pi_2000_09_11_140_m",
    "bi_sa_2001_03_01_675_f", "ch_ma_1995_08_28_639_f", "vi_mi_2000_08_21_472_f",
    "an_ma_1995_01_07_728_f", "il_si_1995_11_28_393_f", "ma_lu_2000_09_18_459_m",
    "ca_ma_2001_07_01_650_f", "sa_pa_2001_05_14_311_f", "lo_pi_2001_09_08_203_m",
    "ag_no_2000_02_12_330_f", "cr_la_1997_12_18_107_f")
  
  bad_ids_careless <- c(
    careless_resp_ids2, bad_ids_method1
  )
  
  controls_with_low_accuracy <- accuracy_df[accuracy_df$avg_acc < 0.5, ]$subj_name
  
  to_be_excluded <- bad_ids_careless[bad_ids_careless %in% controls_with_low_accuracy]
  
  # These subjects are removed because of convergence problems with hDDMrl.
  to_be_excluded2 <- c(
    "ja_qu_1999_09_21_439_m", 
    "ma_ma_2000_03_26_404_f", "da_sc_1993_01_08_813_f", "ir_po_1993_02_15_077_f",
    "an_am_1993_05_20_789_f", "si_co_1992_03_23_596_f",
    "so_tr_2001_01_04_222_f", "so_ca_1996_09_12_448_f",
    "al_ro_1989_04_25_160_f", # AN
    to_be_excluded)
  
  # to_be_excluded <- accuracy_df[
  #   accuracy_df$subj_name %in% bad_ids_careless & accuracy_df$avg_acc < 0.5, 
  # ]$subj_name
  
  prl_data3 <- prl_data2[!(prl_data2$subj_name %in% to_be_excluded2), ]
  # length(unique(prl_data3$subj_name))
  
  # These subjects have also been excluded from the input to hDDMrl.
  # Perhaps due to bad Gelman-Rubin statistics, not sure.
  also_escluded <- c(
    "gi_ca_2006_10_14_101_f", "sa_ca_2004_06_11_885_f", 
    "gi_qa_2001_11_24_193_f", "al_lu_1997_03_21_166_f", 
    "li_va_2001_05_29_471_f"
  )
  prl_data3 <- prl_data3[!prl_data3$subj_name %in% also_escluded, ]
  # length(unique(prl_data3$subj_name))
  
  # Data preparation for hDDMrl.
  prl_data3$stimulus_type <- factor(prl_data3$stimulus_type)
  prl_data3$stimulus_type <- prl_data3$stimulus_type %>% 
    forcats::fct_recode(
      neutral = "socialshame"
    )
  
  # prl_data3 %>% 
  #   group_by(group, diag_cat) %>% 
  #   summarise(
  #     n = n_distinct(subj_name)
  #   )
  # 
  # prl_data3 %>% 
  #   summarise(
  #     n = n_distinct(subj_name)
  #   )
  
  if (0) {
    prl_data3$is_rich_choice <- case_when(
      prl_data3$is_target_rewared_in_present_epoch & prl_data3$is_target_img_chosen ~ 1,
      !prl_data3$is_target_rewared_in_present_epoch & !prl_data3$is_target_img_chosen ~ 1,
      TRUE ~ 0
    )
  }
  
  prl_data3
  
}
