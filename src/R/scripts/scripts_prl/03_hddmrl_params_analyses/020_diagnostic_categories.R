# Script name: 020_diagnostic_categories.R
# Project: Eating disorders Montecatini.
# Script purpose: Determine whether. for patients, the hDDMrl parameterd depend   
# on the diagnostic categories.
# @author: Corrado Caudek <corrado.caudek@unifi.it>
# Date Created: Mon Nov  8 10:03:07 2021
# Last Modified Date: Mon Nov  8 10:03:07 2021
# 
# Notes: 
#   Use this script to save the subj_code and other demographic
#   information of all participants to be used in hDDMrl (anorexic patients
#   and controls).



# Prelims -----------------------------------------------------------------

suppressPackageStartupMessages({
  library("here")
  library("tidyverse")
})
library("cmdstanr")
library("brms")

# Increase max print
options(max.print = .Machine$integer.max)

cores <- parallel::detectCores()
cores

source(here("code", "functions", "funs_prl.R"))
source(here("code", "functions", "funs_quest.R"))
source(here("code", "functions", "funs_rescorla_wagner.R"))
source(here("code", "functions", "funs_careless_resp.R"))
source(here("code", "functions", "funs_gen_data_for_hddm.R"))
source(here("code", "functions", "funs_param_analyses.R"))
source(here("code", "functions", "funs_param_classification_analyses.R"))


# This is the complete list of the patients' codes who completed the PRL task.  
# There are 64 patients.
patients_with_prl <- c(
  "al_ca_1996_03_27_621_f", "al_ro_1989_04_25_160_f", "al_zu_1997_04_02_880_f", "am_gu_1999_02_11_937_f",
  "an_am_1996_05_12_176_f", "an_de_1998_11_10_289_f", "ar_ce_2005_04_20_937_f", "ar_co_1996_12_27_348_f",
  "as_ga_2005_06_15_329_f", "au_ru_1998_09_21_806_f", "be_ma_1999_06_15_475_f", "bi_an_2001_09_16_735_f",
  "bi_di_2006_04_20_725_f", "ca_fa_1996_03_26_092_f", "ca_po_2002_05_25_700_f", "ca_so_2001_01_09_118_f",
  "ch_be_1990_12_20_153_f", "ch_br_1993_10_04_623_f", "ch_ca_2000_09_26_406_f", "ch_ma_2001_10_27_332_f",
  "ch_na_2007_06_23_908_f", "ch_pi_2004_02_25_126_f", "ch_ri_1993_05_05_564_f", "cr_gi_1994_10_14_378_f",
  "da_de_1998_08_15_141_m", "de_sc_1992_07_02_116_f", "el_ma_1986_06_14_839_f", "em_al_1989_07_27_200_f",
  "em_bi_2007_12_28_766_f", "em_gr_2002_08_25_628_f", "em_or_2003_01_02_101_f", "es_bo_2004_07_23_474_f",
  "fe_al_1988_05_06_180_f", "fe_ma_1998_06_29_257_f", "fe_sa_2002_05_09_008_f", "fr_au_1987_12_16_221_f",
  "fr_bo_1993_09_09_170_f", "fr_la_2004_05_17_363_f", "fr_ro_1982_08_15_048_f", "ga_gi_2003_02_09_229_f",
  "gi_ba_2008_01_31_376_f", "gi_ma_1999_09_26_585_f", "gi_to_1996_02_02_043_f", "gi_za_1992_09_07_575_f",
  "gr_bo_1996_07_31_547_f", "gr_de_2002_09_21_426_f", "he_ha_2006_04_21_874_f", "il_fu_2002_12_30_306_f",
  "ir_bo_1981_03_29_325_f", "ir_pi_2002_01_22_765_f", "ir_to_2007_08_01_838_f", "ir_ve_2004_02_09_500_f",
  "lu_mu_1997_03_18_059_f", "lu_te_1990_10_28_496_f", "ma_ba_1995_05_25_321_f", "ma_be_1997_09_01_726_f",
  "ma_va_1998_07_04_538_f", "ma_za_2002_02_28_051_f", "ra_al_2002_10_05_370_f", "sa_ta_2003_11_14_150_f",
  "so_be_2008_12_15_399_f", "ch_pi_2001_10_08_418_f", "cr_pa_1969_04_12_179_f", "gi_va_1992_04_14_174_f"
)

# We have to split them in two diagnostic categories (AN, BN).

# Read diagnostic categories.
diagn_categories <- rio::import(
  here::here(
    "data", "raw", "misc", "diagn_cat.xlsx"
  )
)

table(
  diagn_categories$diagnosis, diagn_categories$is_recovered
)
# The data.frame diagn_categories contains the diagnostic categories of all
# patients who completed the PRL task.

# Read raw data in the format required for HDDMrl.
raw_data <- rio::import(
  here(
    "data", "processed", "prl", "data_for_hddm", "hddm_input.csv"
  )
)

# Select patients.
patients <- raw_data %>% 
  dplyr::filter(
    is_patient == 1
  )

# Add diagnostic categories.
patients_df <- left_join(patients, diagn_categories, by = "subj_code")

# Use only AN = anorexic, AN-R = recovered anorexic, BN = bulimia nervosa, 
# BN-R = recovered bulimia nervosa.
patients_df <- patients_df %>% 
  mutate(
    gravity = case_when(
      diagnosis == "AN" & is_recovered == "no" ~ "AN",
      diagnosis == "AN" & is_recovered == "si" ~ "AN-R",
      diagnosis == "BN" & is_recovered == "no" ~ "BN",
      diagnosis == "BN" & is_recovered == "si" ~ "BN-R",
      TRUE ~ "ERROE"
  )
) %>% 
  dplyr::select(-c(diagnosis, is_recovered))

table(
  patients_df$gravity
)



# Get at-risk students ----------------------------------------------------

at_risk_subj_codes <- c(
  "vi_mi_2000_08_21_472_f", "ma_ta_2001_05_23_401_f", "sa_sa_2000_11_24_418_m", "gi_se_1999_07_09_402_f",
  "ch_lo_2000_09_25_565_f", "ma_ma_2001_07_10_611_f", "li_li_2001_12_04_406_f", "ot_na_1999_03_18_271_f",
  "ma_pa_2001_06_11_636_f", "gi_sp_1995_10_16_533_f", "se_pi_2001_01_22_920_f", "cl_pe_2001_09_26_424_f",
  "ve_ma_2000_07_25_946_f", "el_li_1999_09_08_687_f", "an_re_2001_08_28_633_f", "ca_mi_2001_06_16_988_f",
  "ma_la_2001_09_12_609_f", "gi_po_1998_11_07_576_f", "ed_sc_2001_09_26_034_m", "ma_pi_2001_05_11_566_f",
  "ha_ri_2001_07_07_704_f", "gi_me_2001_03_31_627_f", "an_ci_1994_03_26_829_f", "gi_ga_2001_07_20_277_f",
  "mo_fo_1996_12_29_813_f", "gi_gi_1990_03_28_384_f", "ga_fi_2000_12_23_825_f", "sa_la_1994_11_13_963_f",
  "al_be_1997_03_10_966_f", "ch_ma_1995_08_28_639_f", "la_al_1996_06_14_190_f"
)

at_risk_df <- raw_data[raw_data$subj_code %in% at_risk_subj_codes, ]
at_risk_df$gravity <- "AT_RISK"

table(at_risk_df$gravity)


# Villa dei Pini ----------------------------------------------------------


vdp <- rio:::import(
  here(
    "data", "processed", "prl", "data_for_hddm", "villa_dei_pini", 
    "vdp_hddm_input.csv"
  )
)

vdp$subj_code <- vdp$subj_code %>% 
  dplyr::recode(
    "ausa_bu_1996_05_06_888_f" ="au_bu_1996_05_06_888_f",
    "mama_bi_2004_05_09_224_f" = "ma_bi_2004_05_09_224_f"
  )

vdp_diagnosis <- rio:::import(
  here(
    "data", "processed", "prl", "data_for_hddm", "villa_dei_pini", 
    "vdp_diagnosis.xlsx"
  )
) %>% 
  dplyr::select(
    -c(age, gender)
  )


vdp_df <- left_join(
  vdp, vdp_diagnosis, by = "subj_code"
) %>% 
  dplyr::rename(
    gravity = group
  )

table(vdp_df$gravity)

# Change RT.
summary(vdp_df$rt)

temp <- vdp_df
head(temp)

temp$RT <- ifelse(
  temp$rt < 0.2, NA, temp$rt
)

summary(temp$RT)

set.seed(123)
# Multiple imputation on NAs.
temp1 <- data.frame(
  rt          = temp$RT, 
  trial       = temp$trial,
  gravity     = temp$gravity, 
  subj_code   = temp$subj_code
)

# Imputes the "best value" according to the linear regression model, also 
# known as regression imputation.
imp <- mice::mice(temp1, method = "norm.predict", m = 1) 
temp2 <- complete(imp)
summary(temp2$rt)
vdp_df$rt <- temp2$rt


# Combine dfs -------------------------------------------------------------

mydat <- bind_rows(
  patients_df, at_risk_df, vdp_df
)

mydat$subj_idx <- NULL


# I want to number each subject in the data.frame so that subjects are 
# ordered sequentially, according to the order they appear in the data frame. 
# https://community.rstudio.com/t/why-does-group-indices-use-alphabetical-ordering/5452
# As suggested in the above link, I wrap group_indices in another function:
grpid = function(x) match(x, unique(x))
# then
temp <- mydat %>% 
  mutate(subj_idx = group_indices(., subj_code) %>% grpid)
# In this manner, the variable subj_idx assigns an integer to each subject;
# this integer is ordered according to the sequence in which the subjects are 
# present in the data.frame.
# table(diagn_cat_df$subj_idx)


# Remove condition "food"
diagn_cat_df <- temp %>% 
  dplyr::filter(
    stim != "food"
  )


diagn_cat_df$split_by <- as.numeric(factor(diagn_cat_df$gravity)) - 1
unique(diagn_cat_df$split_by)

table(diagn_cat_df$split_by, diagn_cat_df$gravity)
#     AN AN-R AN-RES AT_RISK   BN BN-R BN-RES
# 0 5440    0      0       0    0    0      0
# 1    0 1600      0       0    0    0      0
# 2    0    0   2240       0    0    0      0
# 3    0    0      0    3520    0    0      0
# 4    0    0      0       0 1600    0      0
# 5    0    0      0       0    0  800      0
# 6    0    0      0       0    0    0   2400



# Check RTs ---------------------------------------------------------------

summary(diagn_cat_df$rt)



# Save RDS file for HDDMrl ------------------------------------------------


rio::export(
  diagn_cat_df,
  here::here(
    "data", "processed", "prl", "data_for_hddm", "diagn_cat.csv"
  )
)




####### END


# Get parameter estimates -------------------------------------------------

# # if file does not exist:
# if(
#   !file.exists(
#     here("data", "processed", "prl", "prl_params_cleaned", 
#          # "prl_params_no_split_by_group_and_subj_code.rds"
#          "prl_params_and_subj_code.rds"
#     )
#   )
# ) add_subj_code_to_hddm_params_and_save()

add_subj_code_to_hddm_params_and_save()
# here::here("data", "processed", "prl", "prl_params_cleaned", 
#            "prl_params_and_subj_code_2022_03_03.rds")




# Read diagnostic categories that have been recorded.
remission_ids_list <- rio::import(
  here::here(
    "data", "raw", "misc", "remission_ids_list.xlsx"
  )
)

params_init <- readRDS(
  here::here("data", "processed", "prl", "prl_params_cleaned", 
             "prl_params_and_subj_code_2022_03_03.rds")
)
params_init$subj_idx <- NULL

params <- left_join(params_init, recorded_diagn_categories, by = "subj_code")



# Add is_patient variable.
params$is_patient <- ifelse(
  params$subj_code %in% complete_list_patients, 1, 0
)

# Add category == HC for all controls.
params$category <- ifelse(
  params$is_patient == 0, "HC", params$category
)

table(params$category)
summary(factor(params$category))

# Required for dplyr::recode().
params$category <- as.character(params$category)

params$category <- ifelse(
  params$subj_code == "al_ca_1996_03_27_621_f", "BN", params$category)
params$category <- ifelse(
  params$subj_code == "au_ru_1998_09_21_806_f", "AN", params$category)
params$category <- ifelse(
  params$subj_code == "be_ma_1999_06_15_475_f", "AN", params$category)
params$category <- ifelse(
  params$subj_code == "ca_fa_1996_03_26_092_f", "BN", params$category)
params$category <- ifelse(
  params$subj_code == "ca_po_2002_05_25_700_f", "AN", params$category)
params$category <- ifelse(
  params$subj_code == "ch_ca_2000_09_26_406_f", "AN", params$category)
params$category <- ifelse(
  params$subj_code == "ch_ri_1993_05_05_564_f", "AN", params$category)
params$category <- ifelse(
  params$subj_code == "em_bi_2007_12_28_766_f", "AN", params$category)
params$category <- ifelse(
  params$subj_code == "fr_bo_1993_09_09_170_f", "AN", params$category)
params$category <- ifelse(
  params$subj_code == "ga_gi_2003_02_09_229_f", "AN", params$category)
params$category <- ifelse(
  params$subj_code == "gi_ma_1999_09_26_585_f", "AN", params$category)
params$category <- ifelse(
  params$subj_code == "gi_za_1992_09_07_575_f", "AN", params$category)
params$category <- ifelse(
  params$subj_code == "ir_ve_2004_02_09_500_f", "AN", params$category)
params$category <- ifelse(
  params$subj_code == "lu_te_1990_10_28_496_f", "BN", params$category)
params$category <- ifelse(
  params$subj_code == "ma_be_1997_09_01_726_f", "AN", params$category)
params$category <- ifelse(
  params$subj_code == "ma_va_1998_07_04_538_f", "AN", params$category)
params$category <- ifelse(
  params$subj_code == "ra_al_2002_10_05_370_f", "BN", params$category)
params$category <- ifelse(
  params$subj_code == "cr_pa_1969_04_12_179_f", "BN", params$category)
params$category <- ifelse(
  params$subj_code == "em_or_2003_01_02_101_f", "AN", params$category)


# Get at risk.
quest_at_risk <- readRDS(
  here::here("data", "processed", "quest", "quest_data.rds")
) %>% 
  dplyr::select(
    subj_code, eat26_at_risk, is_patient
  ) %>% 
  dplyr::filter(is_patient == 0 & eat26_at_risk == 1 )

ids_at_risk <- quest_at_risk$subj_code %>% 
  as.character()


# Clean up HC.
params_patients <- params %>% 
  dplyr::filter(is_patient == 1)
params_hc <- params %>% 
  dplyr::filter(is_patient == 0)

invlogit <- function(x) {
  exp(x / (1 + exp(x)))
}

hist(invlogit(params_hc$alpha_pos))
hist(invlogit(params_hc$alpha_neg))

params_hc_clean <- params_hc %>% 
  dplyr::filter(
    (invlogit(params_hc$alpha_pos) > 0.2) & 
      (invlogit(params_hc$alpha_neg > 0.2))
  )

params_hc_clean <- params_hc_clean %>% 
  mutate(
    category = case_when(
      subj_code %in% ids_at_risk ~ "AR",
      TRUE ~ "HC"
      )
  )
summary(factor(params_hc_clean$category))


controls_with_na <- c(
  "fr_ba_1997_10_29_663_f", 
  "so_ia_2000_03_21_569_f", 
  "an_ol_2000_03_06_615_f", 
  "na_ge_1996_05_29_070_f", 
  "fr_pl_2002_02_14_755_f", 
  "ar_cr_1996_08_05_738_f", 
  "fr_da_1994_04_19_591_f", 
  "gi_ge_1992_09_10_458_f", 
  "ch_gr_1992_09_24_323_f",
  "gi_fi_1996_03_09_339_f",
  "si_sc_1992_12_23_119_f",
  "an_me_2001_08_15_795_f",
  "al_an_1996_06_03_205_f",
  "su_or_1993_01_13_765_f",
  "ni_bi_1996_12_16_648_f",
  "cr_ci_1999_08_21_931_f",
  "gi_ma_1998_10_27_642_f",
  "ch_pr_1997_05_06_654_f",
  "fe_ba_1993_05_15_800_m",
  "ma_ro_1996_10_30_794_f",
  "fr_ba_1957_10_12_166_m",
  "ir_bo_1995_04_03_290_f",
  "fr_di_2001_07_01_522_f",
  "fr_ba_1997_10_29_663_f",
  "an_ol_2000_03_06_615_f",
  "na_ge_1996_05_29_070_f",
  "fr_pl_2002_02_14_755_f",
  "ar_cr_1996_08_05_738_f",
  "fr_da_1994_04_19_591_f",
  "gi_ge_1992_09_10_458_f",
  "ch_gr_1992_09_24_323_f",
  "gi_fi_1996_03_09_339_f",
  "si_sc_1992_12_23_119_f",
  "al_an_1996_06_03_205_f",
  "su_or_1993_01_13_765_f",
  "ni_bi_1996_12_16_648_f",
  "gi_ma_1998_10_27_642_f",
  "ma_ro_1996_10_30_794_f",
  # bad Rescorla-Wagner
  "fr_as_1997_02_10_127_f", "fr_ca_1994_02_07_453_f", "no_sp_2002_01_02_892_f", 
  "el_mi_1996_12_11_513_f", "le_la_2001_11_11_647_f", "ar_cr_1996_08_05_738_f"
)

params_hc_clean2 <- 
  params_hc_clean[!(params_hc_clean$subj_code %in% controls_with_na), ]


# Collapse BED in BN.
params_patients$category <- dplyr::recode(
  params_patients$category, 
  "BED" = "BN"
)
summary(factor(params_patients$category))


remission <- 
  params_patients[params_patients$subj_code %in% remission_ids_list$subj_code, ]
remission$category <- "RE"

not_remission <- 
  params_patients[!(params_patients$subj_code %in% remission_ids_list$subj_code), ]

params_patients_tot <- rbind(remission, not_remission)
table(params_patients_tot$category)

params_clean <- rbind(params_patients_tot, params_hc_clean2)
summary(factor(params_clean$category))

# Order category levels.
params_clean$category <- factor(
  params_clean$category, 
  ordered = TRUE, 
  levels = c("AN", "BN", "RE", "AR", "HC")
)
summary(factor(params_clean$category))


params_clean %>% 
  group_by(stim, category) %>% 
  summarise(
    an = mean(alpha_neg, trim = 0.05),
    ap = mean(alpha_pos, trim = 0.05),
    t = mean(t, trim = 0.05),
    a = mean(a, trim = 0.05),
    v = mean(v, trim = 0.05),
    n = n()
  )



# Save df for generating hDDMrl inpu --------------------------------------


diagn_cat_df <- params_clean %>% 
  dplyr::select(
    subj_code, category
  )

saveRDS(
  diagn_cat_df,
  here("data", "processed", "prl", "complete_cleaned_raw_data", 
       "diagn_cat.rds")
  
)




# Prelim analyses ---------------------------------------------------------


m1a <- brms::brm(
  bf(
    alpha_pos ~ category * stim + (1 | subj_code)
  ),
  family = asym_laplace(),
  chains = 4, 
  cores = 12,
  data = params_clean,
  control = list(
    max_treedepth = 15,
    adapt_delta = 0.9),
  backend = "cmdstanr"
)

pp_check(m1a, type = "dens_overlay", ndraws = 100)
plot(loo(m1a))

plot(conditional_effects(m1a), ask = FALSE)


# Contrasts
params_clean$grp <- paste0(params_clean$stim, params_clean$category)
table(params_clean$grp)

m2 <- brms::brm(
  bf(
    alpha_pos ~ grp + (1 | subj_code)
    #sigma ~ grp
  ),
  family = asym_laplace(),
  chains = 4, 
  cores = 12,
  data = params_clean,
  # control = list(
  #   max_treedepth = 10,
  #   adapt_delta = 0.9),
  backend = "cmdstanr"
)
pp_check(m2, type = "dens_overlay", ndraws = 100)
plot(loo(m2))


library("emmeans")

fit2_em <- emmeans(m2, ~ grp)
fit2_em
# grp       emmean lower.HPD upper.HPD
# foodAN    -0.501    -1.560     0.544
# foodAR     1.575     0.581     2.567
# foodBN     1.230    -0.217     2.589
# foodHC     1.556     0.915     2.116
# neutralAN  1.667     0.785     2.573
# neutralAR  1.765     0.712     2.873
# neutralBN  2.337     0.959     3.736
# # neutralHC  2.101     1.361     2.766
# 
# Point estimate displayed: median 
# HPD interval probability: 0.95 

contr_em <- contrast(fit2_em, "tukey")
contr_em
# 
# contrast              estimate lower.HPD upper.HPD
# foodAN - foodAR        -2.0571    -3.116    -1.066
# foodAN - foodBN        -1.7029    -3.175    -0.398
# foodAN - foodHC        -2.0424    -2.814    -1.209
# foodAN - neutralAN     -2.1480    -2.994    -1.243
# foodAN - neutralAR     -2.2645    -3.361    -1.190
# foodAN - neutralBN     -2.8138    -4.226    -1.415
# foodAN - neutralHC     -2.5746    -3.353    -1.697
# foodAR - foodBN         0.3562    -1.038     1.750
# foodAR - foodHC         0.0213    -0.823     0.829
# foodAR - neutralAN     -0.0902    -1.026     0.953
# foodAR - neutralAR     -0.2052    -1.145     0.805
# foodAR - neutralBN     -0.7437    -2.199     0.624
# foodAR - neutralHC     -0.5128    -1.371     0.267
# foodBN - foodHC        -0.3290    -1.550     0.917
# foodBN - neutralAN     -0.4467    -1.739     0.974
# foodBN - neutralAR     -0.5556    -1.993     0.890
# foodBN - neutralBN     -1.1248    -2.536     0.406
# foodBN - neutralHC     -0.8770    -2.029     0.408
# foodHC - neutralAN     -0.1089    -0.825     0.603
# foodHC - neutralAR     -0.2145    -1.128     0.627
# foodHC - neutralBN     -0.7681    -2.045     0.426
# foodHC - neutralHC     -0.5433    -0.842    -0.209
# neutralAN - neutralAR  -0.1101    -1.215     0.905
# neutralAN - neutralBN  -0.6587    -2.064     0.662
# neutralAN - neutralHC  -0.4308    -1.198     0.257
# neutralAR - neutralBN  -0.5282    -1.953     0.889
# neutralAR - neutralHC  -0.3267    -1.137     0.639
# neutralBN - neutralHC   0.2295    -1.007     1.445
# 
# Point estimate displayed: median 
# HPD interval probability: 0.95 



# Get demo info.
demoinfo <- get_demo_info() %>% 
  arrange(subj_code) 

demoinfo %>% as.data.frame()

df <- full_join(params, demoinfo, by = "subj_code") %>% 
  as.data.frame()

df$is_patient <- NULL
df$is_patient <- df$is_patient_p
df$is_patient_p <- NULL

# Patient ch_ri_1993_05_05_564_f is anorexic.
df$is_anorexic <- 
  ifelse(df$subj_code == "ch_ri_1993_05_05_564_f", 1, df$is_anorexic)

df$is_anorexic <- 
  ifelse(df$subj_code == "em_or_2003_01_02_101_f", 1, df$is_anorexic)


controls_with_na <- c(
  "fr_ba_1997_10_29_663_f", 
  "so_ia_2000_03_21_569_f", 
  "an_ol_2000_03_06_615_f", 
  "na_ge_1996_05_29_070_f", 
  "fr_pl_2002_02_14_755_f", 
  "ar_cr_1996_08_05_738_f", 
  "fr_da_1994_04_19_591_f", 
  "gi_ge_1992_09_10_458_f", 
  "ch_gr_1992_09_24_323_f",
  "gi_fi_1996_03_09_339_f",
  "si_sc_1992_12_23_119_f",
  "an_me_2001_08_15_795_f",
  "al_an_1996_06_03_205_f",
  "su_or_1993_01_13_765_f",
  "ni_bi_1996_12_16_648_f",
  "cr_ci_1999_08_21_931_f",
  "gi_ma_1998_10_27_642_f",
  "ch_pr_1997_05_06_654_f",
  "fe_ba_1993_05_15_800_m",
  "ma_ro_1996_10_30_794_f",
  "fr_ba_1957_10_12_166_m",
  "ir_bo_1995_04_03_290_f",
  "fr_di_2001_07_01_522_f",
  "fr_ba_1997_10_29_663_f",
  "an_ol_2000_03_06_615_f",
  "na_ge_1996_05_29_070_f",
  "fr_pl_2002_02_14_755_f",
  "ar_cr_1996_08_05_738_f",
  "fr_da_1994_04_19_591_f",
  "gi_ge_1992_09_10_458_f",
  "ch_gr_1992_09_24_323_f",
  "gi_fi_1996_03_09_339_f",
  "si_sc_1992_12_23_119_f",
  "al_an_1996_06_03_205_f",
  "su_or_1993_01_13_765_f",
  "ni_bi_1996_12_16_648_f",
  "gi_ma_1998_10_27_642_f",
  "ma_ro_1996_10_30_794_f"
)

df$is_anorexic <- 
  ifelse(df$is_patient == 0 & df$subj_code %in% controls_with_na, 
         0, df$is_anorexic)

df %>% 
  dplyr::select(stim, subj_code, is_patient, is_anorexic, alpha_pos, bmi, age) %>% 
  arrange(subj_code)

df$is_bulimic <- 
  ifelse(df$is_patient == 1 & df$is_anorexic == 0, 1, 0)

glimpse(df)

only_patients <- df %>% 
  dplyr::filter(is_patient == 1) 
length(unique(only_patients$subj_code))

table(df$is_patient)



bad_controls_rescorla_wagner <- 
  c("fr_as_1997_02_10_127_f", "fr_ca_1994_02_07_453_f", "no_sp_2002_01_02_892_f", 
    "el_mi_1996_12_11_513_f", "le_la_2001_11_11_647_f", "ar_cr_1996_08_05_738_f")


df %>% 
  # dplyr::filter(gender == "Female") %>% 
  dplyr::filter(stringr::str_count(df$subj_code) > 0) %>% 
  dplyr::filter(!(subj_code %in% bad_controls_rescorla_wagner)) %>% 
  dplyr::filter(!(is_patient == 1 & is_anorexic == 0)) %>% 
  group_by(is_patient, is_anorexic, stim) %>% 
  summarise(
    lr_pos = mean(alpha_pos, na.rm = TRUE),
    lr_neg = mean(alpha_neg, na.rm = TRUE),
    t = mean(t, na.rm = TRUE, trim = 0.1),
    a = mean(a, na.rm = TRUE, trim = 0.1),
    v = mean(v, na.rm = TRUE, trim = 0.1),
    z = mean(z, na.rm = TRUE, trim = 0.1),
    age = mean(age, na.rm = TRUE),
    n = n()
  )


temp <- 
  df %>% 
  # dplyr::filter(gender == "Female") %>% 
  dplyr::filter(stringr::str_count(df$subj_code) > 0) %>% 
  dplyr::filter(!(subj_code %in% bad_controls_rescorla_wagner)) %>% 
  dplyr::filter(!(is_patient == 1 & is_anorexic == 0)) 


good_subjects_demographics <- temp %>% 
  dplyr::select(subj_code, stim, is_anorexic, gender, age, bmi)

foo <- good_subjects_demographics
foo$gender <- NULL
foo$subj_code <- NULL
foo$stim <- NULL

imp <- mice::mice(foo, method = "norm.predict", m = 1) 
temp <- complete(imp)

good_subjects_demographics$age <- temp$age
good_subjects_demographics$bmi <- temp$bmi

head(good_subjects_demographics)

good_subjects_demographics %>% 
  group_by(is_anorexic) %>% 
  summarise(
    bmi = mean(bmi)
  )

foo$is_anorexic <- factor(foo$is_anorexic)

foo %>% 
  ggplot(aes(x=bmi, color=is_anorexic)) +
  geom_density()


saveRDS(
  good_subjects_demographics,
  here::here(
    "data", "processed", "prl", "data_for_hddm", 
    "good_subjects_demographics.rds"
  )
)




# Stop here.





mod <- lme4::lmer(
  v ~ is_anorexic + stim + bmi +
    (1 | subj_code),
  data = df
)
summary(mod)

mod <- lme4::lmer(
  alpha_pos ~ is_anorexic * stim + bmi +
    (1 | subj_code),
  data = df
)
summary(mod)









# Get patients codes.
patients_codes <- find_patients_codes()

params$is_patient <- ifelse(params$subj_code %in% patients_codes, 1, 0)

only_patients_param <- params %>% 
  dplyr::filter(is_patient == 1)

only_patients_param$subj_idx <- NULL
only_patients_param$is_patient <- NULL

controls_params <- params %>% 
  dplyr::filter(is_patient == 0)


# Get diagnostic categories -----------------------------------------------

temp <- rio::import(
  here::here("data", "raw", "diagnostic_categories.xlsx")
)


diagn_cat <- data.frame(
  subj_code = temp[, 1],
  diagnostic_cat = temp[, 2]
)

diagn_cat %>% 
  arrange(subj_code)


# Only patients group -----------------------------------------------------

patients_params <- left_join(only_patients_param, diagn_cat, by = "subj_code")
head(patients_params)

patients_params %>% 
  arrange(subj_code)


# 17 participants have NA as diagnostic category.
# On average, they are similar to AN-R. So, they
# are attributed to that class.
patients_params$diagnostic_cat <- ifelse(
  is.na(patients_params$diagnostic_cat), 
  "AN-R", patients_params$diagnostic_cat
)

patients_params$diagnostic_cat <- ifelse(
  patients_params$diagnostic_cat == "AN", 
  "AN-R", patients_params$diagnostic_cat
)


anorexics <- patients_params %>% 
  dplyr::filter(diagnostic_cat == "AN-R")

anorexics$subj_code <- factor(anorexics$subj_code)

anorexics_subj_codes <- anorexics$subj_code %>% 
  unique()


# Save subj_code of anorexic patients.
saveRDS(
  anorexics_subj_codes,
  here("data", "processed", "prl", "anorexics_subj_codes.rds")
)


patients_params$a_pos <- 
  exp(patients_params$alpha_pos) / (1 + exp(patients_params$alpha_pos))
patients_params$a_neg <- 
  exp(patients_params$alpha_neg) / (1 + exp(patients_params$alpha_neg))

patients_params %>% 
  group_by(stim, diagnostic_cat) %>% 
  summarise(
    lr_pos = mean(a_pos, na.rm = TRUE, trim = 0.1),
    lr_neg = mean(a_neg, na.rm = TRUE, trim = 0.1),
    t = mean(t, na.rm = TRUE, trim = 0.1),
    a = mean(a, na.rm = TRUE, trim = 0.1),
    v = mean(v, na.rm = TRUE, trim = 0.1),
    z = mean(z, na.rm = TRUE, trim = 0.1),
    n = n()
  )


controls_params$a_pos <- 
  exp(controls_params$alpha_pos) / (1 + exp(controls_params$alpha_pos))
controls_params$a_neg <- 
  exp(controls_params$alpha_neg) / (1 + exp(controls_params$alpha_neg))


controls_params %>% 
  group_by(stim) %>% 
  summarise(
    lr_pos = mean(a_pos, na.rm = TRUE, trim = 0.1),
    lr_neg = mean(a_neg, na.rm = TRUE, trim = 0.1),
    t = mean(t, na.rm = TRUE, trim = 0.1),
    a = mean(a, na.rm = TRUE, trim = 0.1),
    v = mean(v, na.rm = TRUE, trim = 0.1),
    z = mean(z, na.rm = TRUE, trim = 0.1),
    n = n()
  )





