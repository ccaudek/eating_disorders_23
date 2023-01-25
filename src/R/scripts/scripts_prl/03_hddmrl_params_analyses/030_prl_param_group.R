# Script name: 030_prl_param_group.R
# Project: eating disorders with patients and controls
# Script purpose: comparison between groups
# @author: Corrado Caudek <corrado.caudek@unifi.it>
# Date Created: Fri Jun  4 11:29:43 2021
# Last Modified Date: Fri Jun  4 11:29:43 2021
# 
# Notes: 


library("here")
library("tidyverse")
library("forcats")
library("readxl")
library("stringr")
library("pROC")
library("brms")
library("cmdstanr")
set_cmdstan_path("/Users/corrado/cmdstan")

source(here::here("lib", "ed_fnc.R"))

# this list has been recovered by hand from the Excel file with the
# questionnaires data of the patients
patients_codes <- c("cr_gi_1994_10_14_347_f",
                    "bi_an_2001_09_16_735_f",
                    "ir_bo_1981_03_29_325_f",
                    "la87_ac98",
                    "al_zu_1997_04_02_880_f",
                    "ch_be_1990_12_20_153_f",
                    "grbo1996_07_31_547_f",
                    "am_gu_1999_02_11_937_f",
                    "ir_pi_2002_01_22_765_f",
                    "el_ma_1986_06_14_839_f",
                    "da_de_1998_08_15_141_m",
                    "em_gr_2002_08_25_628_f",
                    "fe_al_1988_05_06_180_f",
                    "gi_ma_1999_09_26_585_f",
                    "lu_mu_1997_03_059_f",
                    "ch_ca_2000_09_26_406_f",
                    "em_al_1989_07_27_200_f",
                    "ca_po_2002_05_25_700_f",
                    "cr_pa_1969_04_12_179_f",
                    "ar_co_1996_12_27_348_f_",
                    "a_dc_98",
                    "de_sc_1992_07_02_116_f",
                    "em_or_2003_02_01_101_f",
                    "ga_gi_2003_02_09_g",
                    "ju_yo_1998_05_28_316_f",
                    "ch_na_2007_06_23_908_f",
                    "ma_ba_1995_05_25_321_f",
                    "al_ro_1989_04_25_160_f",
                    "an_am_1996_05_12_176_f",
                    "ca_pa_2002_04_05_939",
                    "he_ha_2006_04_21_874_f",
                    "ch_br_1993_10_04_623_f",
                    "fe_ma_1998_06_29_257_f",
                    "fe_sa_2002_05_09_008_f",
                    "so_be_2008_12_15_399_f",
                    "ca_so_2001_01_09_118_f",
                    "ch_pi_2001_10_08_418_f",
                    "la_al_1996_06_14_190_f",
                    "is_ie_1986_08_18_291_f",
                    "al_pe_1996_09_02_886_m",
                    "sa_ta_2003_11_14_150_f",
                    "ch_pi_2004_02_25_126_f",
                    "as_ga_2005_06_15_329_f",
                    "fr_la_2004_05_17_363_f",
                    "ma_be_1996_" 
)


generate_rds_prl_params_and_quest_data(patients_codes)


# e  n  d  


