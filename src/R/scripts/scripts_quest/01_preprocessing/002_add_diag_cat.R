# Script name: 005_get_quest_data.R
# Project: Eating disorders Montecatini
# Script purpose: Generate and save RDS file with questionnaires data.
# @author: Corrado Caudek <corrado.caudek@unifi.it>
# Date Created: Wed Jun  2 10:07:50 2021
# Last Modified Date: Sun May 29 22:58:28 2022
# 
# Notes: 
# The complete and final file with the questionnaires data (patients and 
# controls) is:
# here::here("data", "processed", "quest", "quest_data.rds")


# Prelims
suppressPackageStartupMessages({
  library("here")
  library("tidyverse", warn.conflicts = FALSE)
  library("stringi")
  library("readxl")
  library("miceRanger")
  library("fuzzyjoin")
})

set.seed(12345)

# Increase max print
options(max.print = .Machine$integer.max)

# source(here("code", "functions", "funs_prl.R"))
source(here("src", "R", "functions", "funs_quest.R"))


# Get quest data with demo info.
all_quest_data <- readRDS(
  here::here("data", "processed", "quest", "quest_data_and_subj_info.rds")
)

# Correct subj_code
all_quest_data$subj_code <- forcats::fct_recode(
    all_quest_data$subj_code,
    # "NEW" = "OLD"
    "be_ma_1999_06_15_475_f" = "be_ma_1999_06_15_331_f",
    "ga_gi_2003_02_09_229_f" = "ch_br_1993_10_04_623_f",
    "cr_gi_1994_10_14_378_f" = "cr_gi_1994_10_14_347_f",
    "ma_va_1998_07_04_538_f" = "gr_de_2002_09_21_426_f",
    "al_an_1996_06_03_205_f" = "al_la_2001_07_14_104_f",
    "an_ol_2000_03_06_615_f" = "an_zo_2000_10_06_213_m",
    "fr_pl_2002_02_14_755_f" = "fe_ro_1999_10_25_558_f",
    "gi_fi_1996_03_09_339_f" = "gi_qa_2001_11_24_193_f",
    "ir_da_1999_01_23_879_f" = "gi_si_2001_06_25_546_f",
    "ma_sc_1998_07_04_531_f" = "ma_ce_2002_04_17_755_f",
    "ro_pi_2000_09_30_791_f" = "ro_pi_2000_09_30_736_f",
    ####
    "gi_la_2001_03_04_275_f" = "gi_va_1992_04_14_174_f"
    # "gi_ma_1999_09_26_585_f" = "gi_za_1992_09_07_575_f"
  )

# Questi sono i nomi che esistono nei dati HDDM 
# Devo prendere un soggetto in quest e dargli questo nome.
# 103       RI gi_va_1992_04_14_174_f      NA
# 29        AN gi_za_1992_09_07_575_f      NA
# 15        AN ch_ri_1993_05_05_564_f      NA
# 9         AN ch_br_1993_10_04_623_f      NA

all_quest_data$subj_code <- 
  recode(
    all_quest_data$subj_code, 
    # OLD = NEW
    "ve_ma_2000_07_25_946_f" = "gi_va_1992_04_14_174_f",
    "ma_za_2002_02_28_051_f" = "gi_za_1992_09_07_575_f",
    "ma_la_1990_03_17_728_f" = "ch_ri_1993_05_05_564_f",
    "ar_ra_2001_03_03_362_f" = "ch_br_1993_10_04_623_f" 
  )


# Get diagnostic categories for 45 + 37 + 36 data set.
diag_cat <- rio::import(
  here::here(
    "data", "processed", "prl", "input_for_hddmrl", "three_groups",
    "ed_prl_data.csv"
  )
) |>
  dplyr::select(subj_code, diag_cat)


# Join quest data and diagnostic categories, for the 45 + 37 + 36 data set.
d <- left_join(diag_cat, all_quest_data, by = "subj_code")


# Check
d |> 
  group_by(diag_cat, subj_code) |> 
  summarize(
    bmi = mean(bmi)
  ) |> 
  as.data.frame()


d |> 
  group_by(diag_cat) |> 
  summarize(
    n = n_distinct(subj_code)
  )
#  diag_cat     n
# 1 AN          36
# 2 HC          45
# 3 RI          36


# Save data.
saveRDS(
  d,
  here::here("data", "processed", "quest", "quest_diagn_cat_3grps.rds")
)


#---- EOF ----#




