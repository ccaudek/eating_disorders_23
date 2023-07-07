# Script name: 03_correct_subj_codes.R
# Project: ED Montecatini
# Script purpose: correct subj_codes for descriptive analyses
# @author: Corrado Caudek <corrado.caudek@unifi.it>
# Date Created: Thu Dec 22 12:33:49 2022
# Last Modified Date: Thu Dec 22 12:33:49 2022
#
# 👉 

# Get data used for RLDDM analysis.
ddm_dat <- rio::import(
  here::here("data", "processed", "prl", "input_for_hddmrl", "hddm_input_v3.csv")
)

# Subject codes for RLDDM.
ddm_codes <- unique(ddm_dat$subj_code)
length(ddm_codes)
# [1] 305

# Get quest data.
quest_dat <- readRDS(
  here::here("data", "processed", "quest", "quest_diagn_data.rds")
)

# Correct subjects identifiers.
quest_dat$subj_code <- forcats::fct_recode(
  quest_dat$subj_code,
  # "NEW" = "OLD"
  "be_ma_1999_06_15_475_f" = "be_ma_1999_06_15_331_f",
  "cr_gi_1994_10_14_378_f" = "cr_gi_1994_10_14_347_f",
  "ga_lu_2000_03_17_328_f" = "ga_lu_2000_03_17_468_f",
  "li_va_2001_05_29_471_m" = "li_va_2001_05_29_471_f",
  "lu_le_2001_07_04_283_f" = "lu_le_2001_07_01_283_f",
  "ro_pi_2000_09_30_791_f" = "ro_pi_2000_09_30_736_f"
)


# Subject codes for quest.
quest_codes <- unique(thedat$subj_code)


# Fix subj_codes for patients ---------------------------------------------

# Patiens codes in quest.
pat_quest_dat <- quest_dat |> 
  dplyr::filter(diagnosis == "AN" | diagnosis == "BN")

pat_quest_codes <- sort(unique(pat_quest_dat$subj_code))

# Find subj_codes of patients who completed the quest but are not in 
# ddm_codes.
setdiff(pat_quest_codes, ddm_codes)
# [1] "al_ro_1989_04_25_160_f" "ca_pa_2002_04_05_939_f" "cl_co_1996_03_16_886_f"
# [4] "gi_lu_1992_06_10_601_f" "ju_yo_1998_05_28_316_f" "ma_la_1990_03_17_728_f"

# Correct subjects identifiers.
# moved above
# quest_dat$subj_code <- forcats::fct_recode(
#   quest_dat$subj_code,
#   # "NEW" = "OLD"
#   "be_ma_1999_06_15_475_f" = "be_ma_1999_06_15_331_f",
#   "cr_gi_1994_10_14_378_f" = "cr_gi_1994_10_14_347_f"
# )


# Patiens codes in ddm.
pat_ddm_dat <- ddm_dat[!(ddm_dat$diag_cat == "HC" | ddm_dat$diag_cat == "RI"), ]
pat_ddm_codes <- unique(pat_ddm_dat$subj_code) |> 
  sort()

# Find subj_codes of patients who completed the ddm but are not in 
# quest codes.
setdiff(pat_ddm_codes, pat_quest_codes)
# [1] "al_ca_1996_03_27_621_f" "ch_ri_1993_05_05_564_f" "gi_za_1992_09_07_575_f"
# [4] "ma_va_1998_07_04_538_f"


########

hc_ddm_dat <- ddm_dat[(ddm_dat$diag_cat == "HC" | ddm_dat$diag_cat == "RI"), ]
hc_ddm_codes <- unique(hc_ddm_dat$subj_code) |> 
  sort()
length(hc_ddm_codes)
# [1] 239

hc_quest_dat <- quest_dat[
  (quest_dat$diag_cat == "HC" | quest_dat$diag_cat == "RI"), ]
dim(hc_quest_dat)
# [1]  265 192

hc_quest_codes <- unique(hc_quest_dat$subj_code) |> 
  sort()
length(hc_quest_codes)
# [1] 265

# Codes that are in DDM but not in quest.
setdiff(hc_ddm_codes, hc_quest_codes) |> 
  sort()
# [1] "al_an_1996_06_03_205_f" "al_an_1998_01_26_923_f" "al_vi_1995_09_29_298_f"
# [4] "an_me_2001_08_15_795_f" "an_ol_2000_03_06_615_f" "ar_cr_1996_08_05_738_f"
# [7] "ca_bu_1980_05_03_267_f" "ch_gr_1992_09_24_323_f" "ch_pr_1997_05_06_654_f"
# [10] "co_mi_2001_06_21_110_f" "cr_ci_1999_08_21_931_f" "fe_ba_1993_05_15_800_m"
# [13] "fr_ba_1957_10_12_166_m" "fr_ba_1997_10_29_663_f" "fr_da_1994_04_19_591_f"
# [16] "fr_di_2001_07_01_522_f" "fr_pl_2002_02_14_755_f" "ga_lu_2000_03_17_328_f"
# [19] "gi_fi_1996_03_09_339_f" "gi_ge_1992_09_10_458_f" "gi_ma_1998_10_27_642_f"
# [22] "ir_bo_1995_04_03_290_f" "ir_da_1999_01_23_879_f" "ka_mo_1967_10_02_556_f"
# [25] "li_va_2001_05_29_471_m" "lu_le_2001_07_04_283_f" "ma_ro_1996_10_30_794_f"
# [28] "ma_sc_1998_07_04_531_f" "na_ge_1996_05_29_070_f" "ni_bi_1996_12_16_648_f"
# [31] "pa_pe_1994_12_31_482_f" "ro_pi_2000_09_30_791_f" "si_sc_1992_12_23_119_f"
# [34] "so_ia_2000_03_21_569_f" "so_ta_1996_08_06_280_f" "su_or_1993_01_13_765_f"


# Final quest data.

saveRDS(
  quest_dat, 
  here::here(
    "data", "processed", "quest", "quest_with_rlddm_params", 
    "cleaned_quest_data.RDS"
  )
)

# eof ---

