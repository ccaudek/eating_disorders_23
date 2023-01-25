

ddm_dat <- rio::import(
  here::here("data", "processed", "prl", "input_for_hddmrl", "hddm_input_v3.csv")
)

ddm_codes <- unique(ddm_dat$subj_code)

quest_dat <- readRDS(
  here::here("data", "processed", "quest", "quest_diagn_data.rds")
)

quest_codes <- unique(thedat$subj_code)

foo <- ddm_dat[ddm_dat$subj_code %in% quest_codes, ]
length(unique(foo$subj_code))

glimpse(thedat)

# patiens codes in quest
pat_quest_dat <- quest_dat |> 
  dplyr::filter(diagnosis == "AN" | diagnosis == "BN")

pat_quest_codes <- sort(unique(pat_quest_dat$subj_code))

length(intersect(ddm_codes, pat_quest_codes))

# Pazienti che hanno fatto i questionari ma che non risultano nel DDMRL
setdiff(pat_quest_codes, ddm_codes)
# [1] "al_ro_1989_04_25_160_f" "be_ma_1999_06_15_331_f" "ca_pa_2002_04_05_939_f"
# [4] "cl_co_1996_03_16_886_f" "cr_gi_1994_10_14_347_f" "gi_lu_1992_06_10_601_f"
# [7] "ju_yo_1998_05_28_316_f" "ma_la_1990_03_17_728_f"




