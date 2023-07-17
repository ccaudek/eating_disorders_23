# Relation between MPS perfectionism scale and HDDMrl params

library(tidyverse)
library(here)
library(rio)
library(mice)
library(brms)


# Read quest data
quest_data <- rio::import(
  here::here(
    "data", "processed", "quest", "quest3grp.csv"
  )
) |> 
  dplyr::select(-diag_cat)

# Read hddmrl params
params_data <- rio::import(
  here::here(
    "data", "processed", "prl", "subj_hddm_params", "subj_code_hddm_params.csv"
  )
)

mps_df <- quest_data |> 
  dplyr::select(subj_code, mps_cmd, mps_ps, mps_pepc, mps_or, mps_tot) |> 
  distinct()

params_wide_data <- params_data |> 
  dplyr::select(-c(params, subj_idx)) |> 
  pivot_wider(names_from = par, values_from = val)

d <- left_join(params_wide_data, mps_df, by = "subj_code")
length(unique(d$subj_code))

d$stim <- factor(d$stim)

result <- d %>%
  group_by(subj_code, diag_cat) %>%
  summarize(diff_pos_alpha = if (n_distinct(stim) == 2) {
    pos_alpha[stim == "neutral"] - pos_alpha[stim == "food"]
  } else {
    NA
  }) 

d1 <- left_join(result, mps_df, by = "subj_code")
length(unique(d$subj_code))

d1$diff_pos_alpha <- scale(d1$diff_pos_alpha) |> 
  as.numeric()
d1$mps_cmd <- scale(d1$mps_cmd) |> 
  as.numeric()
d1$mps_ps <- scale(d1$mps_ps) |> 
  as.numeric()
d1$mps_pepc <- scale(d1$mps_pepc) |> 
  as.numeric()
d1$mps_or <- scale(d1$mps_or) |> 
  as.numeric()
d1$mps_tot <- scale(d1$mps_tot) |> 
  as.numeric()

imputed_data <- mice(d1, m = 1, maxit = 0, method = "deterministic")
d2 <- complete(imputed_data)

hist(d2$diff_pos_alpha)

fm <- brm(
  diff_pos_alpha ~ mps_cmd + mps_ps + mps_pepc + mps_or,
  d2
)
summary(fm)
pp_check(fm)
bayes_R2(fm)
conditional_effects(fm, "bsq14_tot")

fm <- brm(
  diff_pos_alpha ~ diag_cat*mps_tot,
  d2
)
pp_check(fm)
summary(fm)
bayes_R2(fm)
conditional_effects(fm, "mps_tot:diag_cat")



fm <- brm(
  pos_alpha ~ stim * (mps_cmd + mps_ps + mps_pepc + mps_or) +
    (stim | subj_code),
  d
)


# For Montecatini ----

out <- d2 |> 
  dplyr::filter(diag_cat == "AN") |> 
  dplyr::select(subj_code, diff_pos_alpha) 

out <- out[order(out$diff_pos_alpha), ]

an_df <- d |> 
  dplyr::filter(diag_cat == "AN") |> 
  dplyr::select(-starts_with("mps_"))

temp <- left_join(an_df, out, by = "subj_code")
temp <- temp[order(temp$diff_pos_alpha), ]
temp$diff_pos_alpha <- NULL
temp$diag_cat <- NULL

temp <- temp %>% 
  mutate(across(where(is.numeric), ~ round(., 2)))
