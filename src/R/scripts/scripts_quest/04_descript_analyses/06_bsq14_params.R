


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

bsq_df <- quest_data |> 
  dplyr::select(subj_code, bsq14_tot) |> 
  distinct()

params_wide_data <- params_data |> 
  dplyr::select(-c(params, subj_idx)) |> 
  pivot_wider(names_from = par, values_from = val)

d <- left_join(params_wide_data, bsq_df, by = "subj_code")
length(unique(d$subj_code))

d$stim <- factor(d$stim)

result <- d %>%
  group_by(subj_code, diag_cat) %>%
  summarize(diff_pos_alpha = if (n_distinct(stim) == 2) {
    pos_alpha[stim == "food"] - pos_alpha[stim == "neutral"]
  } else {
    NA
  }) 

d1 <- left_join(result, bsq_df, by = "subj_code")
length(unique(d$subj_code))

d1$diff_pos_alpha <- scale(d1$diff_pos_alpha) |> 
  as.numeric()
d1$bsq14_tot <- scale(d1$bsq14_tot) |> 
  as.numeric()

imputed_data <- mice(d1, m = 1, maxit = 0, method = "deterministic")
d2 <- complete(imputed_data)

hist(d2$diff_pos_alpha)

fm <- brm(
  diff_pos_alpha ~ bsq14_tot,
  d2,
  backend="cmdstanr"
)
summary(fm)
pp_check(fm)
bayes_R2(fm)
conditional_effects(fm, "bsq14_tot")

m1 <- brm(
  pos_alpha ~ bsq14_tot * stim,
  d,
  backend="cmdstanr"
)
pp_check(m1)
summary(m1)
bayes_R2(m1)
conditional_effects(m1, "bsq14_tot")

