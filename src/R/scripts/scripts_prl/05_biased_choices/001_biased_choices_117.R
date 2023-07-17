# Script name: 001_biased_choices_117.R
# Project: ED Montecatini
# Script purpose: analysis of is_target_img_chosen as a function of group.
# @author: Corrado Caudek <corrado.caudek@unifi.it>
# Date Created: Wed Dec  7 09:20:42 2022
# Last Modified Date: Mon Jul 10 08:46:31 2023
#
# 👉 I ask whether the marginal selection of the target image (regardless of 
# reward history) differs between patients and controls.

library("here")
library("tidyverse")
library("brms")
library("emmeans")
library("BayesFactor")
library("bayestestR")
library("insight")
library("see")

source(here::here("src", "R", "functions", "funs_prl.R"))
source(here::here("src", "R", "functions", "funs_consecutive_trial_analysis.R"))

# Read complete raw data, including is_target_img_chosen.
d <- readRDS(
  here::here("data", "processed", "prl", "raw_prl_data", "prl_tot_raw_data.rds")
  ) |> 
  dplyr::select(-diag_cat) |> 
  dplyr::rename(
    subj_code = subj_name
  )

# Read list of 117 subjects used in the final analysis, including diag_cat.
temp <- rio::import(
  here::here(
    "data", "processed", "prl", "input_for_hddmrl", "three_groups",
    "ed_prl_data.csv"
  )
) |> 
  select(subj_code, diag_cat) |> 
  distinct()

dat <- left_join(temp, d, by = "subj_code")
length(unique(df$subj_code))







d1 <- gen_data_for_consecutive_trial_analysis() |>
  dplyr::rename(
    subj_code = subj_name
  ) |> 
  dplyr::select(
    -diag_cat
  )



dat |> 
  group_by(diag_cat) |> 
  summarize(
    n = n_distinct(subj_code)
  )
#   diag_cat     n
# 1 AN          36
# 2 HC          45
# 3 RI          36

# Examine the proportions of is_target_img_chosen in the 3 groups.

dat$diag_cat <- factor(dat$diag_cat)
dat$diag_cat <- relevel(dat$diag_cat, ref = "AN")
contrasts(dat$diag_cat)

# Is target image chosen
dat$trg_chosen <- dat$is_target_img_chosen
unique(dat$trg_chosen)
# [1] 0 1

# Subject id.
dat$ID <- factor(dat$subj_code)
length(unique(dat$ID))
# [1] 117

# RTs in sec.
dat <- dat |> 
  mutate(
    rt = rt / 1000
  ) |> 
  dplyr::filter(rt > 0.2 & stimulus_type == "food")


# By-subject data ---

# Computer one proportion for each condition/subject.
bysubj_freq <- dat |> 
  group_by(diag_cat, subj_code) |> 
  summarize(
    y = mean(trg_chosen)
  ) |> 
  ungroup()


# bysubj_freq$flag <- ifelse(
#   (bysubj_freq$di == "HC") & (bysubj_freq$y < 0.3), NA, bysubj_freq$y
# )

bysubj_freq <- bysubj_freq[complete.cases(bysubj_freq), ] 
bysubj_freq$flag <- NULL

hist(bysubj_freq$y)


# The proportion of times the target image was chosen, for each group 
bysubj_freq |> 
  group_by(diag_cat) |> 
  summarise(
    m = mean(y, na.rm = TRUE),
    std = sd(y, na.rm = TRUE)
  ) 
# diag_cat     m    std
# 1 AN       0.450 0.133 
# 2 HC       0.479 0.0919
# 3 RI       0.471 0.0706

# The proportions were analyzed with a robust Bayesian regression model 
# (a Beta model produced similar results).

priors <- c(
  set_prior("student_t(4, 0, 1.0)", class = "b")
)

bmod <- brm(
  bf(
    y ~ diag_cat, 
    sigma ~ diag_cat
  ),
  data = bysubj_freq,
  family = student(),
  control = list(adapt_delta = 0.99),
  prior = priors,
  # backend = "cmdstan",
  warmup = 1000,
  iter = 5000,
  cores = parallel::detectCores(),
  seed = "12345",
  chains = 4,
  refresh = 0,
  silent = TRUE
)

pp_check(bmod)
summary(bmod)
#                  Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
# Intercept            0.49      0.01     0.46     0.51 1.00     9991     9042
# sigma_Intercept     -2.91      0.21    -3.32    -2.49 1.00     8602     9738
# diag_catHC           0.01      0.02    -0.02     0.04 1.00    10844    10357
# diag_catRI          -0.01      0.02    -0.04     0.02 1.00    10939    10424
# sigma_diag_catHC    -0.03      0.24    -0.51     0.45 1.00    11112    11052
# sigma_diag_catRI    -0.03      0.26    -0.54     0.47 1.00    10652    10774


emm <- emmeans(bmod, "diag_cat")
contrasts <- contrast(
  emm, 
  method = "pairwise", 
  adjust = "none", 
  ref = "contr.sum", 
  return_df = TRUE
)
contrasts
# contrast estimate lower.HPD upper.HPD
# AN - HC  -0.00723   -0.0374    0.0236
# AN - RI   0.01305   -0.0187    0.0462
# HC - RI   0.02003   -0.0108    0.0493
# 
# Point estimate displayed: median 
# HPD interval probability: 0.95 
