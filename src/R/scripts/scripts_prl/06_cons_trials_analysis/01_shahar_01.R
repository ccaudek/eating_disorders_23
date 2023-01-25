# Script name: 01_shahar_01.R
# Project: ED Montecatini
# Script purpose: consecutive trial analysis
# @author: Corrado Caudek <corrado.caudek@unifi.it>
# Date Created: Fri Dec  2 06:14:18 2022
# Last Modified Date: Sab 17 Dic 2022 09:28:14 CET
#
# 👉 Replicate the consecutive trial analysis of Shahar et al. (2019).

# Prelims
suppressPackageStartupMessages({
  library("here")
  library("tidyverse")
  library("stringi")
  library("readxl")
  library("brms")
  library("marginaleffects")
  library("insight")
  library("see")
  library("bayestestR")
})

# Increase max print
options(max.print = .Machine$integer.max)

source(here::here("src", "R", "functions", "funs_prl.R"))
source(here::here("src", "R", "functions", "funs_consecutive_trial_analysis.R"))


# Read list of subjects used in the hDDMrl analysis.
lookup_tbl <- rio::import(
  here::here("data", "processed", "prl", "hddm_look_up_table_v3.csv")
)

# Read complete raw data.
d1 <- gen_data_for_consecutive_trial_analysis()

d <- d1 |>
  dplyr::rename(
    subj_code = subj_name
  )

length(unique(d$subj_code))
summary(factor(d$diag_cat))
setdiff(unique(d$subj_code), unique(lookup_tbl$subj_code))

# Recode feedback
table(d$feedback)
#     0     1 
# 40484 48316 

d |> 
  group_by(diag_cat) |> 
  summarize(
    n_distinct(subj_code)
  )
# 1 AN                            37
# 2 AN_R                          10
# 3 BN                            12
# 4 BN_R                           5
# 5 HC                           212
# 6 RI                            26


# Add lead variables ------------------------------------------------------

# Test.
# foo <- tibble(
#   idx = rep(c("a", "b"), each = 10),
#   blk = rep(1:4, each = 5),
#   x = c(1:5, 1:5, 1:5, 1:5)
# )
# 
# foo <- foo |>
#   group_by(idx, blk) |>
#   mutate(
#     x_lag = lag(x)
#   )


# Is mapping same ---------------------------------------------------------

# Same mapping:
# 1 (same): target position in i trial == target position in i-1 trial
# 0 (different): target position in i trial != target position in i-1 trial

# prev_position_target_img
d <- d |>
  group_by(subj_code, stimulus_type) |> 
  mutate(
    prev_position_target_img = lag(position_target_img)
)
# foo <- tibble(
#   position_target_img = df$position_target_img,  
#   prev_position_target_img = df$prev_position_target_img
# )

d <- d |> 
  group_by(subj_code, stimulus_type) |> 
  mutate(
    is_mapping_same = ifelse(
      position_target_img == prev_position_target_img, 1, 0
    )
  )
# table(df$is_mapping_same)
# foo <- tibble(
#   position_target_img = df$position_target_img,
#   prev_position_target_img = df$prev_position_target_img,
#   is_mapping_same = df$is_mapping_same
# )


# Stay (image) ------------------------------------------------------------

# Which image has been chosen?
# In epoch 1, let's call A the target image.
# If is_target_rewarded_in_present_epoch == 1 and is_target_img_chosen == 1,
# then participant has chosen the image A.
# If is_target_rewarded_in_present_epoch == 0 and is_target_img_chosen == 1,
# then participant has chosen the image B.

d <- d |> 
  group_by(subj_code) |> 
  mutate(
    is_img_a_chosen = case_when(
      is_target_rewared_in_present_epoch == 1 & is_target_img_chosen == 1 ~ "A",
      is_target_rewared_in_present_epoch == 0 & is_target_img_chosen == 1 ~ "B",
      is_target_rewared_in_present_epoch == 1 & is_target_img_chosen == 0 ~ "B",
      is_target_rewared_in_present_epoch == 0 & is_target_img_chosen == 0 ~ "A",
      TRUE ~ NA_character_
    )
  )
# summary(factor(d1$is_img_a_chosen))

# previous image chosen
d <- d |> 
  group_by(subj_code, stimulus_type) |> 
  mutate(
    prev_is_img_a_chosen = lag(is_img_a_chosen)
  )

# prev_feedback
d <- d |>
  group_by(subj_code, stimulus_type) |> 
  mutate(
    prev_feedback = lag(feedback)
  )
# foo <- tibble(
#   feedback = df$feedback,
#   prev_feedback = df$prev_feedback
# )

# Define stay
d <- d |>
  group_by(subj_code, diag_cat, stimulus_type, prev_feedback) |> 
  mutate(
    stay = ifelse(
      is_img_a_chosen == prev_is_img_a_chosen, 1, 0
    )
  )

# Mean stay
bysubj_df <- d |> 
  group_by(
    subj_code, diag_cat, stimulus_type, prev_feedback, is_mapping_same
    ) |> 
  summarize(
    stay = mean(stay, na.rm = TRUE),
    n = n()
  ) |> 
  ungroup()

bysubj_df <- bysubj_df[!is.na(bysubj_df$is_mapping_same), ]

bysubj_df |> 
  group_by(diag_cat, stimulus_type, prev_feedback, is_mapping_same) |> 
  summarize(
    stay = mean(stay, na.rm = TRUE),
    n = n()
  ) |> 
  as.data.frame()

# There is no difference in the interaction is_mapping_same x prev_feedback as
# a function of stimulus_type, for the ED patients. So, I collapse the 'food'
# and 'neutral' conditions.

# I consider only the HC, AN, and BN groups.
bysubj_ed <- bysubj_df[bysubj_df$diag_cat %in% c("AN", "HC", "BN"), ]

# bysubj_ed$diag_cat <- ifelse(bysubj_ed$diag_cat == "HC", "HC", "ED")
# bysubj_ed$diag_cat <- factor(bysubj_ed$diag_cat)
# summary(bysubj_ed$diag_cat)

bysubj_ed %>%
  group_by(is_mapping_same, prev_feedback) %>%
  summarise(
    m = mean(stay)
  )
#   is_mapping_same prev_feedback     m
# 1               0             0 0.471
# 2               0             1 0.690
# 3               1             0 0.369
# 4               1             1 0.795

0.690 - 0.471
# [1] 0.219
0.795 - 0.369
# [1] 0.426

# Rename and recode variables.
bysubj_ed <- bysubj_ed |> 
  dplyr::rename(
    feedback = prev_feedback,
    mapping = is_mapping_same,
    image = stimulus_type,
    diagnosis = diag_cat)

bysubj_ed$feedback <- factor(bysubj_ed$feedback)
bysubj_ed$feedback <- bysubj_ed$feedback |> 
  forcats::fct_recode(
    "punishment" = "0",
    "reward" = "1"
  )

bysubj_ed$mapping <- factor(bysubj_ed$mapping)
bysubj_ed$mapping <- bysubj_ed$mapping |> 
  forcats::fct_recode(
    "flipped" = "0",
    "same" = "1"
  )

bysubj_ed$image <- factor(bysubj_ed$image)
bysubj_ed$diagnosis <- factor(bysubj_ed$diagnosis)
bysubj_ed$diagnosis <- relevel(bysubj_ed$diagnosis, ref = "HC")
contrasts(bysubj_ed$diagnosis)

summary(bysubj_ed$diagnosis)


# Three-way interaction ---------------------------------------------------

bcpriors <- get_prior(
  stay ~ 0 + Intercept + mapping * feedback * diagnosis +
    (1 + mapping * feedback | subj_code),
  family = zero_one_inflated_beta(),
  data = bysubj_ed
)

priors_0 <- c(
  set_prior("student_t(3, 0, 0.2)", class = "b", coef = "Intercept"),
  set_prior("student_t(3, 0, 0.2)", class = "b"),
  set_prior("student_t(3, 0, 0.2)", class = "sd"),
  set_prior("lkj(1)", class = "cor"),
  set_prior("gamma(0.01, 0.01)", class = "phi"),
  set_prior("beta(2, 2)", class = "coi"),
  set_prior("beta(2, 2)", class = "zoi")
)

mod_0 <- brm(
  stay ~ 0 + Intercept + mapping * feedback * diagnosis + 
    (1 + mapping * feedback | subj_code),
  family = zero_one_inflated_beta(),
  backend = "cmdstanr",
  data = bysubj_ed,
  prior = priors_0,
  iter = 2000
)
mod_0 <- add_criterion(mod_0, "loo")

pp_check(mod_0)
summary(mod_0)
#                                        Estimate Est.Error l-95% CI u-95% CI Rhat
# Intercept                                 -0.13      0.04    -0.21    -0.05 1.00
# mappingsame                               -0.42      0.06    -0.53    -0.31 1.00
# feedbackreward                             1.02      0.09     0.85     1.19 1.00
# diagnosisAN                                0.06      0.09    -0.11     0.23 1.00
# diagnosisBN                                0.06      0.12    -0.17     0.29 1.00
# mappingsame:feedbackreward                 0.90      0.07     0.77     1.03 1.00
# mappingsame:diagnosisAN                   -0.06      0.11    -0.29     0.16 1.00
# mappingsame:diagnosisBN                   -0.04      0.15    -0.35     0.26 1.00
# feedbackreward:diagnosisAN                -0.16      0.16    -0.50     0.13 1.00
# feedbackreward:diagnosisBN                 0.04      0.19    -0.31     0.43 1.00
# mappingsame:feedbackreward:diagnosisAN     0.14      0.13    -0.10     0.41 1.00
# mappingsame:feedbackreward:diagnosisBN     0.03      0.17    -0.29     0.36 1.00

dcat <- make_conditions(mod_0, "diagnosis")
me <- conditional_effects(mod_0, "mapping:feedback", conditions = dcat)
## change colours to grey scale
plot(me, plot = FALSE)[[1]] +
  see::scale_colour_okabeito(order = c(1, 5)) +
  labs(
    x = "Mapping",
    y = "Stay"
  ) +
  # bayesplot::theme_default(base_family = "sans") +
  papaja::theme_apa() +
  theme(legend.position="bottom") 

# This is the plot for the supplementary material.
ggsave("shahar_01.pdf", width = 21, height = 11, units = "cm")

plot(mod_0, variable = "^b_mappingsame:feedbackreward", regex = TRUE)


# Two-way interaction only ------------------------------------------------

# There is no 3-way interaction with 'diagnosis', so I simplify the model.
# The figure replicates Fig. 2C of Shahar et al. (2019).

bcpriors <- get_prior(
  stay ~ 0 + Intercept + mapping * feedback +
    (1 + mapping * feedback | subj_code),
  family = zero_one_inflated_beta(),
  data = bysubj_ed
)

priors_1 <- c(
  set_prior("student_t(3, 0, 0.2)", class = "b", coef = "Intercept"),
  set_prior("student_t(3, 0, 0.2)", class = "b"),
  set_prior("student_t(3, 0, 0.2)", class = "sd"),
  set_prior("lkj(1)", class = "cor"),
  set_prior("gamma(0.01, 0.01)", class = "phi"),
  set_prior("beta(2, 2)", class = "coi"),
  set_prior("beta(2, 2)", class = "zoi")
)

mod_1 <- brm(
  stay ~ 0 + Intercept + mapping * feedback +
    (1 + mapping * feedback | subj_code),
  family = zero_one_inflated_beta(),
  backend = "cmdstanr",
  data = bysubj_ed,
  prior = priors_1,
  iter = 2000,
  save_pars = save_pars(all = TRUE)
)
mod_1 <- add_criterion(mod_1, "loo")

me <- conditional_effects(mod_1, "mapping:feedback")
plot(me, plot = FALSE)[[1]] +
  see::scale_colour_okabeito(order = c(1, 5)) +
  labs(
    x = "Mapping",
    y = "Stay"
  ) +
  #bayesplot::theme_default(base_family = "sans") +
  papaja::theme_apa() +
  theme(legend.position="bottom") 

ggsave("shahar_02.pdf", width = 9.5, height = 11, units = "cm")


summary(mod_1)
#                            Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
# Intercept                     -0.12      0.04    -0.20    -0.04 1.00     2098     2444
# mappingsame                   -0.43      0.05    -0.53    -0.33 1.00     2595     3097
# feedbackreward                 1.00      0.08     0.84     1.15 1.00     2124     2504
# mappingsame:feedbackreward     0.92      0.07     0.79     1.05 1.00     3471     3095

# Bayes Factor.
mod_bf = bayestestR::bayesfactor_parameters(mod_1)
mod_bf
# Parameter                  |       BF
# -------------------------------------
# (Intercept)                |    14.10
# mappingsame                | 9.88e+06
# feedbackreward             | 5.97e+12
# mappingsame:feedbackreward | 7.97e+14
plot(mod_bf)

plot(bayesfactor_models(mod_1)) +
  scale_fill_pizza()

# Interaction term:
effectsize::interpret_bf(mod_bf$log_BF[4], include_value = TRUE)
# [1] "extreme evidence (BF = 7.97e+14) in favour of"
# (Rules: jeffreys1961)

rope = bayestestR::bayesfactor_parameters(mod_1, null = c(-0.0132, 0.0132))
rope
# Parameter                  |       BF
# -------------------------------------
# (Intercept)                |    12.75
# mappingsame                | 1.06e+07
# feedbackreward             | 6.89e+12
# mappingsame:feedbackreward | 7.95e+14
plot(rope)
effectsize::interpret_bf(exp(mod_bf$log_BF[4]), include_value = TRUE)
# [1] "extreme evidence (BF = 7.99e+14) in favour of"

bayes_R2(mod_1)
#    Estimate  Est.Error     Q2.5     Q97.5
# R2 0.713901 0.01001142 0.693518 0.7324759

bayestestR::hdi(mod_1)
# Parameter                  |        95% HDI
# -------------------------------------------
# (Intercept)                | [-0.19, -0.04]
# mappingsame                | [-0.54, -0.32]
# feedbackreward             | [ 0.84,  1.15]
# mappingsame:feedbackreward | [ 0.79,  1.05]

hypothesis(mod_1, "mappingsame:feedbackreward > 0")
#                 Hypothesis Estimate Est.Error CI.Lower CI.Upper Evid.Ratio Post.Prob
# 1 (mappingsame:feed... > 0     0.92      0.07     0.81     1.03        Inf         1

bayestestR::describe_posterior(mod_1)
# Parameter                  | Median |         95% CI |     pd |          ROPE | % in ROPE |  Rhat |     ESS
# -----------------------------------------------------------------------------------------------------------
# (Intercept)                |  -0.12 | [-0.20, -0.04] | 99.88% | [-0.10, 0.10] |    32.74% | 1.000 | 2036.00
# mappingsame                |  -0.43 | [-0.54, -0.32] |   100% | [-0.10, 0.10] |        0% | 1.001 | 2485.00
# feedbackreward             |   0.99 | [ 0.84,  1.15] |   100% | [-0.10, 0.10] |        0% | 1.002 | 1839.00
# mappingsame:feedbackreward |   0.92 | [ 0.79,  1.05] |   100% | [-0.10, 0.10] |        0% | 1.001 | 2801.00

# Model comparison --------------------------------------------------------

loo_compare(mod_0, mod_1, criterion = "loo")
#     elpd_diff se_diff
# mod_0   0.0       0.0  
# mod_1 -35.3      25.6  




# eof ---









stan_m1 <- stan_glmer(stay ~ mapping * feedback * diagnosis + 
                      (1 + mapping * feedback | subj_code),
                    data = temp,
                    family = gaussian(),
                    diagnostic_file = file.path(tempdir(), "df1.csv")
)
stan_m2 <- stan_glm(stay ~ Species + Petal.Length,
                    data = temp,
                    family = gaussian(),
                    diagnostic_file = file.path(tempdir(), "df2.csv")
)
bayestestR::bayesfactor_models(stan_m1, stan_m2, denominator = stan_m1)




(BFM <- bayestestR::bayesfactor_models(mod_0, mod_1, denominator = 1))

bayes_factor(mod_0, mod_1)








#' For the 2-way interaction above, when considering only the AN group, there is
#' no interesting difference when is_target_img_chosen == 1 or == 0.

# Mean stay
bysubj_df <- d |> 
  group_by(subj_code, diag_cat, stimulus_type, prev_feedback, is_mapping_same,
           is_target_img_chosen) |> 
  summarize(
    stay = mean(stay, na.rm = TRUE),
    n = n()
  ) |> 
  ungroup()

bysubj_df <- bysubj_df[!is.na(bysubj_df$is_mapping_same), ]

bysubj_df |> 
  group_by(diag_cat, stimulus_type, prev_feedback, is_mapping_same, 
           is_target_img_chosen) |> 
  summarize(
    stay = mean(stay, na.rm = TRUE),
    n = n()
  ) |> 
  as.data.frame()

# There is no difference in the interaction is_mapping_same x prev_feedback as
# a function of stimulus_type, for the ED patients. So, I collapse the 'food'
# and 'neutral' conditions.

# I consider only the HC, AN, and BN groups.
bysubj_ed <- bysubj_df[bysubj_df$diag_cat %in% c("AN", "HC", "BN"), ]

# bysubj_ed$diag_cat <- ifelse(bysubj_ed$diag_cat == "HC", "HC", "ED")
# bysubj_ed$diag_cat <- factor(bysubj_ed$diag_cat)
# summary(bysubj_ed$diag_cat)

bysubj_ed %>%
  group_by(diag_cat, is_mapping_same, prev_feedback) %>%
  summarise(
    m = mean(stay)
  )

# Rename and recode variables.
bysubj_ed <- bysubj_ed |> 
  dplyr::rename(
    feedback = prev_feedback,
    mapping = is_mapping_same,
    image = stimulus_type,
    diagnosis = diag_cat,
    target_chosen = is_target_img_chosen)


bysubj_ed$feedback <- factor(bysubj_ed$feedback)
bysubj_ed$feedback <- bysubj_ed$feedback |> 
  forcats::fct_recode(
    "punishment" = "0",
    "reward" = "1"
  )

bysubj_ed$mapping <- factor(bysubj_ed$mapping)
bysubj_ed$mapping <- bysubj_ed$mapping |> 
  forcats::fct_recode(
    "flipped" = "0",
    "same" = "1"
  )

bysubj_ed$image <- factor(bysubj_ed$image)
bysubj_ed$diagnosis <- factor(bysubj_ed$diagnosis)
summary(bysubj_ed$diagnosis)










# Same 2-way interaction as above, for the target-choices only.
patients_only_target_choices_df <- bysubj_ed |> 
  dplyr::filter(target_chosen == 0 & diagnosis == "AN")

bcpriors <- get_prior(
  stay ~ mapping * feedback +
    (1 + mapping * feedback | subj_code),
  family = skew_normal(),
  data = patients_only_target_choices_df
)

mod_2 <- brm(
  stay ~ mapping * feedback +
    (1 + mapping * feedback | subj_code),
  family = skew_normal(),
  backend = "cmdstanr",
  data = patients_only_target_choices_df,
  prior = bcpriors
)

conditional_effects(mod_2, "mapping:feedback")



plot(mod_2, variable = "^b_mappingsame:feedbackreward", regex = TRUE)

