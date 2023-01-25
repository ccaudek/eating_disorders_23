
library("here")
library("tidyverse")
library("stringr")
library("brms")
library("semTools")


# Read RLDDM params.
param_dat <- readRDS(
  here::here(
    "data", "processed", "quest", "quest_with_rlddm_params", 
    "rlddm_params.RDS"
  )
)


# Read quest data.
quest_dat <- readRDS(
  here::here(
    "data", "processed", "quest", "quest_with_rlddm_params", 
    "cleaned_quest_data.RDS"
  )
)

param_codes <- unique(param_dat$subj_code)
length(param_codes)
# [1] 302
quest_codes <- unique(quest_dat$subj_code)
length(quest_codes)
# [1] 333

length(intersect(param_codes, quest_codes))

quest_param_df <- inner_join(
  quest_dat, param_dat, by = "subj_code"
)

length(unique(quest_param_df$subj_code))

quest_param_df <- mutate_if(quest_param_df, is.character, as.factor)

quest_param_df |> 
  group_by(diag_cat, stim) |> 
  summarize(
    n = n()
  )

hc_only <- quest_param_df |> 
  dplyr::filter(diag_cat == "HC")
length(unique(hc_only$subj_code))

contrasts(quest_param_df$diag_cat)
#      RI AN BN BN_R AN_R
# HC    0  0  0    0    0
# RI    1  0  0    0    0
# AN    0  1  0    0    0
# BN    0  0  1    0    0
# BN_R  0  0  0    1    0
# AN_R  0  0  0    0    1

summary(quest_param_df)

temp <- quest_param_df %>% 
  dplyr:: select(subj_code, starts_with("ros_"))

rosenberg <- distinct(temp, subj_code, .keep_all = TRUE)
rosenberg <- rosenberg |> 
  select(-c(subj_code, ros_tot))

model <- '
  f  =~ ros_1 + ros_2 + ros_3 + ros_4 + ros_5 + ros_6 + ros_7 + 
        ros_8 + ros_9 + ros_10
'
fit <- cfa(model, data = rosenberg, ordered = TRUE, std.lv = TRUE)

semTools::reliability(fit)
#           f
# alpha     0.9167888
# alpha.ord 0.9401014
# omega     0.9269497
# omega2    0.9269497
# omega3    0.9503552
# avevar    0.6442613

hist(quest_param_df$ros_tot)



# Rosenberg scale ---------------------------------------------------------

ros_df <- quest_param_df |> 
  dplyr::select(
    ros_tot, diag_cat, subj_code
  )
ros_df <- distinct(ros_df, subj_code, .keep_all = TRUE)


m1 <- brm(
  ros_tot ~ diag_cat,
  data = ros_df, 
  # prior = prior_ma,
  family = gaussian(),
  # control = list(adapt_delta = 0.98),
  iter = 4000,
  cores = 4,
  backend = "cmdstan"
)
pp_check(m1)
summary(m1)
#              Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
# Intercept       28.79      0.41    28.00    29.59 1.00    11517     5712
# diag_catRI      -5.78      1.17    -8.04    -3.47 1.00    11672     6517
# diag_catAN      -6.37      1.01    -8.32    -4.41 1.00    10561     6060
# diag_catBN      -7.86      1.72   -11.15    -4.51 1.00    11573     6843
# diag_catBN_R    -4.20      2.50    -9.10     0.60 1.00    12668     5983
# diag_catAN_R    -6.98      1.83   -10.63    -3.40 1.00    11370     6014

ros1_df <- quest_param_df[
  !(quest_param_df$diag_cat == "HC" | quest_param_df$diag_cat == "RI"), ]

ros1_df$diag_cat <- factor(ros1_df$diag_cat)
contrasts(ros1_df$stim)

ros1_df$stim_num <- ifelse(ros1_df$stim == "food", 0.5, -0.5)

ros_an_df <- quest_param_df[quest_param_df$diag_cat == "AN", ]
hist(ros1_df$alpha_pos)


bf_alpha_p <- bf(alpha_pos ~ ros_tot + (1 | subj_code)) + 
  gaussian()
bf_alpha_n <- bf(alpha_neg ~ ros_tot + (1 | subj_code)) + 
  skew_normal()
bf_v <- bf(v               ~ ros_tot + (1 | subj_code)) + 
  skew_normal()
bf_a <- bf(a               ~ ros_tot + (1 | subj_code)) + 
  skew_normal()
bf_t <- bf(t               ~ ros_tot + (1 | subj_code)) + 
  skew_normal()

hist(ros_an_df$t)


m1a <- brm(
  bf_alpha_p + bf_alpha_n + bf_v + bf_a + bf_t + set_rescor(FALSE),
  data = quest_param_df, # ros_an_df, 
  # prior = prior_ma,
  # control = list(adapt_delta = 0.98),
  iter = 4000,
  cores = 4
  # backend = "cmdstan"
)
pp_check(m1a, resp = 'alphapos')
summary(m1a)
#                    Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
# alphapos_Intercept    -1.82      1.59    -4.97     1.24 1.00     8331     6119
# alphaneg_Intercept     1.43      2.12    -2.79     5.55 1.00     3646     4377
# v_Intercept            1.56      1.26    -0.94     4.01 1.00     2250     3895
# a_Intercept            1.46      0.16     1.14     1.78 1.00     5673     6116
# t_Intercept            0.18      0.04     0.10     0.26 1.00     6876     6102
# alphapos_ros_tot       0.07      0.07    -0.06     0.20 1.00     8195     6154
# alphaneg_ros_tot      -0.02      0.09    -0.20     0.16 1.00     3575     4656
# v_ros_tot              0.01      0.05    -0.09     0.12 1.00     2210     3685
# a_ros_tot             -0.00      0.01    -0.02     0.01 1.00     5790     6279
# t_ros_tot              0.00      0.00    -0.00     0.00 1.00     6503     6302


sjPlot::tab_model(m1a, show.r2 = FALSE, show.obs = FALSE)

conditions <- make_conditions(m1a, "diag_cat")
conditional_effects(m1a, "ros_tot:stim_num", conditions = conditions)


## only include points close to the specified values of zAge
me <- conditional_effects(
  m1a, "zBase:Trt", conditions = conditions,
  select_points = 0.1
)
plot(me, points = TRUE)





