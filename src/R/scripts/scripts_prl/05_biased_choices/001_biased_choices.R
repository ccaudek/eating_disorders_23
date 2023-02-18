# Script name: 001_biased_choices.R
# Project: ED Montecatini
# Script purpose: analysis of is_target_img_chosen.
# @author: Corrado Caudek <corrado.caudek@unifi.it>
# Date Created: Wed Dec  7 09:20:42 2022
# Last Modified Date: Wed Dec  7 09:20:42 2022
#
# 👉 I ask whether the marginal selection of the target image (regardless of 
# reward history) differs between patients and controls.

library("here")
library("tidyverse")
library("brms")
library("tidybayes")
library("scales")
library("ggthemes")
library("emmeans")
library("BayesFactor")
library("bayestestR")
library("crayon")  # for terminal colors
library("patchwork")
library("insight")
library("see")

library("cmdstanr")
cmdstan_version()

library("tidylog") # Load tidylog after dplyr and/or tidyr


# Increase max print
options(max.print = .Machine$integer.max)

source(here::here("src", "R", "functions", "funs_prl.R"))
source(here::here("src", "R", "functions", "funs_consecutive_trial_analysis.R"))


# Read data ---

# Read list of subjects used in the hDDMrl analysis.
lookup_tbl <- rio::import(
  here::here("data", "processed", "prl", "input_for_hddmrl", 
             "hddm_look_up_table_v3.csv")
)

# Read complete raw data.
d1 <- gen_data_for_consecutive_trial_analysis()

d <- d1 |>
  dplyr::rename(
    subj_code = subj_name
  )

d |> 
  group_by(diag_cat) |> 
  summarize(
    n_distinct(subj_code)
  )
# 1 AN      37
# 2 AN_R    10
# 3 BN      12
# 4 BN_R     5
# 5 HC     212
# 6 RI      26

# Keep only 3 diagnostic categories: AN, BN, HC ---

dat <- d |> 
  dplyr::filter(diag_cat == "AN" | diag_cat == "BN" | diag_cat == "HC")

# Examine the proportions of is_target_img_chosen in all groups.
d |> 
  group_by(diag_cat, stimulus_type) %>% 
  summarise(
    m = mean(is_target_img_chosen, na.rm=TRUE),
    n = n_distinct(subj_code)
  )
# diag_cat stimulus_type     m     n
# 1  AN       food          0.447    35
# 2  AN       neutral       0.492    34
# 3  AN_R     food          0.477     9
# 4  AN_R     neutral       0.483    10
# 5  BN       food          0.492    12
# 6  BN       neutral       0.443    10
# 7  BN_R     food          0.49      5
# 8  BN_R     neutral       0.474     5
# 9  HC       food          0.477   199
# 10 HC       neutral       0.476   188
# 11 RI       food          0.471    26
# 12 RI       neutral       0.489    21

# Examine the proportions of is_target_img_chosen in the 3 groups.
dat |> 
  group_by(diag_cat, stimulus_type) %>% 
  summarise(
    m = mean(is_target_img_chosen, na.rm=TRUE),
    n = n_distinct(subj_code)
  )
#   diag_cat stimulus_type     m     n
# 1 AN       food          0.447    35
# 2 AN       neutral       0.492    34
# 3 BN       food          0.492    12
# 4 BN       neutral       0.443    10
# 5 HC       food          0.477   199
# 6 HC       neutral       0.476   188

# Recode variables.

# Diagnostic category -> di (reference: HC)
dat$di <- factor(dat$diag_cat)
dat$di <- relevel(dat$di, ref = "HC")
contrasts(dat$di)

# Stimulus type
dat$st <- factor(dat$stimulus_type)
dat$st <- relevel(dat$st, ref = "neutral")
contrasts(dat$st)

# Is target image chosen
dat$trg_chosen <- dat$is_target_img_chosen
unique(dat$trg_chosen)
# [1] 0 1

# Subject id.
dat$ID <- factor(dat$subj_code)
length(unique(dat$ID))
# [1] 261

# RTs in sec.
dat <- dat |> 
  mutate(
    rt = rt1 / 1000
  ) |> 
  dplyr::filter(rt > 0.2 & stimulus_type == "food")


# By-subject data ---

# Computer one proportion for each condition/subject.
bysubj_prop <- dat |> 
  group_by(di, ID) |> 
  summarize(
    y = mean(trg_chosen)
  )

bysubj_freq$flag <- ifelse(
  (bysubj_freq$di == "HC") & (bysubj_freq$y < 0.3), NA, bysubj_freq$y
)

bysubj_freq <- bysubj_freq[complete.cases(bysubj_freq), ] 
bysubj_freq$flag <- NULL

hist(bysubj_prop$y)

bysubj_prop |> 
  group_by(di) |> 
  summarise(
    m = mean(y, na.rm = TRUE)
  )

priors <- c(
  set_prior("student_t(4, 0, 2.5)", class = "b"),
  set_prior("student_t(4, 0, 2.5)", class = "sigma")
)

bmod_01 <- brm(
  bf(y ~ di, phi ~ di),
  data = bysubj_prop,
  family = Beta(),
  control = list(adapt_delta = 0.99),
  # prior = priors,
  backend = "cmdstan",
  warmup = 1000,
  iter = 5000,
  cores = parallel::detectCores(),
  seed = "12345",
  chains = 4
)
pp_check(bmod_01)
summary(bmod_01)



priors <- c(
  set_prior("student_t(4, 0, 2.5)", class = "b")
)

bmod_02 <- brm(
  bf(y ~ di, sigma ~ di),
  data = bysubj_prop,
  family = student(),
  control = list(adapt_delta = 0.99),
  prior = priors,
  backend = "cmdstan",
  warmup = 1000,
  iter = 5000,
  cores = parallel::detectCores(),
  seed = "12345",
  chains = 4
)
pp_check(bmod_02)
summary(bmod_02)


emmeans::emmeans(bmod_02, ~ di,
                 regrid = "response") %>% 
  contrast(method = "revpairwise")
# contrast estimate lower.HPD upper.HPD
# AN - HC  -0.00126   -0.0277    0.0267
# BN - HC   0.01537   -0.0278    0.0587
# BN - AN   0.01668   -0.0323    0.0661
# 
# Point estimate displayed: median 
# HPD interval probability: 0.95 


options(digits=10)

bmod_00 <- brm(
  bf(y ~ 1),
  data = bysubj_prop,
  family = student(),
  control = list(adapt_delta = 0.99),
  backend = "cmdstan",
  warmup = 1000,
  iter = 5000,
  cores = parallel::detectCores(),
  seed = "12345",
  chains = 4
)
pp_check(bmod_00)
summary(bmod_00)

print(bmod_00, digits = 3)
#           Estimate Est.Error l-95% CI u-95% CI  Rhat Bulk_ESS Tail_ESS
# Intercept    0.484     0.004    0.477    0.492 1.001    10486     9954







# eof ---






























bysubj_wide <- bysubj_freq %>%
  pivot_wider(names_from = st, values_from = y)

# Positive values indicate preference for neutral over food stimuli.
bysubj_wide <- bysubj_wide |> 
  mutate(delta = neutral - food)

bysubj_wide |> 
  group_by(di) |> 
  summarize(
    delta_choices = mean(delta, na.rm=T),
    se_delta = sd(delta, na.rm=T) / sqrt(n()),
    n = n()
  )


hist(bysubj_wide$delta)

# Standardize
bysubj_wide$delta_st <- 
  bysubj_wide$delta - mean(bysubj_wide$delta, na.rm = TRUE) /
  sd(bysubj_wide$delta, na.rm = TRUE)


priors <- c(
  set_prior("student_t(4, 0, 2.5)", class = "b"),
  set_prior("student_t(4, 0, 2.5)", class = "sigma")
)

bmod_01 <- brm(
  bf(delta_st ~ 0 + di),
  data = bysubj_wide,
  family = student(),
  control = list(adapt_delta = 0.99),
  prior = priors,
  backend = "cmdstan",
  warmup = 1000,
  iter = 20000,
  cores = parallel::detectCores(),
  seed = "12345",
  chains = 4
)
pp_check(bmod_01)
summary(bmod_01)
#      Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
# diHC    -0.89      0.83    -2.50     0.74 1.00    67380    52618
# diAN    -0.16      1.62    -3.34     3.05 1.00    65536    50262
# diBN    -1.47      2.43    -6.74     2.95 1.00    68968    42387

emmeans::emmeans(bmod_01, ~ di,
                 regrid = "response") %>% 
  contrast(method = "revpairwise")
# contrast estimate lower.HPD upper.HPD
# AN - HC     0.719     -2.75      4.34
# BN - HC    -0.446     -5.83      4.33
# BN - AN    -1.203     -7.29      4.31
# 
# Point estimate displayed: median 
# HPD interval probability: 0.95 


# Proportions of food choices ---------------------------------------------

# By-subject data ---

# Computer one proportion for each condition/subject.
bysubj_freq <- dat |> 
  group_by(di, st, ID) |> 
  summarize(
    y = mean(trg_chosen, na.rm = TRUE)
  )

bysubj_freq$flag <- ifelse(
  (bysubj_freq$di == "HC") & (bysubj_freq$y < 50), NA, bysubj_freq$y
)

bysubj_freq <- bysubj_freq[complete.cases(bysubj_freq), ] 
bysubj_freq$flag <- NULL

bysubj_wide <- bysubj_freq %>%
  pivot_wider(names_from = st, values_from = y)

# Positive values indicate preference for neutral over food stimuli.
bysubj_wide <- bysubj_wide |> 
  mutate(delta = neutral - food)

bysubj_wide |> 
  group_by(di) |> 
  summarize(
    delta_choices = mean(delta, na.rm=T),
    se_delta = sd(delta, na.rm=T) / sqrt(n()),
    n = n()
  )


hist(bysubj_wide$delta)

# Standardize
bysubj_wide$delta_st <- 
  bysubj_wide$delta - mean(bysubj_wide$delta, na.rm = TRUE) /
  sd(bysubj_wide$delta, na.rm = TRUE)


priors <- c(
  set_prior("student_t(4, 0, 2.5)", class = "b"),
  set_prior("student_t(4, 0, 2.5)", class = "sigma")
)

bmod_01 <- brm(
  bf(delta_st ~ 0 + di),
  data = bysubj_wide,
  family = student(),
  control = list(adapt_delta = 0.99),
  prior = priors,
  backend = "cmdstan",
  warmup = 1000,
  iter = 20000,
  cores = parallel::detectCores(),
  seed = "12345",
  chains = 4
)
pp_check(bmod_01)
summary(bmod_01)
#      Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
# diHC    -0.89      0.83    -2.50     0.74 1.00    67380    52618
# diAN    -0.16      1.62    -3.34     3.05 1.00    65536    50262
# diBN    -1.47      2.43    -6.74     2.95 1.00    68968    42387

emmeans::emmeans(bmod_01, ~ di,
                 regrid = "response") %>% 
  contrast(method = "revpairwise")























# In the analysis below, I use the proportion of choices for each subject and
# condition. 

# Computer one proportion for each condition/subject.
bysubj_prop <- dat |> 
  group_by(di, st, ID) |> 
  summarize(
    y = mean(trg_chosen)
  )

bysubj_prop$flag <- ifelse(
  (bysubj_prop$di == "HC") & (bysubj_prop$y < 0.4), NA, bysubj_prop$y
)

bysubj_prop <- bysubj_prop[complete.cases(bysubj_prop), ] 
bysubj_prop$flag <- NULL

bysubj_wide <- bysubj_prop %>%
  pivot_wider(names_from = st, values_from = y)

# Positive values indicate preference for neutral over food stimuli.
bysubj_wide <- bysubj_wide |> 
  mutate(delta = neutral - food)

bysubj_prop |> 
  group_by(di, st) |> 
  summarize(
    prop = mean(y),
    n = n()
  )
# di    st       prop
# 1 HC    neutral 0.476
# 2 HC    food    0.477
# 3 AN    neutral 0.492
# 4 AN    food    0.447
# 5 BN    neutral 0.443
# 6 BN    food    0.492

# AN
(0.492 - 0.447) * 100
# 4.5
# BN
(0.492 - 0.443) * 100
# 4.9
# HC
(0.477 - 0.476) * 100
# 0.1

bmod_beta_biased_choices_01 <- brm(
  bf(y ~ di * st + (1 + st | ID),
     phi ~ di),
  data = bysubj_prop,
  family = Beta(),
  #control = list(adapt_delta = 0.99),
  # prior = priors,
  backend = "cmdstan",
  # algorithm = "meanfield",
  # warmup = 1000,
  iter = 20000,
  cores = parallel::detectCores(),
  seed = "12345",
  init = "random",
  chains = 4
)
pp_check(bmod_beta_biased_choices_01)

posterior_beta <- bmod_beta_biased_choices_01 %>% 
  gather_draws(`b_.*`, regex = TRUE) %>% 
  mutate(component = ifelse(str_detect(.variable, "phi_"), "Precision", "Mean"),
         intercept = str_detect(.variable, "Intercept"))

ggplot(posterior_beta, aes(x = .value, y = fct_rev(.variable), fill = component)) +
  geom_vline(xintercept = 0) +
  stat_halfeye(aes(slab_alpha = intercept), 
               .width = c(0.8, 0.95), point_interval = "median_hdi") +
  scale_fill_viridis_d(option = "viridis", end = 0.6) +
  scale_slab_alpha_discrete(range = c(1, 0.4)) +
  guides(fill = "none", slab_alpha = "none") +
  labs(x = "Coefficient", y = "Variable",
       caption = "80% and 95% credible intervals shown in black") +
  facet_wrap(vars(component), ncol = 1, scales = "free_y")



summary(bmod_beta_biased_choices_01)



only_patients <- bysubj_prop |> 
  dplyr::filter(di != "HC")

bmod_01 <- brm(
  bf(
    y ~ di * st + (1 + st | ID)
    ),
  data = bysubj_prop,
  family = Beta(),
  control = list(adapt_delta = 0.99),
  # prior = priors,
  backend = "cmdstan",
  iter = 20000,
  cores = parallel::detectCores(),
  seed = "12345",
  init = "random",
  chains = 4
)
pp_check(bmod_01)
summary(bmod_01)






# Run model on individual trials ---

p_est <- mean(bysubj_prop$y)
theta_est <- log(bysubj_prop$y / (1 - bysubj_prop$y))
c(mean(theta_est), sd(theta_est))

# Fit the brm model.
priors <- c(
  set_prior("normal(-0.11, 0.402)", class = "b", coef = "Intercept"),
  set_prior("student_t(3, 0, 0.2)", class = "b"),
  set_prior("student_t(3, 0, 0.2)", class = "sd"),
  set_prior("lkj(1)", class = "cor")
  # set_prior("normal(60, 20)", class = "phi")
)
  
# With variational inference cmdstan cannot be used!
bmod_biased_choices_01 <- brm(
  trg_chosen ~ 0 + Intercept + di * st + (1 + st | ID),
  data = dat,
  family = bernoulli(),
  #control = list(adapt_delta = 0.99),
  prior = priors,
  # backend = "cmdstan",
  # algorithm = "meanfield",
  # warmup = 1000,
  # iter = 40000,
  cores = parallel::detectCores(),
  seed = "12345",
  init = "random",
  chains = 4,
)
  
pp_check(bmod_biased_choices_01)


# Below I use the brms functions.
# me <- conditional_effects(
#   bmod_biased_choices_01, "st:di")
# plot(me, plot = FALSE)[[1]] +
#   facet_wrap(~di) +
#   labs(
#     x = "Target Image Cagegory",
#     y = "Probability of Target Choice"
#   ) +
#   scale_color_grey() +
#   scale_fill_grey()


# The y-axis scale is changed from logit to prob.
p1 <- bmod_biased_choices_01 |> 
  emmeans( ~ st | di, regrid = "response") %>%
  gather_emmeans_draws() %>%
  ggplot(aes(x = st, y = .value)) +
  ylim(0.425, 0.525) +
  stat_halfeye() +
  stat_summary(aes(group = NA), fun.y = mean, geom = "line") +
  facet_grid(~ di) +
  geom_hline(yintercept=0, linetype="dashed", color = "lightgray") +
  labs(
    x = "Cagegory of Target Image",
    y = "Posterior Probability\nof Target Choice"
  ) +
  bayesplot::theme_default(base_family = "sans")
  # papaja::theme_apa()


# Plot the distribution and add the limits of the two CIs
posteriors = insight::get_parameters(bmod_biased_choices_01)

#HDI
ci_hdi <- parameters::ci(posteriors$`b_diAN:stfood`, method = "HDI")
#prior
prior = distribution_normal(4000, mean = 0, sd = 0.2)
#support interval
# si_1 = si(posteriors$b_reward_oneback1, prior, BF = 1)
p2 <- ggplot(posteriors, aes(x = `b_diAN:stfood`)) +
  theme_classic() +
  geom_density(fill = "lightgray") +
  # The median in red
  geom_vline(
    xintercept = median(posteriors$`b_diAN:stfood`),
    color = "black",
    size = 1
  ) +
  geom_vline(
    xintercept = ci_hdi$CI_low,
    color = "darkgray",
    linetype = "dashed",
    size = 1
  ) +
  geom_vline(
    xintercept = ci_hdi$CI_high,
    color = "darkgray",
    linetype = "dashed",
    size = 1
  )+
  xlab(label="Interaction-Term Coefficient (AN vs. HC)")+
  ylab(label="Density") +
  bayesplot::theme_default(base_family = "sans")


#HDI
ci_hdi <- parameters::ci(posteriors$`b_diBN:stfood`, method = "HDI")
#prior
prior = distribution_normal(4000, mean = 0, sd = 0.2)
#support interval
# si_1 = si(posteriors$b_reward_oneback1, prior, BF = 1)
p3 <- ggplot(posteriors, aes(x = `b_diBN:stfood`)) +
  theme_classic() +
  geom_density(fill = "lightgray") +
  # The median in red
  geom_vline(
    xintercept = median(posteriors$`b_diBN:stfood`),
    color = "black",
    size = 1
  ) +
  geom_vline(xintercept = ci_hdi$CI_low,
             color = "darkgray",
             linetype = "dashed",
             size = 1) +
  geom_vline(xintercept = ci_hdi$CI_high,
             color = "darkgray",
             linetype = "dashed",
             size = 1)+
  xlab(label="Interaction-Term Coefficient (BN vs. HC)")+
  ylab(label="Density") +
  bayesplot::theme_default(base_family = "sans")


p1 / (p2 + p3)

ggsave("biased_choices.pdf", width = 21, height = 16, units = "cm")

# Bayes Factor.
mod_bf = bayesfactor_parameters(bmod_biased_choices_01)
mod_bf
# Parameter   |     BF
# --------------------
# (Intercept) | 951.64
# diAN        |  0.401
# diBN        |  0.653
# stfood      |  0.155
# diAN:stfood |   4.94
# diBN:stfood |  0.862

plot(mod_bf)

# AN vs HC interaction term.
effectsize::interpret_bf(exp(mod_bf$log_BF[5]), include_value = TRUE)
# [1] "moderate evidence (BF = 4.94) in favour of"

# BN vs HC interaction term.
effectsize::interpret_bf(exp(mod_bf$log_BF[6]), include_value = TRUE)
# [1] "anecdotal evidence (BF = 1/1.16) against"

mod_bf_rope=bayesfactor_parameters(bmod_biased_choices_01, null=c(-0.0402, 0.0402))
mod_bf_rope
# Parameter   |    BF
# -------------------
# (Intercept) | 31.63
# diAN        | 0.237
# diBN        | 0.625
# stfood      | 0.048
# diAN:stfood |  4.47
# diBN:stfood | 0.864
# # 
# * Evidence Against The Null: [-0.040, 0.040]

plot(mod_bf_rope)

# Check the scripts here:
# https://osf.io/hrmy8



# Same as above with all the data -----------------------------------------

d$di <- factor(d$diag_cat)
d$di <- relevel(d$di, ref = "HC")
contrasts(dat$di)


# Stimulus type
dat$st <- factor(dat$stimulus_type)
dat$st <- relevel(dat$st, ref = "neutral")
contrasts(dat$st)

# Is target image chosen
d$trg_chosen <- factor(d$is_target_img_chosen)
unique(d$trg_chosen)
# [1] 0 1

# Stimulus type
d$st <- factor(d$stimulus_type)
d$st <- relevel(d$st, ref = "neutral")
contrasts(d$st)

# Subject id.
d$ID <- factor(d$subj_code)
length(unique(d$ID))
# [1] 261

# RTs in sec.
d <- d |> 
  mutate(
    rt = rt1 / 1000
  )



# Run model on individual trials ---

bysubj2_prop <- d |> 
  group_by(di, st, ID) |> 
  summarize(
    y = mean(as.numeric(as.character(trg_chosen)), na.rm = T)
  )





p_est <- mean(bysubj2_prop$y)
theta_est <- log(bysubj2_prop$y / (1 - bysubj2_prop$y))
c(mean(theta_est), sd(theta_est))

# Fit the brm model.
priors <- c(
  set_prior("normal(-0.11, 0.39)", class = "b", coef = "Intercept"),
  set_prior("student_t(3, 0, 0.2)", class = "b"),
  set_prior("student_t(3, 0, 0.2)", class = "sd"),
  set_prior("lkj(1)", class = "cor")
  # set_prior("normal(60, 20)", class = "phi")
)

# With variational inference cmdstan cannot be used!
bmod_biased_choices_02 <- brm(
  trg_chosen ~ 0 + Intercept + di * st + (1 + st | ID),
  data = d,
  family = bernoulli(),
  #control = list(adapt_delta = 0.99),
  prior = priors,
  # backend = "cmdstan",
  # algorithm = "meanfield",
  # warmup = 1000,
  # iter = 40000,
  cores = parallel::detectCores(),
  seed = "12345",
  init = "random",
  chains = 4,
)

pp_check(bmod_biased_choices_02)

mod2_bf = bayesfactor_parameters(bmod_biased_choices_02)
mod2_bf
# Parameter     |     BF
# ----------------------
# (Intercept)   | 228.56
# diAN          |  0.413
# diAN_R        |  0.340
# diBN          |  0.691
# diBN_R        |  0.432
# diRI          |  0.391
# stfood        |  0.131
# diAN:stfood   |   6.49
# diAN_R:stfood |  0.486
# diBN:stfood   |  0.852
# diBN_R:stfood |  0.584
# diRI:stfood   |  0.449

summary(bmod_biased_choices_02)
#               Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
# Intercept        -0.10      0.02    -0.13    -0.06 1.00     2227     2406
# diAN              0.05      0.05    -0.04     0.15 1.00     2050     2494
# diAN_R            0.02      0.08    -0.13     0.17 1.00     3018     3321
# diBN             -0.09      0.08    -0.24     0.06 1.00     2123     2218
# diBN_R            0.00      0.09    -0.18     0.19 1.00     3371     3174
# diRI              0.04      0.06    -0.07     0.16 1.00     2219     2737
# stfood            0.00      0.03    -0.06     0.06 1.00     1791     2322
# diAN:stfood      -0.18      0.08    -0.34    -0.03 1.00     1968     2527
# diAN_R:stfood    -0.01      0.12    -0.24     0.23 1.00     2808     2784
# diBN:stfood       0.12      0.12    -0.09     0.36 1.00     2303     2526
# diBN_R:stfood     0.03      0.14    -0.24     0.31 1.00     3474     2800
# diRI:stfood      -0.06      0.09    -0.23     0.11 1.00     1998     2735

# eof ---


trg_lag_df <- d |>
  select(feedback, trg_chosen, ID, st, di) |> 
  group_by(ID, st, di) |> 
  mutate(
    prev_trg_chosen = lag(trg_chosen),
    prev_feedback = lag(feedback)
  )
trg_lag_df$prev_trg_chosen <- 
  as.numeric(as.character(trg_lag_df$prev_trg_chosen))

trg_lag_df$trg_chosen <- 
  as.numeric(as.character(trg_lag_df$trg_chosen))


trg_lag_df$same_prev_fdbk <- ifelse(
  (trg_lag_df$trg_chosen == trg_lag_df$prev_trg_chosen) & 
    trg_lag_df$prev_feedback == 1, 1, 0
)


trg_lag_df |> 
  group_by(same_prev_fdbk, st, di) |> 
  summarize(
    y = mean(trg_chosen, na.rm = TRUE),
    n = n()
  ) |> 
  as.data.frame()


mod <- lme4::glmer(
  trg_chosen ~ prev_trg_chosen + prev_feedback + st * di + (st | ID),
  family=binomial(link="logit"),
      data = trg_lag_df)

summary(mod)

temp <- trg_lag_df |> 
  dplyr::filter(di %in% c("HC", "AN", "BN"))


m3 <- brm(
  trg_chosen ~ 0 + Intercept + prev_trg_chosen + prev_feedback + 
    di * st + (1 + st | ID),
  data = temp,
  family = bernoulli(),
  #control = list(adapt_delta = 0.99),
  prior = priors,
  backend = "cmdstan",
  # algorithm = "meanfield",
  # warmup = 1000,
  # iter = 40000,
  cores = parallel::detectCores(),
  seed = "12345",
  init = "random",
  chains = 4,
)

m3_bf = bayesfactor_parameters(m3)
m3_bf
# Parameter       |       BF
# --------------------------
# (Intercept)     | 1.25e+25
# prev_trg_chosen | 3.46e+60
# prev_feedback   |    0.932
# diAN            |    0.327
# diBN            |    0.651
# stfood          |    0.120
# diAN:stfood     |     3.72
# diBN:stfood     |    0.832

plot(m3_bf)


###########################################################################


# Bayesian regression
bysubj_prop$di <- relevel(bysubj_prop$di, ref = "AN")

bcpriors <- get_prior(
  y ~ 0 + Intercept + di * st + (1 + st | ID),
  family = Beta("logit"),
  data = bysubj_prop
)

priors <- c(
  set_prior("student_t(3, 0, 0.2)", class = "b", coef = "Intercept"),
  set_prior("student_t(3, 0, 0.2)", class = "b"),
  set_prior("student_t(3, 0, 0.2)", class = "sd"),
  # set_prior("gamma(0.01, 0.01)", class = "phi"),
  set_prior("lkj(1)", class = "cor")
)

mod1 <- brm(
  y ~ 0 + Intercept + di * st + (st | ID),
  data = bysubj_prop, 
  prior = priors,
  family = zero_inflated_beta(),
  # control = list(adapt_delta = 0.99, max_treedepth = 15),
  # backend = "cmdstan",
  iter = 10000, 
  chains = 4, 
  cores = 4
)

p <- conditional_effects(
  mod1,
  effects = "st:di"
)

p <- plot(p, plot = FALSE)[[1]] +
  xlab("Diagnostic category") +
  ylab("Choice of target stimulus") 
p
# p + scale_colour_colorblind() 


emmeans(mod1, specs = pairwise ~ st:di, type = "response")

#get the adjusted means
warp_em <- emmeans(mod1,  ~ st | di, type = "response")
warp_em

#get all possible contrasts
cont <- contrast(warp_em, "tukey")
cont

#get the posterior draws from the contrasts
cont_posterior <- gather_emmeans_draws(cont)

#plot
ggplot(cont_posterior,
       aes(y = contrast, x = .value)) +
  stat_halfeye() +
  facet_wrap(~di) +
  geom_vline(xintercept = 0, color = "red", lty = 2)

ggplot(
  cont_posterior,
  aes(y = contrast, x = .value, fill = di, group = di)) +
  stat_halfeye(alpha = 0.5) +
  geom_vline(xintercept = 0, color = "red", lty = 2)

mod1 |> 
  emmeans( ~ st | di) |> 
  gather_emmeans_draws() |> 
  ggplot(aes(x = st, y = .value)) +
  stat_halfeye() +
  stat_summary(aes(group = NA), fun.y = mean, geom = "line") +
  facet_grid(~ di) +
  geom_hline(yintercept=0, linetype="dashed", color = "lightgray") +
  labs(
    x = "Stimulus Type",
    y = expression(ln(p) - ln(1-p))
  ) +
  papaja::theme_apa()

pp_check(mod1)

summary(mod1)
#             Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
# Intercept      -0.09      0.02    -0.13    -0.05 1.00     4220     3163
# diAN            0.04      0.05    -0.05     0.13 1.00     3964     3220
# diBN           -0.05      0.07    -0.19     0.07 1.00     5389     2629
# stfood         -0.01      0.03    -0.07     0.05 1.00     3060     3315
# diAN:stfood    -0.17      0.08    -0.35    -0.02 1.00     3271     2897
# diBN:stfood     0.06      0.09    -0.11     0.25 1.00     4787     2514

# Bayes Factor.
mod_bf = bayestestR::bayesfactor_parameters(mod1)
mod_bf

plot(mod_bf)

plot(bayesfactor_models(mod_1)) +
  scale_fill_pizza()

# Interaction term:
effectsize::interpret_bf(mod_bf$log_BF[4], include_value = TRUE)


rope = bayestestR::bayesfactor_parameters(mod_1, null = c(-0.0132, 0.0132))
rope





dat <- dat |>
  group_by(di, st, ID) |> 
  mutate(
    prev_feedback = lag(feedback)
  )



bysubj_prop <- dat |> 
  group_by(di, st, ID) |> 
  summarize(
    y = mean(trg_chosen),
    inc = sum(trg_chosen),
    size = n()
  )

bysubj_prop |> 
  group_by(di, st) |> 
  summarize(
    prop = mean(y),
    n = n()
  )

bysubj_prop$y <- ifelse(bysubj_prop$y == 0, 0.005, bysubj_prop$y)
bysubj_prop$y <- ifelse(bysubj_prop$y == 1, 0.995, bysubj_prop$y)


mod_01_glmer <- lme4::glmer(
  inc / size ~ di * st + (1 | ID),
  weights = size,
  family = binomial(),
  data = bysubj_prop
)

car::Anova(mod_01_glmer)
summary((mod_01_glmer))

bcpriors <- get_prior(
  inc / size ~ di * st + (1 | ID),
  family = binomial,
  data = bysubj_prop
)

priors <- c(
  set_prior("student_t(3, 0, 0.2)", class = "b", coef = "Intercept"),
  set_prior("student_t(3, 0, 0.2)", class = "b"),
  set_prior("student_t(3, 0, 0.2)", class = "sd"),
  # set_prior("gamma(0.01, 0.01)", class = "phi"),
  set_prior("lkj(1)", class = "cor")
)

mod2 <- brm(
  inc ~ 0 + Intercept + di * st + (st | ID),
  # weights = size,
  data = bysubj_prop, 
  prior = priors,
  family = poisson(),
  # control = list(adapt_delta = 0.99, max_treedepth = 15),
  # backend = "cmdstan",
  # iter = 2000, 
  chains = 4, 
  cores = 4
)

summary(mod2)
#                 Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
# Intercept           0.48      0.00     0.47     0.49 1.00    20402    11333
# sigma_Intercept    -3.03      0.08    -3.20    -2.86 1.00    10431    10591
# diAN               -0.00      0.01    -0.03     0.02 1.00    12945     8894
# diBN                0.01      0.02    -0.03     0.06 1.00    12175     9313
# sigma_diAN          0.29      0.20    -0.10     0.70 1.00    10862    10247
# sigma_diBN          0.18      0.31    -0.41     0.81 1.00    12963    10506

pp_check(mod2)

# Bayes Factor.
mod_bf = bayestestR::bayesfactor_parameters(mod2)
mod_bf
plot(mod_bf)
