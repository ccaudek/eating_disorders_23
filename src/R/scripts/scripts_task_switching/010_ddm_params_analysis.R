# Script name: 101_ddm_params_analysis.R
# Project: Eating disorders Montecatini
# Script purpose: Analyze DDM parameters for the task switching experiment.
# @author: Corrado Caudek <corrado.caudek@unifi.it>
# Date Created: Thu Jun  2 14:05:52 2022
# Last Modified Date: Thu Jun  2 14:05:52 2022
#
# 👉 

suppressPackageStartupMessages({
  library("here")
  library("tidyverse")
  library("cmdstanr")
  library("brms")
})


d <- rio::import(
  here::here(
    "data", "processed", "task_switching", "output_params_hddm", 
    "task_switch_params.csv"
  )
)

d$ii <- NULL
d$subj_idx <- 0:354


# Add subj_code -----------------------------------------------------------

code_df <- readRDS(
  here::here(
    "data", "processed", "task_switching", "input_for_hddm",
    "lookup_table_task_switching.rds"
  )
)
code_df$subj_idx <- as.integer(code_df$subj_idx)

d1 <- left_join(d, code_df, by = "subj_idx")
d1$subj_idx <- NULL


d1$diag_cat <- factor(d1$diag_cat)
d1$diag_cat %>% summary()


# Drift rate --------------------------------------------------------------


v_df <- d1[, c(5:8, 15:16)]
names(v_df)

v_names <- c("repeat.food",   "repeat.plants", "switch.food",   "switch.plants",
             "subj_code", "diag_cat")
colnames(v_df) <- v_names

out <- v_df %>%
  pivot_longer(!c("subj_code", "diag_cat"), names_to = "condition", values_to = "drift_rate")

v_long <- out %>% separate(condition, c("task", "stim"))

glimpse(v_long)

hist(v_long$drift_rate)

v_long %>% 
  group_by(diag_cat, stim, task) %>% 
  summarise(
    avg_drift_rate = mean(drift_rate, trim = 0.1)
  ) %>% 
  as.data.frame()

v_long$diag_cat <- relevel(v_long$diag_cat, ref = "HC")
contrasts(v_long$diag_cat)
  
an_hc_v_df <- v_long %>% 
  dplyr::filter(
    diag_cat != "AN_R" & diag_cat != "BN_R"
  )

an_hc_v_df$diag_cat <- factor(an_hc_v_df$diag_cat)
contrasts(an_hc_v_df$diag_cat)

bf_v <- bf(
  drift_rate ~ diag_cat * stim * task + (task * stim | subj_code)
)


fit_v <- brm(
  bf_v, 
  data = an_hc_v_df, 
  family = student(),
  chains = 4, cores = 4,
  iter = 2000,
  backend = "cmdstan"
)

pp_check(fit_v)

summary(fit_v)

conditional_effects(fit_v, "stim:task")

conditions <- make_conditions(fit_v, "diag_cat")
conditional_effects(fit_v, "stim:task", conditions = conditions)

bayes_R2(fit_v)


# Decision threshold ------------------------------------------------------


a_df <- d1[, c(1:4, 15:16)]
names(v_df)

a_names <- c("repeat.food",   "repeat.plants", "switch.food",   "switch.plants",
             "subj_code", "diag_cat")
colnames(a_df) <- a_names

out <- a_df %>%
  pivot_longer(!c("subj_code", "diag_cat"), names_to = "condition", values_to = "dec_thr")

a_long <- out %>% separate(condition, c("task", "stim"))


a_long %>% 
  group_by(diag_cat, stim, task) %>% 
  summarise(
    avg_dec_thr = mean(dec_thr, trim = 0.1)
  ) %>% 
  as.data.frame()

an_hc_a_df <- a_long %>% 
  dplyr::filter(
    diag_cat != "AN_R" & diag_cat != "BN_R"
  )

hist(an_hc_a_df$dec_thr)

bf_a <- bf(
  dec_thr ~ diag_cat * stim * task + (task * stim | subj_code)
)

fit_a <- brm(
  bf_a, 
  data = an_hc_a_df, 
  #family = exgaussian(),
  family = exponential(),
  chains = 4, cores = 4,
  iter = 2000,
  backend = "cmdstan"
)

pp_check(fit_a)

summary(fit_a)

conditions <- make_conditions(fit_a, "diag_cat")
conditional_effects(fit_a, "stim:task", conditions = conditions)

bayes_R2(fit_a)



# Non decision time -------------------------------------------------------


t_df <- d1[, c(9:12, 15:16)]
names(t_df)

t_names <- c("repeat.food",   "repeat.plants", "switch.food",   "switch.plants",
             "subj_code", "diag_cat")
colnames(t_df) <- t_names

out <- t_df %>%
  pivot_longer(!c("subj_code", "diag_cat"), names_to = "condition", values_to = "ndt")

t_long <- out %>% separate(condition, c("task", "stim"))

hist(t_long$ndt)

t_long %>% 
  group_by(diag_cat, stim, task) %>% 
  summarise(
    avg_ndt = mean(ndt, trim = 0.1)
  ) %>% 
  as.data.frame()

an_hc_t_df <- t_long %>% 
  dplyr::filter(
    diag_cat != "AN_R" & diag_cat != "BN_R"
  )
an_hc_t_df$diag_cat <- factor(an_hc_t_df$diag_cat)
an_hc_t_df$diag_cat %>% summary()


hist(an_hc_t_df$ndt)

bf_t <- bf(
  ndt ~ diag_cat * stim * task + (task * stim | subj_code)
)

fit_t <- brm(
  bf_t, 
  data = an_hc_t_df, 
  #family = exgaussian(),
  family = exponential(),
  chains = 4, cores = 4,
  iter = 1000,
  backend = "cmdstan"
)

pp_check(fit_t)

summary(fit_t)

conditions <- make_conditions(fit_t, "diag_cat")
conditional_effects(fit_t, "stim:task", conditions = conditions)

bayes_R2(fit_t)






