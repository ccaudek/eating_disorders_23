# Script name: 02_wcst_descriptive_stats.R
# Project: Eating disorders Montecatini
# Script purpose: Compare descriptve stats between patients and controls
# @author: Corrado Caudek <corrado.caudek@unifi.it>
# Date Created: Tue May 24 17:04:30 2022
# Last Modified Date: Tue May 24 17:04:30 2022
#
# 👉 

# Prelims
library("here")
library("tidyverse")
library("brms")
library("cmdstanr")


patients <- readRDS(
  here("data", "processed", "wcst", "wcst_descript_patients.rds")
)
patients$is_patient <- 1

controls <- readRDS(
  here("data", "processed", "wcst", "wcst_descript_controls.rds")
)
controls$is_patient <- 0

d <- rbind(
  patients, controls
)

d %>% 
  group_by(is_patient) %>% 
  summarise(
    prop_pers_err = mean(prop_pers_err),
    prop_non_pers_err = mean(prop_non_pers_err),
    prop_err = mean(prop_err),
    prop_cor = mean(prop_cor),
    n = n()
  )

hist(d$prop_pers_err)

d$is_patient <- factor(d$is_patient)

bf1 <- bf(prop_pers_err ~ is_patient)

fit1 <- brm(
  bf1, 
  data = d, 
  family = exgaussian(),
  chains = 4, cores = 4,
  backend = "cmdstan"
)

pp_check(fit1)

summary(fit1)
conditional_effects(fit1, "is_patient")



# prop_non_pers_err ----

hist(d$prop_non_pers_err)

bf2 <- bf(prop_non_pers_err ~ is_patient)

fit2 <- brm(
  bf2, 
  data = d, 
  family = exgaussian(),
  chains = 4, cores = 4,
  backend = "cmdstan"
)

pp_check(fit2)

summary(fit2)
conditional_effects(fit2, "is_patient")

# prop_err -----

hist(d$prop_err)

bf3 <- bf(prop_err ~ is_patient)

fit3 <- brm(
  bf3, 
  data = d, 
  family = exgaussian(),
  chains = 4, cores = 4,
  backend = "cmdstan"
)

pp_check(fit3)

summary(fit3)
conditional_effects(fit3, "is_patient")


# prop_cor ---


hist(d$prop_cor)

bf4 <- bf(prop_cor ~ is_patient)

fit4 <- brm(
  bf4, 
  data = d, 
  family = Beta(),
  chains = 4, cores = 4,
  backend = "cmdstan"
)

pp_check(fit4)

summary(fit4)
conditional_effects(fit4, "is_patient")



