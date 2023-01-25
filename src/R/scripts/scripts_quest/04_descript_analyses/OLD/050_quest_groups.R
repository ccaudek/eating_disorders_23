#' ---
#' title: "Questionnaires data"
#' author: "[Corrado Caudek](https://ccaudek.github.io/)"
#' date: "First version Wed Jun  9 14:20:42 2021. Last modified `r format(Sys.time(), '%Y-%m-%d')`"
#' output:
#'   pdf_document:
#'     keep_tex: true
#' ---


library("here")
library("tidyverse")
library("forcats")
library("readxl")
library("pROC")
library("brms")
library("cmdstanr")
set_cmdstan_path("/Users/corrado/cmdstan")
library("ROCR")
library("tidybayes")

source(here::here("lib", "ed_fnc.R"))


# read questionnaries data
patients_codes <- get_patients_codes() 

# read PRL params and quest data
quest_data <- readRDS(
  here("data", "processed", "prl", "prl_and_quest", 
       "prl_params_and_quest_data.rds")
)

quest_data$eat26_tot <- quest_data$dieting + quest_data$bulimia + quest_data$oral_control

quest_data %>% 
  group_by(group) %>% 
  summarise(
    avg_eat26_tot = median(eat26_tot, na.rm = TRUE), 
    avg_eat26_bu  = median(bulimia, na.rm = TRUE), 
    avg_eat26_di  = median(dieting, na.rm = TRUE), 
    avg_eat26_oc  = median(oral_control, na.rm = TRUE), 
    n = n()
  )

quest_data %>% 
  group_by(group) %>% 
  summarise(
    avg_dass_a = median(dass21_anxiety, na.rm = TRUE), 
    avg_dass_s  = median(dass21_stress, na.rm = TRUE), 
    avg_dass_d  = median(dass21_dep, na.rm = TRUE),
    n = n()
  )

quest_data %>% 
  group_by(group) %>% 
  summarise(
    avg_sias = median(sias, na.rm = TRUE), 
    avg_orto  = median(orto_tot, na.rm = TRUE), 
    avg_ros  = median(ros_tot, na.rm = TRUE),
    avg_bsq14  = median(bsq14_tot, na.rm = TRUE),
    n = n()
  )

quest_data %>% 
  group_by(group) %>% 
  summarise(
    avg_ps = median(mps_ps, na.rm = TRUE), 
    avg_o  = median(mps_o, na.rm = TRUE), 
    avg_cmd  = median(mps_cmd, na.rm = TRUE),
    avg_pepc  = median(mps_pepc, na.rm = TRUE),
    n = n()
  )


subj_info <- get_subj_info()

quest_data2 <- left_join(quest_data, subj_info, by = "subj_code")
quest_data2$height  
quest_data2$present_weight

quest_data2$bmi <- quest_data2$present_weight / (quest_data2$height/100)^2
hist(quest_data2$bmi)
# sort(quest_data2$bmi)

m1 <- brm(
  bmi ~ age + sex +  
       oral_control + dieting + bulimia +
       bsq14_tot + ros_tot +
       dass21_stress + dass21_anxiety + dass21_dep +
       sias + mps_ps + mps_o + mps_cmd + mps_pepc + orto_tot,
  data = quest_data2, 
  # prior = prior_ma,
  family = skew_normal(),
  control = list(adapt_delta = 0.98),
  iter = 4000,
  cores = 6,
  backend = "cmdstan"
)

summary(m1)

plot(conditional_effects(m1, "oral_control"))
plot(conditional_effects(m1, "dieting"))
plot(conditional_effects(m1, "bsq14_tot"))
plot(conditional_effects(m1, "ros_tot"))
plot(conditional_effects(m1, "mps_cmd"))
plot(conditional_effects(m1, "age"))

bayes_R2(m1)


hist(quest_data2$age)


#' Gender-Dependent Associations of Anxiety and Depression Symptoms With 
#' Eating Disorder Psychopathology in a Representative Population Sample
#' Mareike Ernst, Antonia M. Werner, Ana N. Tibubos, Manfred E. Beutel, 
#' MartinadeZwaan and ElmarBrähler

table(quest_data2$sex)

hist(quest_data2$eat26_tot)


quest_data2$is_patient <- factor(quest_data2$is_patient)

quest_data3 <- quest_data2 %>% 
  dplyr::filter(sex != "Altro")

m3 <- brm(
  eat26_tot ~ bmi + age + (dass21_anxiety + dass21_dep) * (is_patient + sex),
  data = quest_data3, 
  # prior = prior_ma,
  family = skew_normal(),
  control = list(adapt_delta = 0.98),
  iter = 4000,
  cores = 6,
  backend = "cmdstan"
)

summary(m3)
plot(conditional_effects(m3, "dass21_anxiety:is_patient"))
plot(conditional_effects(m3, "dass21_dep:is_patient"))
plot(conditional_effects(m3, "dass21_dep"))
plot(conditional_effects(m3, "is_patient"))
plot(conditional_effects(m3, "age"))



m4 <- brm(
  eat26_tot ~ bmi + age + (dass21_anxiety + dass21_dep),
  data = quest_data3[quest_data3$sex == "Femmina", ], 
  # prior = prior_ma,
  family = skew_normal(),
  control = list(adapt_delta = 0.98),
  iter = 4000,
  cores = 6,
  backend = "cmdstan"
)

summary(m4)
plot(conditional_effects(m4, "dass21_dep"))


m4a <- brm(
  eat26_tot ~ bmi + age + (dass21_anxiety + dass21_dep),
  data = quest_data3[quest_data3$sex == "Femmina" & quest_data3$is_patient == 0, ], 
  # prior = prior_ma,
  family = skew_normal(),
  control = list(adapt_delta = 0.98),
  iter = 4000,
  cores = 6,
  backend = "cmdstan"
)

summary(m4a)
plot(conditional_effects(m4a, "dass21_dep"))


m5 <- brm(
  eat26_tot ~ bmi + age + (dass21_anxiety + dass21_dep),
  data = quest_data3[quest_data3$sex == "Maschio", ], 
  # prior = prior_ma,
  family = skew_normal(),
  control = list(adapt_delta = 0.98),
  iter = 4000,
  cores = 6,
  backend = "cmdstan"
)

summary(m5)
plot(conditional_effects(m5, "dass21_dep"))


quest_data3 %>% 
  group_by(sex) %>% 
  summarise(
    avg_bu = mean(bulimia, trim = 0.1, na.rm = TRUE),
    avg_di = mean(dieting, trim = 0.1, na.rm = TRUE),
    avg_oc = mean(oral_control, trim = 0.1, na.rm = TRUE),
    n = n()
  )


dat <- quest_data2 %>% 
  dplyr::filter(sex != "Altro")

dat$is_patient <- factor(dat$is_patient)

m6 <- brm(
  bulimia ~ sex,
  data = dat, 
  # prior = prior_ma,
  family = skew_normal(),
  control = list(adapt_delta = 0.98),
  iter = 4000,
  cores = 6,
  backend = "cmdstan"
)
summary(m6)

plot(conditional_effects(m6, "sex"))


m7 <- brm(
  dieting ~ sex,
  data = dat, 
  # prior = prior_ma,
  family = skew_normal(),
  control = list(adapt_delta = 0.98),
  iter = 4000,
  cores = 6,
  backend = "cmdstan"
)
summary(m7)

plot(conditional_effects(m7, "sex"))


dat %>% 
  group_by(sex) %>% 
  summarise(
    avg_bsq14 = mean(bsq14_tot, trim = 0.1, na.rm = TRUE)
  )


# Prevalence of body image dissatisfaction among youth in the United Arab 
# Emirates: gender, age, and body mass index differences
# Siham Alharballeh1 & Hamzeh Dodeen
hist(dat$bsq14)

fm0 <- brm(
  bsq14_tot ~ age + bmi + sex,
  data = dat, 
  # prior = prior_ma,
  family = skew_normal(),
  control = list(adapt_delta = 0.98),
  iter = 4000,
  cores = 6,
  backend = "cmdstan"
)
summary(fm0)

plot(conditional_effects(fm0, "bmi"))
plot(conditional_effects(fm0, "sex"))
# plot(conditional_effects(fm1, "is_patient:sex"))

fm1 <- brm(
  eat26_tot ~ bsq14_tot * (sex + is_patient) + age + bmi,
  data = dat, 
  # prior = prior_ma,
  family = skew_normal(),
  control = list(adapt_delta = 0.98),
  iter = 4000,
  cores = 6,
  backend = "cmdstan"
)
summary(fm1)

plot(conditional_effects(fm1, "bsq14_tot:is_patient"))




hist(dat$bmi)

dat$bmi_cat <- cut(dat$bmi, breaks = c(0, 18.5, 24.9, 29.9, 100))
summary(dat$bmi_cat)


dat$mps <- dat$mps_ps + dat$mps_o + dat$mps_cmd + dat$mps_pepc

fm2 <- brm(
  eat26_tot ~ mps * (sex + is_patient) + age + bmi,
  data = dat, 
  # prior = prior_ma,
  family = skew_normal(),
  control = list(adapt_delta = 0.98),
  iter = 4000,
  cores = 6,
  backend = "cmdstan"
)
summary(fm2)
plot(conditional_effects(fm2, "mps"))



fm3 <- brm(
  eat26_tot ~ sias * (sex + is_patient) + age + bmi,
  data = dat, 
  # prior = prior_ma,
  family = skew_normal(),
  control = list(adapt_delta = 0.98),
  iter = 4000,
  cores = 6,
  backend = "cmdstan"
)
summary(fm3)
plot(conditional_effects(fm3, "sias:is_patient"))





