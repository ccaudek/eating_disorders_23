#' ---
#' title: "PRL params and diagnostic groups"
#' author: "[Corrado Caudek](https://ccaudek.github.io/)"
#' date: "First version Wed Jun  9 14:20:42 2021. Last modified `r format(Sys.time(), '%Y-%m-%d')`"
#' output:
#'   pdf_document:
#'     keep_tex: true
#' ---

# Prelims
suppressPackageStartupMessages({
  library("here")
  library("tidyverse")
  library("forcats")
  library("readxl")
  library("brms")
  library("cmdstanr")
  library("tidybayes")
})

set_cmdstan_path("/Users/corrado/cmdstan")
# Increase max print
options(max.print = .Machine$integer.max)

source(here::here("lib", "ed_fnc.R"))


d <- readRDS(
  here("data", "processed", "prl", "prl_and_quest", "prl_params_and_quest_data.rds")
)

patients_codes <- c(
  "cr_gi_1994_10_14_347_f",
  "bi_an_2001_09_16_735_f",
  "ir_bo_1981_03_29_325_f",
  "la87_ac98" ,
  "al_zu_1997_04_02_880_f",
  "ch_be_1990_12_20_153_f",
  "gr_bo_1996_07_31_547_f",
  "am_gu_1999_02_11_937_f",
  "ir_pi_2002_01_22_765_f",
  "el_ma_1986_06_14_839_f",
  "da_de_1998_08_15_141_m",
  "em_gr_2002_08_25_628_f",
  "fe_al_1988_05_06_180_f",
  "gi_ma_1999_09_26_585_f",
  "lu_mu_1997_03_18_059_f",
  "ch_ca_2000_09_26_406_f",
  "em_al_1989_07_27_200_f",
  "ca_po_2002_05_25_700_f",
  "cr_pa_1969_04_12_179_f",
  "ar_co_1996_12_27_348_f",
  "an_de_1998_11_10_289_f",
  "de_sc_1992_07_02_116_f",
  "em_or_2003_02_01_101_f",
  "ga_gi_2003_02_09_229_f",
  "ju_yo_1998_05_28_316_f",
  "ch_na_2007_06_23_908_f",
  "ma_ba_1995_05_25_321_f",
  "al_ro_1989_04_25_160_f",
  "an_am_1996_05_12_176_f",
  "ca_pa_2002_04_05_939_f",
  "he_ha_2006_04_21_874_f",
  "ch_br_1993_10_04_623_f",
  "fe_ma_1998_06_29_257_f",
  "fe_sa_2002_05_09_008_f",
  "so_be_2008_12_15_399_f",
  "ca_so_2001_01_09_118_f",
  "ch_pi_2001_10_08_418_f",
  "la_al_1996_06_14_190_f",
  "is_ie_1986_08_18_291_f",
  "al_pe_1996_09_02_886_m",
  "sa_ta_2003_11_14_150_f",
  "ch_pi_2004_02_25_126_f",
  "as_ga_2005_06_15_329_f",
  "fr_la_2004_05_17_363_f",
  "ma_be_1996_"
)

patients_codes_prl <- find_subj_code_of_patients()
sort(patients_codes_prl)

sort(patients_codes)

bad_rt_participants <- find_bad_RTs_participants(patients_codes_prl)

d_clean1 <- d %>% 
  dplyr::filter(
    !subj_code %in% bad_rt_participants
  )

length(unique(d_clean1[d_clean1$is_patient == 1, ]$subj_code))


# Remove 'bad' control subjects
THRESHOLD <- 0.4
bad_subj_codes <- find_bad_controls(THRESHOLD)
length(bad_subj_codes)

d_clean2 <- d_clean1 %>% 
  dplyr::filter(
    !subj_code %in% bad_subj_codes
  )

d_clean2 <- d_clean2 %>% 
  dplyr::filter(
    !subj_code %in% bad_rt_participants
  )
length(unique(d_clean2[d_clean2$is_patient == 1, ]$subj_code))


# remove bad IDs from questionnaires data
d_clean3 <- d_clean2 %>% 
  dplyr::filter(!subj_code %in% bad_ids_cr_index)
length(unique(d_clean3[d_clean3$is_patient == 1, ]$subj_code))

diagn_df <- rio::import(here("data", "raw", "patients_diagnosis.xlsx"))

temp <- diagn_df[diagn_df$diagnosis == "BED" | diagn_df$diagnosis == "BN", ]
ids_bulimia <- temp$subj_code
ids_bulimia <- ids_bulimia[!is.na(ids_bulimia)]

remission <- c(
  "al_zu_1997_04_02_880_f",
  "el_ma_1986_06_14_839_f",
  "fe_al_1988_05_06_180_f",
  "lu_mu_1997_03_18_059_f",
  "ar_co_1996_12_27_348_f",
  "de_sc_1992_07_02_116_f"
)

bad_ids_prop_posivive_feedback <- get_bad_ids_prop_posivive_feedback()


'%ni%' <- Negate('%in%')

d_final <- d_clean3 %>% 
  dplyr::filter(
    ((is_patient == 1 & subj_code %ni% remission) | is_patient == 0) &
      ((is_patient == 1 & subj_code %ni% ids_bulimia) | is_patient == 0) &
      (is_patient == 1 | (is_patient == 0 & subj_code %ni% bad_ids_person_tot_cor)) &
      (is_patient == 1 | (is_patient == 0 & subj_code %ni% bad_ids_longstring)) &
      (is_patient == 1 | (is_patient == 0 & subj_code %ni% bad_ids_maha1)) &
      (is_patient == 1 | (is_patient == 0 & subj_code %ni% bad_ids_maha2)) &
      (is_patient == 1 | (is_patient == 0 & subj_code %ni% bad_ids_maha3)) &
      (is_patient == 1 | (is_patient == 0 & subj_code %ni% bad_ids_maha4)) &
      (is_patient == 1 | (is_patient == 0 & subj_code %ni% bad_ids_maha5)) &
      (is_patient == 1 | (is_patient == 0 & subj_code %ni% bad_ids_maha6)) &
      (is_patient == 1 | (is_patient == 0 & subj_code %ni% bad_ids_prop_posivive_feedback)) &
      # (subj_code %ni% bad_ids_prop_posivive_feedback) &
      (is_patient == 1 | (is_patient == 0 & eat26_at_risk == 0)) 
  )

# d_final <- d_final %>% 
#   dplyr::filter(
#     subj_code %ni% bad_ids_prop_posivive_feedback
#   )

d_final %>% 
  group_by(is_patient) %>% 
  count()

d_final$apf <- scale(d_final$alpha_neg_food)[, 1]
d_final$anf <- scale(d_final$alpha_pos_food)[, 1]
d_final$aps <- scale(d_final$alpha_neg_social)[, 1]
d_final$ans <- scale(d_final$alpha_pos_social)[, 1]
d_final$vf <- scale(d_final$v_food)[, 1]
d_final$vs <- scale(d_final$v_social)[, 1]
d_final$y <- scale(d_final$t_food)[, 1]
d_final$y <- scale(d_final$t_social)[, 1]

d_wide <- with(
  d_final,
  data.frame(apf, anf, aps, ans, subj_code, is_patient)
)

d_long <- d_wide %>%
  pivot_longer(
    !c("is_patient", "subj_code"), 
    names_to = "condition", 
    values_to = "alpha"
  )

d_long$is_patient <- factor(d_long$is_patient)

# We contrast-coded `patient` (patient = -0.5, control = 0.5). 
d_long <-
  d_long %>%
  # contrast coding
  mutate(
    patient = ifelse(d_long$is_patient == 1, -0.5, 0.5) 
  )

d_long$is_food <- ifelse(
  d_long$condition == "apf" | d_long$condition == "anf", 1, 0
)
d_long$is_food <- factor(d_long$is_food)


prior_ma <- prior(normal(0, 2), class = "b") + 
  prior(normal(0, 5), class = "Intercept")

prior1 <- c(
  prior(normal(0, 10), class = Intercept),
  prior(normal(0, 10), class = b, coef = patient)
  # prior(cauchy(0, 10), class = sigma)
)

fit1 <- brm(
  bf(
    alpha ~ patient, 
    sigma ~ patient
  ),
  data = d_long, 
  prior = prior1,
  family = student(),
  control = list(adapt_delta = 0.99),
  iter = 4000,
  cores = 6,
  backend = "cmdstan"
)

pp_check(fit1)
pp_check(fit1, type = 'stat', stat = 'mean')
bayes_R2(fit1)
summary(fit1)




m1 <- brm(
    # alpha ~ is_patient + (condition | subj_code), 
    alpha ~ patient + 
      (1 | subj_code) + 
      (1 + patient | condition) + 
      (1 | subj_code:condition),
  data = d_long, 
  prior = prior_ma,
  family = student(),
  control = list(adapt_delta = 0.99),
  iter = 4000,
  cores = 6,
  backend = "cmdstan"
)


m1 <- brm(
  bf(
    alpha ~ is_patient * condition + (condition | subj_code)
    # sigma ~ is_patient,
    # nu ~ is_patient
  ), 
  data = d_long, 
  prior = prior_ma,
  family = student(),
  control = list(adapt_delta = 0.99),
  iter = 4000,
  cores = 6,
  backend = "cmdstan"
)

pp_check(m1)
pp_check(m1, type = 'stat', stat = 'mean')
bayes_R2(m1)
summary(m1)

hyp <- c("is_patient1 < 0")
hypothesis(m1, hyp)

conditional_effects(m1, "is_patient")
conditional_effects(m1, "is_patient:condition")

d_long$y <- d_long$alpha + abs(min(d_long$alpha, na.rm = TRUE)) + 0.1

m1a <- brm(
  alpha ~ is_patient * condition +
    (condition | subj_code), 
  data = d_long, 
  prior = prior_ma,
  family = student(),
  control = list(adapt_delta = 0.99),
  iter = 4000,
  cores = 6,
  backend = "cmdstan"
)
pp_check(m1a)
pp_check(m1a, type = 'stat', stat = 'mean')
bayes_R2(m1a)
summary(m1a)

conditional_effects(m1a, "is_patient:condition")


v_wide <- with(
  d_final,
  data.frame(vf, vs, subj_code, is_patient)
)

v_long <- v_wide %>%
  pivot_longer(
    !c("is_patient", "subj_code"), 
    names_to = "condition", 
    values_to = "v"
  )

v_long$is_patient <- factor(v_long$is_patient)
head(v_long)

m2 <- brm(
  v ~ is_patient * condition +
    (condition | subj_code), 
  data = v_long, 
  prior = prior_ma,
  family = student(),
  control = list(adapt_delta = 0.98),
  iter = 4000,
  cores = 6,
  backend = "cmdstan"
)

pp_check(m2)
bayes_R2(m2)
summary(m2)

conditional_effects(m2, "is_patient:condition") 












m1 <- brm(
  mvbind(apf, anf, aps, ans) ~ is_patient, 
  data = d_final, 
  prior = prior_ma,
  family = student(),
  control = list(adapt_delta = 0.98),
  iter = 4000,
  cores = 6,
  backend = "cmdstan"
)

pp_check(m1, resp = "anf") + xlim(-5, 5)
pp_check(m1, resp = "apf") + xlim(-5, 5)
pp_check(m1, resp = "ans") + xlim(-5, 5)
pp_check(m1, resp = "aps") + xlim(-5, 5)
bayes_R2(m1)
summary(m1)

d_final$grp <- factor(d_final$is_patient)

d_final %>% 
  ggplot(aes(x = grp, y = alpha_pos_food, fill = grp)) + 
  geom_violin(trim = FALSE)+
  geom_boxplot(width = 0.1, fill = "white") +
  labs(
    title = "Plot of alpha  by group", 
    x = "Group", 
    y = "Alpha"
  ) + 
  papaja::theme_apa()

control_data <- d_final %>% 
  dplyr::filter(is_patient == 0)

patients_data <- d_final %>% 
  dplyr::filter(is_patient == 1)


control_data$y <- scale(control_data$alpha_neg_food)[, 1]
control_data$y <- scale(control_data$alpha_pos_food)[, 1]

# dass21_stress + dass21_anxiety + dass21_dep +
# sias + mps_ps + mps_o + mps_cmd + mps_pepc + orto_tot + ros_tot        
# dass21_stress + dass21_anxiety + dass21_dep + sias + mps_ps + mps_o + 
# mps_cmd + mps_pepc + orto_tot + dieting + bulimia + oral_control
m1 <- brm(
  y ~ 
    # dass21_stress + dass21_anxiety + dass21_dep +
    # sias + mps_ps + mps_o + mps_cmd + mps_pepc + orto_tot + ros_tot +      
    # dass21_stress + dass21_anxiety + dass21_dep + sias + mps_ps + mps_o + 
    # mps_cmd + mps_pepc + orto_tot + 
    dieting + bulimia + oral_control, 
  data = control_data, 
  prior = prior_ma,
  family = student(),
  control = list(adapt_delta = 0.98),
  iter = 4000,
  cores = 6,
  backend = "cmdstan"
)

pp_check(m1)
bayes_R2(m1)
summary(m1)


plot(control_data$oral_control, control_data$alpha_neg_food)
plot(control_data$oral_control, control_data$alpha_pos_food)
plot(control_data$oral_control, control_data$alpha_neg_social)
plot(control_data$oral_control, control_data$alpha_pos_social)

plot(patients_data$oral_control, patients_data$alpha_neg_food)



df <- left_join(d_final, diagn_df)

df <- df %>% replace_na(
  list(diagnosis = "NONE")
)

# df$diagnosis <- ifelse(df$group == "control", "NONE", df$diagnosis)
# df$diagnosis <- ifelse(df$group == "at_risk", "NONE", df$diagnosis)

df$diagnosis <- factor(df$diagnosis)
summary(df$diagnosis)

df$dgn <- as.character(df$diagnosis)
df$dgn <- df$dgn %>% 
  dplyr::recode(
    "AN"  = "A",
    "ANR" = "A",
    "BED" = "B",
    "BN"  = "B"
  )


# last <- df[df$subj_code %in% good_ids, ]
# df$dgn <- ifelse(is.na(df$dgn), "A", df$dgn)
# summary(factor(last$dgn))



names_prl <- c(
  "a_neither", "v_food", "v_social", "t_food", "t_social", "z_food", "z_social",
  "alpha_neg_food", "alpha_neg_social", "alpha_pos_food", "alpha_pos_social"
)

last <- df


last$alpha_neg_food <- ifelse(
  last$dgn == "NONE" & last$alpha_neg_food < -2, NA, last$alpha_neg_food
)
temp <- last[!is.na(last$alpha_neg_food), ]


data.frame(s=diagn_df$subj_code, i= diagn_df$in_progress, d=diagn_df$diagnosis)

remission <- c(
  "al_zu_1997_04_02_880_f",
  "el_ma_1986_06_14_839_f",
  "fe_al_1988_05_06_180_f",
  "lu_mu_1997_03_18_059_f",
  "ar_co_1996_12_27_348_f",
  "de_sc_1992_07_02_116_f"
)


TRIM <- 0.1
temp %>% 
  group_by(dgn) %>% 
  summarise(
    avg_alpha_neg_food = mean(alpha_neg_food, na.rm = TRUE, trim = TRIM), 
    se_alpha_neg_food = sqrt(var(alpha_neg_food, na.rm = TRUE) / n()),
    n = n()
  )

temp1 <- temp %>% 
  dplyr::filter(dgn != "B" & !subj_code %in% remission)



# temp1 %>% 
#   group_by(dgn) %>% 
#   summarise(
#     avg_alpha_neg_food = mean(alpha_neg_food, na.rm = TRUE, trim = TRIM), 
#     se_alpha_neg_food = sqrt(var(alpha_neg_food, na.rm = TRUE) / n()),
#     n = n()
#   )
# 
# # dpc <- temp1 %>% 
# #   dplyr::select("a_neither", "v_food", "v_social", "t_food", "t_social", 
# #   "alpha_neg_food", "alpha_neg_social", "alpha_pos_food", "alpha_pos_social")
#   
# tempData <- mice::mice(temp1, m=5, maxit=50, meth = 'pmm', seed=500)
# complete_data <- complete(tempData,1)
# 
# temp1 <- complete_data







t.test(formula = y ~ dgn, data = temp1)
BayesFactor::ttestBF(formula = y ~ dgn, data = temp1)



tempdat <- temp1
tempdat$is_patient <- ifelse(tempdat$dgn == "A", 1, 0)

fit1 <- brm(
  is_patient ~ 
    # bsq14_tot + ros_tot +
    # sias + mps_ps + mps_o + mps_cmd + mps_pepc + orto_tot + 
    # a_neither + v_food + v_social + t_food + t_social + z_food + z_social + 
    alpha_neg_food + alpha_neg_social + alpha_pos_food + alpha_pos_social, 
  data = tempdat, 
  prior = prior_ma,
  # family = sratio("cloglog"),
  family = bernoulli(),
  # family = acat("probit"),
  control = list(adapt_delta = 0.98),
  iter = 4000,
  cores = 6,
  backend = "cmdstan"
)

summary(fit1)








temp <- df[!is.na(df$v_food), ]

temp1 <- temp %>% 
  dplyr::filter(dgn != "B")

temp1$y <- scale(temp1$v_food)[, 1]

ggplot(temp1, aes(x=dgn, y=y, fill=dgn)) + 
  geom_violin(trim=FALSE)+
  geom_boxplot(width=0.1, fill="white")+
  labs(title="Diagnostic groups", x="Diagnostic groups", y = "hDDMrl Parameter") + 
  papaja::theme_apa()


m1 <- brm(
  y ~ dgn, 
  data = temp1, 
  # prior = prior_ma,
  family = student(),
  control = list(adapt_delta = 0.98),
  iter = 4000,
  cores = 6,
  backend = "cmdstan"
)

pp_check(m1)
bayes_R2(m1)
summary(m1)



df %>% 
  group_by(dgn) %>% 
  summarise(across(names_prl, ~round(mean(., na.rm = TRUE, trim = 0.2), 2))) %>% 
  as.data.frame()


nn <- data.frame(
  "a_neither", "v_food", "v_social", "t_food", "t_social", 
  "alpha_neg_food", "alpha_neg_social", "alpha_pos_food", "alpha_pos_social"
)

out <- datclean %>% 
  dplyr::select("a_neither", "v_food", "v_social", "t_food", "t_social", 
                "alpha_neg_food", "alpha_neg_social", "alpha_pos_food", 
                "alpha_pos_social")



pc_out <- princomp(out, cor = TRUE, scores = TRUE)







df$in_progress <- factor(df$in_progress)
df$bmi <- as.numeric(as.character(df$bmi))


# only_an <- df %>% 
#   dplyr::filter(
#     dgn == "A" | dgn == "NONE"
#   )
# is_patient <- only_an$is_patient
# only_an$is_patient <- NULL
# df_s <- standardize_num_vars(only_an)
# df_s$is_patient <- is_patient
# table(df_s$is_patient)


is_patient <- df$is_patient
df$is_patient <- NULL
df_s <- standardize_num_vars(df)
df_s$is_patient <- is_patient
table(df_s$is_patient)



df_s$grp <- factor(df_s$dgn)
df_s$grp <- factor(
  df_s$grp, 
  levels = c("A", "NONE", "2"), 
  ordered = TRUE
)



# It does not add much...
fit1 <- brm(
  grp ~ 
    # bsq14_tot + ros_tot +
    # sias + mps_ps + mps_o + mps_cmd + mps_pepc + orto_tot + 
    a_neither + v_food + v_social + t_food + t_social + z_food + z_social + 
    alpha_neg_food + alpha_neg_social + alpha_pos_food + alpha_pos_social, 
  data = df_s, 
  prior = prior_ma,
  # family = sratio("cloglog"),
  family = cumulative("probit"),
  # family = acat("probit"),
  control = list(adapt_delta = 0.98),
  iter = 4000,
  cores = 6,
  backend = "cmdstan"
)













m1 <- brm(
  is_patient ~ 
    # oral_control + dieting + bulimia +
    # bsq14_tot + ros_tot +
    # sias + mps_ps + mps_o + mps_cmd + mps_pepc + orto_tot +
    a_neither + v_food + v_social + t_food + t_social + z_food + z_social + 
    alpha_neg_food + alpha_neg_social + alpha_pos_food + alpha_pos_social, 
  data = df_s, 
  prior = prior_ma,
  family = bernoulli(),
  control = list(adapt_delta = 0.98),
  iter = 10000,
  cores = 6,
  backend = "cmdstan"
)
summary(m1)

plot(conditional_effects(m1, "oral_control"))
plot(conditional_effects(m1, "sias"))
plot(conditional_effects(m1, "mps_pepc"))
plot(conditional_effects(m1, "alpha_neg_food"))
plot(conditional_effects(m1, "alpha_neg_social"))


hist(df_s$alpha_pos_food)

m2 <- brm(
  alpha_pos_food ~ dgn + oral_control + dieting + bulimia +
    bsq14_tot + ros_tot +
    sias + mps_ps + mps_o + mps_cmd + mps_pepc + orto_tot, 
  data = df_s, 
  prior = prior_ma,
  family = skew_normal(),
  control = list(adapt_delta = 0.98),
  iter = 4000,
  cores = 6,
  backend = "cmdstan"
)


pp_check(m2) 
bayes_R2(m2)
summary(m2)


m2 <- brm(
  alpha_pos_social ~ dgn, 
  data = df_s, 
  prior = prior_ma,
  family = skew_normal(),
  control = list(adapt_delta = 0.98),
  iter = 4000,
  cores = 6,
  backend = "cmdstan"
)
summary(m2)








