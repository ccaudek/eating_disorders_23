# Script name: 045_weight_history.R
# Project: eating disorders with patients and controls
# Script purpose: weigth history and PRL params
# @author: Corrado Caudek <corrado.caudek@unifi.it>
# Date Created: Wed Jun  9 14:20:42 2021
# Last Modified Date: Wed Jun  9 14:20:42 2021
# 
# Notes: 

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
prl_quest_data <- readRDS(
  here("data", "processed", "prl", "prl_and_quest", 
       "prl_params_and_quest_data.rds")
)

# Remove 'bad' control subjects
THRESHOLD <- 0.55
bad_subj_codes <- find_bad_controls(THRESHOLD)
length(bad_subj_codes)

prl_quest_data_clean <- prl_quest_data %>% 
  dplyr::filter(
    !subj_code %in% bad_subj_codes
  )

prl_quest_data_clean$is_patient <- 
  ifelse(prl_quest_data_clean$subj_code %in% patients_codes, 1, 0)
sum(prl_quest_data_clean$is_patient)

prl_quest_data_clean <- prl_quest_data_clean %>% 
  dplyr::mutate(
    eat26 = oral_control + dieting + bulimia
  )
summary(prl_quest_data_clean)

# create group (control = 0, at_risk = 1, patient = 2)
prl_quest_data_clean$group <- prl_quest_data_clean$is_patient
prl_quest_data_clean$group <- 
  ifelse(prl_quest_data_clean$group == 1, 2, prl_quest_data_clean$group)
prl_quest_data_clean$group <- ifelse(
  prl_quest_data_clean$group == 0 & prl_quest_data_clean$eat26 > 19, 1, 
  prl_quest_data_clean$group
)
table(prl_quest_data_clean$group)
# TODO: ordered factor

prl_quest_data_clean$group <- factor(prl_quest_data_clean$group)
prl_quest_data_clean$group <- factor(
  prl_quest_data_clean$group, 
  levels = c("0", "1", "2"), 
  ordered = TRUE
)
table(prl_quest_data_clean$group)
class(prl_quest_data_clean$group)

# remove males
prl_quest_data_clean$sex <- factor(
  stringr::str_sub(prl_quest_data_clean$subj_code, -1, -1)
)

prl_fem <- prl_quest_data_clean %>% 
  dplyr::filter(sex == "f")
table(prl_fem$group)

# Only two extreme groups: patients and controls
noatrisk <- prl_fem %>% 
  dplyr::filter(
    group != "1"
  )
noatrisk$group <- factor(noatrisk$group)
table(noatrisk$group)


is_patient <- noatrisk$is_patient
noatrisk$is_patient <- NULL
df_s <- standardize_num_vars(noatrisk)
df_s$is_patient <- is_patient
table(df_s$is_patient)


prior_ma <- prior(normal(0, 2), class = "b") + 
  prior(normal(0, 5), class = "Intercept")

m1 <- brm(
  is_patient ~ 
    oral_control + dieting + bulimia +
    bsq14_tot + ros_tot +
    sias + mps_ps + mps_o + mps_cmd + mps_pepc + orto_tot +
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

plot(conditional_effects(m1, "alpha_pos_social"))
plot(conditional_effects(m1, "oral_control"))


# All three groups: patients, at-risk, and controls

is_patient <- prl_fem$is_patient
prl_fem$is_patient <- NULL
df_all_s <- standardize_num_vars(prl_fem)
df_all_s$is_patient <- is_patient
table(df_all_s$is_patient)

m2 <- brm(
  is_patient ~ 
    # oral_control + dieting + bulimia +
    bsq14_tot + ros_tot +
    sias + mps_ps + mps_o + mps_cmd + mps_pepc + orto_tot +
    a_neither + v_food + v_social + t_food + t_social + z_food + z_social + 
    alpha_neg_food + alpha_neg_social + alpha_pos_food + alpha_pos_social, 
  data = df_all_s, 
  prior = prior_ma,
  family = bernoulli(),
  control = list(adapt_delta = 0.98),
  iter = 10000,
  cores = 6,
  backend = "cmdstan"
)
summary(m2)


# Ordinal regression with all three groups 

# It does not add much...
fit1 <- brm(
  group ~ 
    # oral_control + dieting + bulimia + 
    bsq14_tot + ros_tot +
    sias + mps_ps + mps_o + mps_cmd + mps_pepc + orto_tot + 
    a_neither + v_food + v_social + t_food + t_social + z_food + z_social + 
    alpha_neg_food + alpha_neg_social + alpha_pos_food + alpha_pos_social, 
  data = df_all_s, 
  prior = prior_ma,
  # family = sratio("cloglog"),
  family = cumulative("probit"),
  # family = acat("probit"),
  control = list(adapt_delta = 0.98),
  iter = 10000,
  cores = 6,
  backend = "cmdstan"
)

summary(fit1)

conditional_effects(fit1, "alpha_pos_social", categorical = TRUE)
conditional_effects(fit1, "bsq14_tot", categorical = TRUE)
conditional_effects(fit1, "sias", categorical = TRUE)






# e  n  d  


subj_info <- get_subj_info()

dd <- left_join(df_all_s, subj_info, by = "subj_code")


dd$ws <- ifelse(dd$ws > 50, NA, dd$ws)
hist(dd$ws)

hist(dd$rws)

prior_ma <- prior(normal(0, 2), class = "b") + 
  prior(normal(0, 5), class = "Intercept")

m9 <- brm(
   rws ~ group +  
    (a_neither + v_food + v_social + t_food + t_social + z_food + z_social +
    alpha_neg_food + alpha_neg_social + alpha_pos_food + alpha_pos_social +
    oral_control + dieting + bulimia +
    bsq14_tot + ros_tot +
    sias + mps_ps + mps_o + mps_cmd + mps_pepc + orto_tot),
  data = dd, 
  prior = prior_ma,
  family = zero_inflated_beta(),
  control = list(adapt_delta = 0.98),
  iter = 4000,
  cores = 6,
  backend = "cmdstan"
)
summary(m9)

conditional_effects(m9, "alpha_neg_social")
conditional_effects(m9, "alpha_pos_social")
conditional_effects(m9, "oral_control")

test_group2_right <- bayestestR::bayesfactor_parameters(m9, direction = ">")
test_group2_right
plot(test_group2_right)


hist(dd$ws)

dd %>% 
  group_by(group) %>% 
  summarise(
    avg_rws = mean(rws, na.rm = TRUE),
    avg_ws = mean(ws, na.rm = TRUE),
    n = n()
  )









foo <- readRDS(
  here("data", "processed", "quest", "raw_quest_data.rds")
)


quest_data <- readRDS(
  here::here("data", "processed", "quest", "quest_data.rds")
)

quest_data$group <- ifelse(
  quest_data$subj_code %in% patients_codes, "patient", "control"
)


df2_s <- df_s %>% 
  dplyr::filter(group != "control")

m2 <- brm(
  is_patient ~ 
    oral_control + dieting + bulimia +
    bsq14_tot + ros_tot +
    sias + mps_ps + mps_o + mps_cmd + mps_pepc + orto_tot +
    a_neither + v_food + v_social + t_food + t_social + z_food + z_social + 
    alpha_neg_food + alpha_neg_social + alpha_pos_food + alpha_pos_social, 
  data = df2_s, 
  prior = prior_ma,
  family = bernoulli(),
  control = list(adapt_delta = 0.98),
  iter = 10000,
  cores = 6,
  backend = "cmdstan"
)
summary(m2)





























prl_quest_data_clean %>% 
  group_by(is_patient) %>% 
  summarise(
    alpha_neg_food   = mean(alpha_neg_food, trim = 0.0, na.rm = TRUE),
    alpha_neg_social = mean(alpha_neg_social, trim = 0.0, na.rm = TRUE),
    alpha_pos_food   = mean(alpha_pos_food, trim = 0.0, na.rm = TRUE),
    alpha_pos_social = mean(alpha_pos_social, trim = 0.0, na.rm = TRUE),
    n = n()
  )

hist(prl_quest_data_clean$alpha_neg_food)

prl_quest_data_clean$alpha_neg_food <- ifelse(
  prl_quest_data_clean$alpha_neg_food < -4.0, 3.33306000, prl_quest_data_clean$alpha_neg_food
)
hist(prl_quest_data_clean$alpha_neg_food)


hist(prl_quest_data$alpha_neg_social)
hist(prl_quest_data$alpha_pos_social)


prl_quest_data_clean %>% 
  ggplot(
    aes(x = factor(is_patient), y = alpha_neg_food)
  ) +
  geom_violin() +
  geom_boxplot(width = .1, fill = "black", outlier.colour = NA) +
  stat_summary(fun.y = median, geom = "point", fill = "white", shape = 21, size = 2.5)

prl_quest_data_clean %>% 
  ggplot(
    aes(x = factor(is_patient), y = alpha_pos_food)
  ) +
  geom_violin() +
  geom_boxplot(width = .1, fill = "black", outlier.colour = NA) +
  stat_summary(fun.y = median, geom = "point", fill = "white", shape = 21, size = 2.5)

prl_quest_data_clean %>% 
  ggplot(
    aes(x = factor(is_patient), y = alpha_neg_social)
  ) +
  geom_violin() +
  geom_boxplot(width = .1, fill = "black", outlier.colour = NA) +
  stat_summary(fun.y = median, geom = "point", fill = "white", shape = 21, size = 2.5)

prl_quest_data_clean %>% 
  ggplot(
    aes(x = factor(is_patient), y = alpha_pos_social)
  ) +
  geom_violin() +
  geom_boxplot(width = .1, fill = "black", outlier.colour = NA) +
  stat_summary(fun.y = median, geom = "point", fill = "white", shape = 21, size = 2.5)

prl_quest_data$t_food <- ifelse(prl_quest_data$t_food > 0.35, 0.35, prl_quest_data$t_food)
prl_quest_data %>% 
  ggplot(
    aes(x = group, y = t_food)
  ) +
  geom_violin() +
  geom_boxplot(width = .1, fill = "black", outlier.colour = NA) +
  stat_summary(fun.y = median, geom = "point", fill = "white", shape = 21, size = 2.5)

prl_quest_data$t_social <- ifelse(prl_quest_data$t_social > 0.45, 0.45, prl_quest_data$t_social)
prl_quest_data %>% 
  ggplot(
    aes(x = group, y = t_social)
  ) +
  geom_violin() +
  geom_boxplot(width = .1, fill = "black", outlier.colour = NA) +
  stat_summary(fun.y = median, geom = "point", fill = "white", shape = 21, size = 2.5)

prl_quest_data$v_food <- ifelse(
  prl_quest_data$v_food < 0, 0.05, prl_quest_data$v_food
)
prl_quest_data %>% 
  ggplot(
    aes(x = group, y = v_food)
  ) +
  geom_violin() +
  geom_boxplot(width = .1, fill = "black", outlier.colour = NA) +
  stat_summary(fun.y = median, geom = "point", fill = "white", shape = 21, size = 2.5)

prl_quest_data$v_social <- ifelse(
  prl_quest_data$v_social > 5, 5, prl_quest_data$v_social
)
prl_quest_data %>% 
  ggplot(
    aes(x = group, y = v_social)
  ) +
  geom_violin() +
  geom_boxplot(width = .1, fill = "black", outlier.colour = NA) +
  stat_summary(fun.y = median, geom = "point", fill = "white", shape = 21, size = 2.5)


with(
  prl_quest_data_clean,
  cor(alpha_neg_food, oral_control, use = "complete.obs"),
)


with(
  prl_quest_data_clean,
  cor(alpha_pos_social, oral_control, use = "complete.obs"),
)
temp <- df1_s
temp$subj_idx <- NULL

df_long <- temp %>%
  pivot_longer(
    cols = c("a_neither", "v_food", "v_social", "t_food", "t_social", 
             "z_food", "z_social", "alpha_neg_food", "alpha_neg_social",
             "alpha_pos_food", "alpha_pos_social"),
    names_to = "params",
    # names_prefix = "rl",
    values_to = "value",
    values_drop_na = TRUE
  )



df_long <- temp %>%
  pivot_longer(
    cols = c("alpha_neg_food", "alpha_neg_social",
             "alpha_pos_food", "alpha_pos_social"),
    names_to = "alpha",
    # names_prefix = "rl",
    values_to = "value",
    values_drop_na = TRUE
  )

df_long$patient <- factor(df_long$is_patient)


m2 <- brm(
  value ~ is_patient * alpha + 
    # oral_control + dieting + bulimia +
    # bsq14_tot + ros_tot +
    # sias + mps_ps + mps_o + mps_cmd + mps_pepc + orto_tot +
    (1 + alpha | subj_code), 
  data = df_long, 
  prior = prior_ma,
  #family = bernoulli(),
  control = list(adapt_delta = 0.97),
  iter = 4000,
  cores = 6,
  backend = "cmdstan"
)
summary(m2)

bayes_R2(m1)

plot(
  conditional_effects(m1)
)








m1 <- brm(
  is_patient ~ 
    oral_control + dieting + bulimia +
    bsq14_tot + ros_tot +
    sias + mps_ps + mps_o + mps_cmd + mps_pepc + orto_tot +
    params +
    (1 + params | subj_code), 
  data = df_long, 
  prior = prior_ma,
  family = bernoulli(),
  control = list(adapt_delta = 0.97),
  iter = 4000,
  cores = 6,
  backend = "cmdstan"
)
summary(m1)





plot(conditional_effects(fit, "zBase:Trt"))



m1 <- brm(
  is_patient ~ 
    oral_control + dieting + bulimia +
    bsq14_tot + ros_tot +
    sias + mps_ps + mps_o + mps_cmd + mps_pepc + orto_tot +
    a_neither + v_food + v_social + t_food + t_social + z_food + z_social + 
    alpha_neg_food + alpha_neg_social + alpha_pos_food + alpha_pos_social, 
  data = df1_s, 
  prior = prior_ma,
  family = bernoulli(),
  control = list(adapt_delta = 0.98),
  iter = 10000,
  cores = 6,
  backend = "cmdstan"
)
summary(m1, prob = 0.95)

bayes_R2(m1)

library("rstanarm")
library("bayestestR")

model <- stan_glm(
  formula = is_patient ~ 
    oral_control + dieting + bulimia,
    # bsq14_tot + ros_tot +
    # sias + mps_ps + mps_o + mps_cmd + mps_pepc + orto_tot +
    # a_neither + v_food + v_social + t_food + t_social + z_food + z_social + 
    # alpha_neg_food + alpha_neg_social + alpha_pos_food + alpha_pos_social,
  data = df1_s,
  prior = normal(0, 3, autoscale = FALSE)
)

model <- stan_glm(
  formula = alpha_pos_food ~ is_patient,
  # bsq14_tot + ros_tot +
  # sias + mps_ps + mps_o + mps_cmd + mps_pepc + orto_tot +
  # a_neither + v_food + v_social + t_food + t_social + z_food + z_social + 
  # alpha_neg_food + alpha_neg_social + alpha_pos_food + alpha_pos_social,
  data = df1_s,
  prior = normal(0, 3, autoscale = FALSE)
)

f1 <- stan_mv(
  formula = list(
    # alpha_pos_food ~ is_patient,
    # alpha_pos_social ~ is_patient,
    alpha_neg_food ~ is_patient,
    alpha_neg_social ~ is_patient
    ),
  data = df1_s,
  # this next line is only to keep the example small in size!
  chains = 3, cores = 6, seed = 12345, iter = 4000)



test_group2_right <- bayesfactor_parameters(model, direction = "<")
test_group2_right
plot(test_group2_right)

My_first_BF <- bayesfactor_parameters(model, null = c(-1, 1))
My_first_BF

library("see")
plot(My_first_BF)
effectsize::interpret_bf(exp(My_first_BF$log_BF[2]), include_value = TRUE)

My_second_BF <- bayesfactor_parameters(model, null = 0)
My_second_BF




only_prl_data <- prl_quest_data %>% 
  dplyr::select(
    subj_code, a_neither, v_food, v_social, t_food, t_social, z_food, 
    z_social, alpha_neg_food, alpha_neg_social, alpha_pos_food, 
    alpha_pos_social
  )

# merge questionnaries subscales and PRL params for females
prl_params_quest_data <- left_join(
  quest_subscales, 
  only_prl_data, 
  by = "subj_code"
)
table(prl_params_quest_data$group)

df <- prl_params_quest_data %>% 
  drop_na()
table(df$group)
summary(df)

# standardize
df_s <- standardize_num_vars(prl_params_quest_data)
df_s$is_patient <- ifelse(df_s$group == "patient", 1, 0)

m1 <- brm(
  is_patient ~ 
    oral_control + dieting + bulimia +
    bsq14_tot + ros_tot +
    sias + mps_ps + mps_o + mps_cmd + mps_pepc + orto_tot +
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



library("lavaan")

mod <- '
  SPEED =~ NA*v_food + v_social + t_food + t_social
  ALPHA =~ NA*alpha_neg_food + alpha_neg_social + alpha_pos_food + alpha_pos_social
  MPS =~ NA*mps_ps + mps_o + mps_cmd + mps_pepc
  EAT =~ NA*oral_control + dieting + bulimia
  # ALPHA ~ SPEED + MPS + bsq14_tot + ros_tot + sias 
  ALPHA ~ EAT + SPEED + MPS + bsq14_tot + ros_tot + sias 
'

fit <- cfa(
  mod, 
  std.lv = TRUE, 
  orthogonal = FALSE, 
  data = prl_params_quest_data
)

summary(
  fit, 
  standardized=TRUE, 
  fit.measures=TRUE
)




# group as an ordinal variable
df_s <- dplyr::mutate(
  df_s,  
  group = 
    dplyr::recode_factor(
    group,   
    `1` = "control", 
    `2` = "at_risk",  
    `3` = "patient",   
    .ordered = TRUE
  )  
)









m1 <- brm(
  group ~ 
    # oral_control + dieting + bulimia + 
    # bsq14_tot + ros_tot +
    # sias + mps_ps + mps_o + mps_cmd + mps_pepc + orto_tot + 
    a_neither + v_food + v_social + t_food + t_social + z_food + z_social + 
    alpha_neg_food + alpha_neg_social + alpha_pos_food + alpha_pos_social, 
  data = df_s, 
  prior = prior_ma,
  family = cumulative("probit"),
  control = list(adapt_delta = 0.98),
  iter = 10000,
  cores = 6,
  backend = "cmdstan"
)

summary(m1)

m2 <- brm(
  alpha_neg_food ~ group, 
  data = df_s, 
  prior = prior_ma,
  family = skew_normal(),
  control = list(adapt_delta = 0.98),
  iter = 10000,
  cores = 6,
  backend = "cmdstan"
)

pp_check(m2) 
summary(m2)






fit1 <- brm(
  group ~ 
    # oral_control + dieting + bulimia + 
    bsq14_tot + ros_tot +
    sias + mps_ps + mps_o + mps_cmd + mps_pepc + orto_tot + 
    a_neither + v_food + v_social + t_food + t_social + z_food + z_social + 
    alpha_neg_food + alpha_neg_social + alpha_pos_food + alpha_pos_social, 
  data = df_s, 
  prior = prior_ma,
  # family = sratio("cloglog"),
  family = cumulative("probit"),
  # family = acat("probit"),
  control = list(adapt_delta = 0.98),
  iter = 10000,
  cores = 6,
  backend = "cmdstan"
)

summary(fit1)

conditional_effects(fit1, "alpha_neg_food", categorical = TRUE)
conditional_effects(fit1, "oral_control", categorical = TRUE)
conditional_effects(fit1, "bulimia", categorical = TRUE)
conditional_effects(fit1, "sias", categorical = TRUE)
conditional_effects(fit1, "t_food", categorical = TRUE)
conditional_effects(fit1, "alpha_neg_food", categorical = TRUE)

pp_check(fit1)  # shows dens_overlay plot by default
pp_check(fit1, type = "stat_2d")

fit2 <- brm(
  group ~ 
    sias + 
    t_food + 
    alpha_neg_food, 
  data = df_s, 
  prior = prior_ma,
  # family = sratio("cloglog"),
  family = cumulative("probit"),
  # family = acat("probit"),
  control = list(adapt_delta = 0.98),
  iter = 10000,
  cores = 6,
  backend = "cmdstan"
)

summary(fit2)











fit1 <- brm(
  formula = bf(group ~ 1 + alpha_neg_food) +
    lf(disc ~ 0 + alpha_neg_food, cmc = FALSE),
  data = df_s, 
  prior = prior_ma,
  # family = sratio("cloglog"),
  family = cumulative("probit"),
  # family = acat("probit"),
  control = list(adapt_delta = 0.97),
  iter = 4000,
  cores = 6,
  backend = "cmdstan"
)

summary(fit1)







hist(df_s$alpha_neg_food)
hist(df_s$alpha_neg_social)
hist(df_s$alpha_pos_food)
hist(df_s$alpha_pos_social)

bf_alpha_neg_food   <- bf(alpha_neg_food   ~ is_patient)
bf_alpha_neg_social <- bf(alpha_neg_social ~ is_patient) 
bf_alpha_pos_food   <- bf(alpha_pos_food   ~ is_patient) 
bf_alpha_pos_social <- bf(alpha_pos_social ~ is_patient) 

fit_alphas <- brm(
  bf_alpha_neg_food + bf_alpha_neg_social + bf_alpha_pos_food + 
    bf_alpha_pos_social + set_rescor(FALSE), 
  family = skew_normal(),
  data = df_s, 
  control = list(adapt_delta = 0.97),
  iter = 4000,
  cores = 6,
  backend = "cmdstan"
)

summary(fit_alphas)









# patients vs controls

pat_cnt_df <- df_s %>% 
  dplyr::filter(group != "at_risk")

priors <- c(
  set_prior("normal(0, 10)", class = "b"),
  set_prior("normal(0, 5)", class = "Intercept")
)

noatrisk <- df_s %>% 
  dplyr::filter(
    group != "at_risk"
  )

m1 <- brm(
  is_patient ~ 
    # oral_control + dieting + bulimia + bsq14_tot + ros_tot +
    # sias + mps_ps + mps_o + mps_cmd + mps_pepc + orto_tot + 
    a_neither + v_food + v_social + t_food + t_social + z_food + z_social + 
    alpha_neg_food + alpha_neg_social + alpha_pos_food + alpha_pos_social, 
  data = noatrisk, 
  family = bernoulli(),
  prior = priors,
  control = list(adapt_delta = 0.97),
  iter = 4000,
  cores = 6,
  backend = "cmdstan"
)

summary(m1)


df_s %>% 
  group_by(group) %>% 
  summarise(
    avg_alpha_neg_food = mean(alpha_neg_food, na.rm = TRUE), 
    avg_alpha_neg_social = mean(alpha_neg_social, na.rm = TRUE), 
    avg_alpha_pos_food = mean(alpha_pos_food, na.rm = TRUE), 
    avg_alpha_pos_social = mean(alpha_pos_social, na.rm = TRUE), 
    n = n()
  )


#use the `predict()` function to calculate the predicted probabilities of pupils in the original data from the fitted model
pred <- predict(m1, type = "response")
pred <- if_else(pred[,1] > 0.5, 1, 0)
confusion_matrix <- table(pred, pull(df_s, is_patient)) #`pull` results in a vector
#correct classification rate
sum(diag(confusion_matrix)) / sum(confusion_matrix)

# Compute AUC for predicting Class with the model
prob <- predict(m1, type="response")
prob <- prob[,1]
pred <- prediction(prob, as.vector(pull(df_s, is_patient)))
AUC <- performance(pred, measure = "auc")
AUC <- AUC@y.values[[1]]
AUC

stanplot(
  m1, 
  pars = "^b_",
  type = "areas",
  prob = 0.95
)

hypothesis(m1, "alpha_neg_food > 0")

plot(
  conditional_effects(
    m1, 
    effects = "alpha_neg_food",
    re_formula = NULL),
  points = FALSE, 
  rug = FALSE
)


hist(df$alpha_neg_food)

priors <- c(
  set_prior("normal(0, 5)", class = "b"),
  set_prior("normal(0, 5)", class = "Intercept")
)

m2 <- brm(
  alpha_neg_food ~ group,
  data = df_s, 
  family = skew_normal(),
  prior = priors,
  control = list(adapt_delta = 0.97),
  iter = 4000,
  backend = "cmdstan"
)

stanplot(
  m2, 
  pars = "^b_",
  type = "areas",
  prob = 0.95
)

summary(m2)

no_atrisk <- df_s %>% 
  dplyr::filter(
    group != "at_risk"
  )


m2 <- brm(
  is_patient ~ oral_control + dieting + bulimia + bsq14_tot + ros_tot +
    sias + mps_ps + mps_o + mps_cmd + mps_pepc + orto_tot + 
    a_neither + v_food + v_social + t_food + t_social + z_food + z_social + 
    alpha_neg_food + alpha_neg_social + alpha_pos_food + alpha_pos_social, 
  data = no_atrisk, 
  family = bernoulli(),
  prior = priors,
  control = list(adapt_delta = 0.97),
  iter = 4000,
  backend = "cmdstan"
)

summary(m2)


pat_cnt_df <- df %>% 
  dplyr::filter(group != "control")

pat_cnt_df_s <- standardize_num_vars(pat_cnt_df)
pat_cnt_df_s$is_patient <- ifelse(pat_cnt_df_s$group == "patient", 1, 0)

m2 <- brm(
  is_patient ~ oral_control + dieting + bulimia + bsq14_tot + ros_tot +
    sias + mps_ps + mps_o + mps_cmd + mps_pepc + orto_tot + a_neither + 
    v_food + v_social + t_food + t_social + z_food + z_social + 
    alpha_neg_food + alpha_neg_social + alpha_pos_food + alpha_pos_social, 
  data = pat_cnt_df_s, 
  family = bernoulli(link = "logit"),
  iter = 40000,
  backend = "cmdstan"
)

summary(m2)
stanplot(m2, 
         type = "areas",
         prob = 0.95)


prior3 <- c(
  set_prior("normal(0, 1)", class = "b"),
  set_prior("normal(0,5)", class = "b", coef = "intercept" )
)

m2 <- brm(
  is_patient ~ v_food + v_social + t_food + t_social + z_food + z_social + 
    alpha_neg_food + alpha_neg_social + alpha_pos_food + alpha_pos_social, 
  data = pat_cnt_df_s, 
  family = bernoulli(link = "logit"),
  iter = 4000,
  backend = "cmdstan"
)







bayes_R2(m1)
m01 <- brm(
  bsq14_tot ~ 1, 
  data = atriskpat_s, 
  family = skew_normal(),
  iter = 40000,
  save_pars = save_pars(all = TRUE)
)
comparison <- bayestestR::bayesfactor(m01, m1)
comparison
bayestestR::bayesfactor(comparison)










quest_fem %>% 
  group_by(group) %>% 
  summarise(
    avg = mean(eat26_tot, na.rm = TRUE), 
    n = n()
  )

quest_clean <- quest_fem %>% 
  drop_na(group)

quest_clean %>% 
  group_by(group) %>% 
  summarise(
    avg = mean(eat26_tot, na.rm = TRUE), 
    n = n()
  )

subj_code_patients <- quest_clean %>% 
  dplyr::filter(group == "patient") %>% 
  dplyr::select(subj_code)

quest_clean$is_patient <- ifelse(
  quest_clean$subj_code %in% subj_code_patients$subj_code, 1, 0)

table(quest_clean$is_patient)

summary(lm(eat26_tot ~ group, quest_clean))
summary(lm(oral_control ~ group, quest_clean))
summary(lm(dieting ~ group, quest_clean))
summary(lm(bulimia ~ group, quest_clean))

summary(lm(bsq14_tot ~ group, quest_clean))

summary(lm(ros_tot ~ group, quest_clean))

summary(lm(dass21_stress ~ group, quest_clean))
summary(lm(dass21_anxiety ~ group, quest_clean))
summary(lm(dass21_dep ~ group, quest_clean))

summary(lm(sias ~ group, quest_clean))

summary(lm(sias ~ group, quest_fem))

summary(lm(mps_ps ~ group, quest_clean))
summary(lm(mps_o ~ group, quest_clean))
summary(lm(mps_cmd ~ group, quest_clean))
summary(lm(mps_pepc ~ group, quest_clean))

summary(lm(orto_tot ~ group, quest_clean))

fm <- glm(
  is_patient ~ oral_control + dieting + bulimia + 
    bsq14_tot +
    ros_tot + sias + mps_ps + mps_o + mps_cmd + mps_pepc +
    orto_tot,
  family = binomial(link = "logit"),
  data = quest_clean
)

summary(fm)
# Make predictions
probabilities <- fm %>% 
  predict(quest_clean, type = "response")
predicted.classes <- ifelse(probabilities > 0.5, 1, 0)
# Model accuracy
mean(predicted.classes == quest_clean$is_patient, na.rm = TRUE)


test_prob <- predict(fm, newdata = quest_clean, type = "response")
test_roc = roc(quest_clean$is_patient ~ test_prob, plot = TRUE, print.auc = TRUE)

table(quest_clean$is_patient)

# Only at-risk vs patients

atriskpat <- quest_clean %>% 
  dplyr::filter(group != "control")

fm <- glm(
  is_patient ~ oral_control + dieting + bulimia + 
    bsq14_tot + ros_tot + sias + mps_ps + mps_o + 
    mps_cmd + mps_pepc + orto_tot,
  family = binomial(link = "logit"),
  data = atriskpat
)

test_prob <- predict(fm, newdata = atriskpat, type = "response")
test_roc = roc(atriskpat$is_patient ~ test_prob, plot = TRUE, print.auc = TRUE)


# standardize
is_patient_var <- atriskpat$is_patient
atriskpat$is_patient <- NULL
atriskpat_s <- standardize_num_vars(atriskpat)
atriskpat_s$is_patient <- is_patient_var

fit1 <- brm(
  is_patient ~ oral_control + dieting + bulimia + 
    bsq14_tot + ros_tot + sias + mps_ps + mps_o + 
    mps_cmd + mps_pepc + orto_tot, 
  data = atriskpat_s, 
  family = bernoulli(link = "logit"), 
  backend = "cmdstan"
)
summary(fit1)

fit2 <- brm(
  is_patient ~ oral_control + dieting + bulimia, 
  data = atriskpat_s, 
  family = bernoulli(link = "logit"), 
  backend = "cmdstan"
)

loo(fit1, fit2)
#    elpd_diff se_diff
# fit2   0.0       0.0  
# fit1 -11.1       2.8  
# Even if they are weak, the variables other than EAT-26 do contribute to
# the classification patients/at-risk


fit0 <- brm(
  is_patient ~ 1, 
  data = atriskpat_s, 
  family = bernoulli(link = "logit"), 
  backend = "cmdstan"
)

loo(fit1, fit0)

plot(conditional_effects(fit1, effects = "oral_control"))
plot(conditional_effects(fit1, effects = "dieting"))

summary(fit1)

hist(atriskpat_s$bsq14_tot)
m1 <- brm(
  bsq14_tot ~ is_patient, 
  data = atriskpat_s, 
  family = skew_normal(),
  iter = 40000,
  save_pars = save_pars(all = TRUE)
)
summary(m1)
bayes_R2(m1)
m01 <- brm(
  bsq14_tot ~ 1, 
  data = atriskpat_s, 
  family = skew_normal(),
  iter = 40000,
  save_pars = save_pars(all = TRUE)
)
comparison <- bayestestR::bayesfactor(m01, m1)
comparison
bayestestR::bayesfactor(comparison)



# exists("fit1")  
# mod <- cmdstanr::cmdstan_model(temp_stan)



# read PRL params and quest data

prl_quest_data <- readRDS(
  here("data", "processed", "prl", "prl_and_quest", 
       "prl_params_and_quest_data.rds")
)


atriskpat2 <- prl_quest_data %>% 
  dplyr::filter(group != "at_risk")


fm7 <- glm(
  is_patient ~ a_neither + v_food + v_social + t_food + t_social +
    z_food + z_social + alpha_neg_food + alpha_neg_social + 
    alpha_pos_food + alpha_pos_social,
  family = binomial(link = "logit"),
  data = atriskpat2
)

test_prob <- predict(fm7, newdata = atriskpat2, type = "response")
test_roc = roc(atriskpat2$is_patient ~ test_prob, plot = TRUE, print.auc = TRUE)






params_quest_prl %>% 
  group_by(group) %>% 
  summarise(
    avg_alpha_pos_food = median(alpha_pos_food, na.rm = TRUE),
    avg_alpha_pos_social = median(alpha_pos_social, na.rm = TRUE),
    avg_alpha_neg_food = median(alpha_neg_food, na.rm = TRUE),
    avg_alpha_neg_social = median(alpha_neg_social, na.rm = TRUE),
    n = n()
  )

params_quest_prl %>% 
  group_by(group) %>% 
  summarise(
    avg_a = median(a_neither, na.rm = TRUE),
    avg_v_food = median(v_food, na.rm = TRUE),
    avg_v_social = median(v_social, na.rm = TRUE),
    avg_t_food = median(t_food, na.rm = TRUE),
    avg_t_social = median(t_social, na.rm = TRUE),
    avg_z_food = median(z_food, na.rm = TRUE),
    avg_z_social = median(z_social, na.rm = TRUE),
    n = n()
  )



quest <- readRDS(
  here("data", "processed", "quest", "raw_quest_data.rds")
)

weight_history <- quest[, 10:27] %>% 
  as.data.frame()
weight_history[, 1]


d <- only_prl_data %>% 
  na.omit()

factanal(d[, c(3:6, 9:12)], factors = 2)

psych::omega(d[, c(3:6)])
psych::omega(d[, c(9:12)])



