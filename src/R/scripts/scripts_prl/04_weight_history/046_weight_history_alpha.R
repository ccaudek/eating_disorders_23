#' ---
#' title: "Association between weight history and PRL params"
#' author: "[Corrado Caudek](https://ccaudek.github.io/)"
#' date: "First version 08:21:00 2021. Last modified `r format(Sys.time(), '%Y-%m-%d')`"
#' output:
#'   pdf_document:
#'     keep_tex: true
#' ---
#'
#' Purpose: to determine whether the PRL hDDMrl parameters can be associated
#' to the behavioral characteristics of participants, after accounting for the
#' individual differences explained by the questionnaires data.

#+ echo=FALSE
library("here")
suppressPackageStartupMessages(library("tidyverse"))
suppressPackageStartupMessages(library("forcats"))
suppressPackageStartupMessages(library("readxl"))
suppressPackageStartupMessages(library("pROC"))
suppressPackageStartupMessages(library("brms"))
suppressPackageStartupMessages(library("cmdstanr"))
set_cmdstan_path("/Users/corrado/cmdstan")
suppressPackageStartupMessages(library("ROCR"))
suppressPackageStartupMessages(library("tidybayes"))

#+ echo=FALSE
source(here::here("lib", "ed_fnc.R"))

# read questionnaires data
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


# All three groups: patients, at-risk, and controls
is_patient <- prl_fem$is_patient
prl_fem$is_patient <- NULL



df_all_s <- standardize_num_vars(prl_fem)
df_all_s$is_patient <- is_patient
table(df_all_s$is_patient)


subj_info <- get_subj_info()

dd <- left_join(df_all_s, subj_info, by = "subj_code")

dd$ws <- ifelse(dd$ws > 50, NA, dd$ws)

dd$groups <- factor(ifelse(dd$is_patient == 1, "patient", "control"))

#+ echo=FALSE, fig.width=7
p1 <- dd %>% 
  ggplot(aes(x=ws, group=groups, fill=groups)) +
  geom_density(adjust=1.5, alpha=.4) +
  labs(
    x = "Weight hystory"
    ) +
  papaja::theme_apa()
p1

#+ echo=FALSE, fig.width=7
p2 <- dd %>% 
  ggplot(aes(x=rws, group=groups, fill=groups)) +
  geom_density(adjust=1.5, alpha=.4) +
  labs(
    x = "Relative weight hystory"
  ) +
  papaja::theme_apa()
p2

#' Split participants in patients, at-risk, and control

dd$Group <- factor(
  ifelse(dd$group == 0, "control", ifelse(dd$group == 1, "at risk", "patient")))

foo <- dd[!is.na(dd$Group), ]
table(foo$Group)


#+ echo=FALSE, fig.width=7
p3 <- foo %>% 
  ggplot(aes(x=rws, group=Group, fill=Group)) +
  geom_density(adjust=2.5, alpha=.4) +
  labs(
    x = "Relative weight hystory"
  ) +
  papaja::theme_apa()
p3

#' The at-risk group is very similar to the control group, and both differs from the patient group.

# Compute BMI
foo$BMI <- foo$present_weight / (foo$height/100)^2
foo$bmi <- as.numeric(scale(foo$BMI))


#+ echo=FALSE, fig.width=7
p4 <- foo %>% 
  ggplot(aes(x=BMI, group=groups, fill=groups)) +
  geom_density(adjust=2.5, alpha=.4) +
  labs(
    x = "Body Mass Index"
  ) +
  papaja::theme_apa()
p4


#+ echo=FALSE, fig.width=7
p5 <- foo %>% 
  ggplot(aes(x=BMI, group=Group, fill=Group)) +
  geom_density(adjust=2.5, alpha=.4) +
  labs(
    x = "Body Mass Index"
  ) +
  papaja::theme_apa()
p5


# Deviation Coding
contrasts(foo$Group) = contr.sum(3)
contrasts(foo$Group)



prior_ma <- prior(normal(0, 2), class = "b") + 
  prior(normal(0, 5), class = "Intercept")

m9 <- brm(
  rws ~ Group + bmi + 
    (a_neither + v_food + v_social + t_food + t_social + z_food + z_social +
       alpha_neg_food + alpha_neg_social + alpha_pos_food + alpha_pos_social +
       oral_control + dieting + bulimia +
       bsq14_tot + ros_tot +
       sias + mps_ps + mps_o + mps_cmd + mps_pepc + orto_tot),
  data = foo, 
  prior = prior_ma,
  family = zero_inflated_beta(),
  control = list(adapt_delta = 0.98),
  iter = 4000,
  cores = 6,
  backend = "cmdstan"
)
summary(m9)

# conditional_effects(m9, "alpha_neg_social")
c_eff <- conditional_effects(m9, "alpha_neg_social")
my_plot <- plot(c_eff, plot = FALSE)[[1]] +
  labs(
    x = "Alpha punishment (social pressure)",
    y = "Relative weight suppression"
  ) +
  papaja::theme_apa()
my_plot

# conditional_effects(m9, "alpha_pos_social")
c_eff <- conditional_effects(m9, "alpha_pos_social")
my_plot <- plot(c_eff, plot = FALSE)[[1]] +
  labs(
    x = "Alpha reward (social pressure)",
    y = "Relative weight suppression"
  ) +
  papaja::theme_apa()
my_plot

# conditional_effects(m9, "oral_control")
c_eff <- conditional_effects(m9, "oral_control")
my_plot <- plot(c_eff, plot = FALSE)[[1]] +
  labs(
    x = "EAT-26: Oral Control",
    y = "Relative weight suppression"
  ) +
  papaja::theme_apa()
my_plot

# conditional_effects(m9, "dieting")
c_eff <- conditional_effects(m9, "dieting")
my_plot <- plot(c_eff, plot = FALSE)[[1]] +
  labs(
    x = "EAT-26: Dieting",
    y = "Relative weight suppression"
  ) +
  papaja::theme_apa()
my_plot

test_group2_right <- bayestestR::bayesfactor_parameters(m9, direction = ">")
test_group2_right
plot(test_group2_right)



#' Marginal effects
#' 
#' Alpha punishment is not a robust effect.

#' Alpha reward
m10 <- brm(
  rws ~ alpha_pos_social,
  data = foo, 
  prior = prior_ma,
  family = zero_inflated_beta(),
  control = list(adapt_delta = 0.98),
  iter = 4000,
  cores = 6,
  backend = "cmdstan"
)
summary(m10)


c_eff <- conditional_effects(m10, "alpha_pos_social")
my_plot <- plot(c_eff, plot = FALSE)[[1]] +
  labs(
    x = "Alpha reward (social pressure)",
    y = "Relative weight suppression"
  ) +
  papaja::theme_apa()
my_plot

test_right <- bayestestR::bayesfactor_parameters(m10, direction = ">")
test_right
plot(test_right)


#' Oral control
m11 <- brm(
  rws ~ oral_control,
  data = foo, 
  prior = prior_ma,
  family = zero_inflated_beta(),
  control = list(adapt_delta = 0.98),
  iter = 4000,
  cores = 6,
  backend = "cmdstan"
)
summary(m11)


c_eff <- conditional_effects(m11, "oral_control")
my_plot <- plot(c_eff, plot = FALSE)[[1]] +
  labs(
    x = "Oral control",
    y = "Relative weight suppression"
  ) +
  papaja::theme_apa()
my_plot

test_right <- bayestestR::bayesfactor_parameters(m11, direction = ">")
test_right

#' Dieting
m12 <- brm(
  rws ~ dieting,
  data = foo, 
  prior = prior_ma,
  family = zero_inflated_beta(),
  control = list(adapt_delta = 0.98),
  iter = 4000,
  cores = 6,
  backend = "cmdstan"
)
summary(m12)


c_eff <- conditional_effects(m12, "dieting")
my_plot <- plot(c_eff, plot = FALSE)[[1]] +
  labs(
    x = "Dieting",
    y = "Relative weight suppression"
  ) +
  papaja::theme_apa()
my_plot

test_right <- bayestestR::bayesfactor_parameters(m12, direction = ">")
test_right


#' Bulimia
m13 <- brm(
  rws ~ bulimia,
  data = foo, 
  prior = prior_ma,
  family = zero_inflated_beta(),
  control = list(adapt_delta = 0.98),
  iter = 4000,
  cores = 6,
  backend = "cmdstan"
)
summary(m13)


c_eff <- conditional_effects(m13, "bulimia")
my_plot <- plot(c_eff, plot = FALSE)[[1]] +
  labs(
    x = "Bulimia",
    y = "Relative weight suppression"
  ) +
  papaja::theme_apa()
my_plot

test_right <- bayestestR::bayesfactor_parameters(m13, direction = ">")
test_right



#' Difference between the present weight and the predicted weight that the participant
#' expect, if she/he does not try to control her/his eating behavior

foo$predicted_weight <- recode_predicted_weight(foo)

imp <- mice::mice(foo, method = "mean", m = 1) # Impute data
data_imp <- complete(imp) # Store data

#' Difference between predicted weight and actual weight
data_imp$pred_weight_dif <- data_imp$predicted_weight - data_imp$present_weight

data_imp %>% 
  group_by(Group) %>% 
  summarise(
    avg_dif = mean(pred_weight_dif, trim = 0.1)
  )

hist(data_imp$pred_weight_dif)

# Control as baseline group
data_imp$Group <- relevel(data_imp$Group, ref = "control")

#' Predicted weight difference
m14 <- brm(
  pred_weight_dif ~ Group,
  data = data_imp, 
  prior = prior_ma,
  family = student(),
  control = list(adapt_delta = 0.98),
  iter = 4000,
  cores = 6,
  backend = "cmdstan"
)
summary(m14)

c_eff <- conditional_effects(m14, "Group")
my_plot <- plot(c_eff, plot = FALSE)[[1]] +
  labs(
    x = "Group",
    y = "Predicted weight difference"
  ) +
  papaja::theme_apa()
my_plot

test_right <- bayestestR::bayesfactor_parameters(m14, direction = ">")
test_right

#' Patients expect to gain more weight than controls; there is no evidence of
#' a difference between at-risk participants and controls.


#' Summary statistics
foo %>% 
  group_by(Group) %>% 
  summarise(
    avg_rws = mean(rws, na.rm = TRUE),
    avg_ws = mean(ws, na.rm = TRUE),
    n = n()
  )







