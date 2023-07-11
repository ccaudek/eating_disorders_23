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


quest_data <- readRDS(
  here::here("data", "processed", "quest", "quest_diagn_cat_3grps.rds")
) |> 
  distinct() |> 
  dplyr::select(-diag_cat)

params_data = rio::import(
  here::here(
    "data",
    "processed",
    "prl",
    "subj_hddm_params",
    "subj_code_hddm_params.csv"
  )
)

d <- left_join(params_data, quest_data, by = "subj_code")
length(unique(d$subj_code))



#+ echo=FALSE, fig.width=7
p1 <- d %>% 
  ggplot(aes(x=ws, group=diag_cat, fill=diag_cat)) +
  geom_density(adjust=1.5, alpha=.4) +
  labs(
    x = "Weight hystory"
    ) +
  papaja::theme_apa()
p1

#+ echo=FALSE, fig.width=7
p2 <- d %>%
  ggplot(aes(x=rws, group=diag_cat, fill=diag_cat)) +
  geom_density(adjust=1.5, alpha=.4) +
  labs(
    x = "Relative weight hystory"
  ) +
  papaja::theme_apa()
p2


#' The at-risk group is very similar to the control group, and both differs from the patient group.



#+ echo=FALSE, fig.width=7
p4 <- d %>% 
  ggplot(aes(x=bmi, group=diag_cat, fill=diag_cat)) +
  geom_density(adjust=2.5, alpha=.4) +
  labs(
    x = "Body Mass Index"
  ) +
  papaja::theme_apa()
p4

d1 <- d |> 
  dplyr::select(
  diag_cat, subj_code, age, stim, par, val, rws, bmi
  )
d1$age <- as.numeric(d1$age)

d1$rws <- ifelse(d1$rws == 0, NA, d1$rws)

d2 <- d1 %>%
  pivot_wider(names_from = par, values_from = val)

imputed_data <- mice(d2, m=1, maxit=50, meth='pmm', seed=123)
d3 <- complete(imputed_data)



m1 <- brm(
  rws ~ diag_cat + bmi + 
    stim * (a + v + t + alpha + pos_alpha) + (stim | subj_code),
  data = d3, 
  family = skew_normal(),
  control = list(adapt_delta = 0.98),
  iter = 2000,
  cores = 3,
  backend = "cmdstanr"
)
summary(m1)

m1 <- lmer(
  rws ~ stim * (a + v + t + alpha + pos_alpha) + (1 | subj_code),
  data = d3
)



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







