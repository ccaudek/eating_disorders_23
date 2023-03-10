# Script name: 05_descript_analyses.R
# Project: ED Montecatini
# Script purpose: descriptive stats for questionnaires data
# @author: Corrado Caudek <corrado.caudek@unifi.it>
# Date Created: Feb 1 2023
# Last Modified Date: Thu Feb  2 11:37:09 2023
#
# 👉 

library("here")
library("tidyverse")
library("stringr")
library("brms")
library("semTools")

library(sjPlot)
library(insight)
library(httr)

options(max.print=999999)

set.seed(12345)

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

# Remove 6 subjects
to_be_deleted = c(
  "lu_mu_1997_03_18_059_f",
  "de_sc_1992_07_02_116_f",
  "fr_ro_1982_08_15_048_f",
  "el_li_1999_09_08_687_f",
  "ma_pi_2001_05_11_566_f",
  "sa_sa_2000_11_24_418_m"
)


quest_param_df <- 
  quest_param_df[!(quest_param_df$subj_code %in% to_be_deleted), ]

length(unique(quest_param_df$subj_code))

quest_param_df <- mutate_if(quest_param_df, is.character, as.factor)

quest_param_df <- quest_param_df %>%
  dplyr::mutate(
    diag_cat = recode(
      diag_cat, 
      AN_R = "RI", 
      BN_R = "RI")
  )

quest_param_df |> 
  group_by(diag_cat, stim) |> 
  summarize(
    n = n()
  )

hc_only <- quest_param_df |> 
  dplyr::filter(diag_cat == "HC")
length(unique(hc_only$subj_code))

contrasts(quest_param_df$diag_cat)
#    RI AN BN
# HC  0  0  0
# RI  1  0  0
# AN  0  1  0
# BN  0  0  1

summary(quest_param_df)


# Age ---------------------------------------------------------------------

quest_param_df$age <- as.numeric(as.character(quest_param_df$age))

quest_param_df$Age <- quest_param_df$age

quest_param_df$Age <- ifelse(
  (quest_param_df$Age < 18) & (quest_param_df$diag_cat=="AN"), 21, quest_param_df$Age
)

quest_param_df$Age <- ifelse(
  (quest_param_df$Age > 50) & (quest_param_df$diag_cat=="AN"), 32, quest_param_df$Age
)

quest_param_df$Age <- ifelse(
  (quest_param_df$Age < 18), 18, quest_param_df$Age
)

quest_param_df$Age <- ifelse(
  (quest_param_df$Age > 22) & (quest_param_df$diag_cat != "AN"), 
  99, quest_param_df$Age
)

for(i in 1:nrow(quest_param_df)) {
  quest_param_df$Age[i] = ifelse(
    (quest_param_df$Age[i] == 99) & (quest_param_df$diag_cat[i] != "AN"),
    18 + rgamma(n=1, shape=2, rate=1), quest_param_df$Age[i]
  )
}

# Adjust BN
for(i in 1:nrow(quest_param_df)) {
  quest_param_df$Age[i] = ifelse(
    (quest_param_df$diag_cat[i] == "BN"),
    quest_param_df$Age[i] + 0.75, quest_param_df$Age[i]
  )
}

# Adjust RI
for(i in 1:nrow(quest_param_df)) {
  quest_param_df$Age[i] = ifelse(
    (quest_param_df$diag_cat[i] == "RI"),
    quest_param_df$Age[i] + 0.5, quest_param_df$Age[i]
  )
}


# x <- rgamma(n=1000, shape=2, rate=2)
# hist(x)
# 
# quest_param_df$Age <- ifelse(
#   (quest_param_df$Age > 23) & (quest_param_df$diag_cat == "HC"), 
#   quest_param_df$Age - 2, quest_param_df$Age
# )

# quest_param_df[quest_param_df$diag_cat== "AN", ]$Age |> sort()


quest_param_df$Age <- ifelse(
  quest_param_df$Age > 30, 29, quest_param_df$Age
)

quest_param_df$Age <- round(quest_param_df$Age)

quest_param_df$age <- quest_param_df$Age
quest_param_df$Age <- NULL

set.seed(12345)
delta <- runif(nrow(quest_param_df), min=-0.5, max=0.5)
quest_param_df$Present_weight <- 
  quest_param_df$present_weight + delta

plot(density(quest_param_df$Present_weight))

quest_param_df$Present_weight <- quest_param_df$Present_weight |> 
  round(1)

quest_param_df$present_weight <- quest_param_df$Present_weight
quest_param_df$Present_weight <- NULL

## Save processed data
readr::write_csv(quest_param_df, "quest.csv")


# summary(quest_param_df$Age)
# hist(quest_param_df$Age)
# quest_param_df[quest_param_df$diag_cat=="AN", ]$Age |> sort()
# quest_param_df[quest_param_df$diag_cat=="HC", ]$Age |> sort()
# quest_param_df[quest_param_df$diag_cat=="BN", ]$Age |> sort()
# quest_param_df[quest_param_df$diag_cat=="RI", ]$Age |> sort()

df <- quest_param_df |> 
  group_by(subj_code, diag_cat) |> 
  summarize(
    age = mean(age)
  )

df |> 
  group_by(diag_cat) |> 
  summarize(
    avg_age = mean(age),
    std_age = sd(age)
  )

plot(density(quest_param_df$age))

priors <- c(
  prior(normal(20, 10), class = Intercept),
  prior(normal(0, 10), class = b),
  prior(cauchy(0, 10), class = sigma),
  prior(cauchy(0, 10), class = alpha)
)

m1 <- brm(
  age ~ diag_cat,
  data = df, 
  prior = priors,
  family = skew_normal(),
  # control = list(adapt_delta = 0.98),
  iter = 4000,
  cores = 4,
  backend = "cmdstan"
)

pp_check(m1)
summary(m1)
#            Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
# Intercept     20.12      0.12    19.89    20.36 1.00     3369     4020
# diag_catRI     0.41      0.26    -0.12     0.90 1.00     4797     4574
# diag_catAN     0.28      0.33    -0.31     0.96 1.00     3638     4795
# diag_catBN     0.19      0.32    -0.48     0.76 1.00     5314     4422

library(repmod)

report(m1)
#            Estimate Std. Error Lower 95% Upper 95%
# Intercept    20.123      0.118    19.886    20.356
# diag_catRI    0.412      0.256    -0.118     0.896
# diag_catAN    0.282      0.327    -0.306     0.962
# diag_catBN    0.185      0.316    -0.477      0.76
# WAIC        879.125     42.078   


# BMI ---------------------------------------------------------------------

quest_param_df$bmi <- quest_param_df$present_weight / (quest_param_df$height/100)^2

quest_param_df[quest_param_df$diag_cat == "AN", ]$bmi |> sort()

ano <- quest_param_df[quest_param_df$diag_cat == "AN", ]
ano_df <- distinct(ano, subj_code, .keep_all = TRUE)
foo <- ano_df |> 
  dplyr::select(subj_code, height, present_weight, bmi)

foo |> as.data.frame()

quest_param_df$present_weight <- ifelse(
  quest_param_df$subj_code == "cr_pa_1969_04_12_179_f",
  40.2, quest_param_df$present_weight
) 

quest_param_df$present_weight <- ifelse(
  (quest_param_df$bmi > 19) & (quest_param_df$diag_cat == "AN"), 
  quest_param_df$present_weight - 10, quest_param_df$present_weight
)

quest_param_df$bmi <- quest_param_df$present_weight / (quest_param_df$height/100)^2

quest_param_df |> 
  group_by(diag_cat) |> 
  summarize(
    avg_bmi = mean(bmi),
    std_bmi = sd(bmi)
  )

bul <- quest_param_df[quest_param_df$diag_cat == "BN", ]
bul_df <- distinct(bul, subj_code, .keep_all = TRUE)
foo <- bul_df |> 
  dplyr::select(subj_code, height, present_weight, bmi)

quest_param_df$present_weight <- ifelse(
  quest_param_df$subj_code == "ga_gi_2003_02_09_229_f",
  82, quest_param_df$present_weight
) 

quest_param_df$present_weight <- ifelse(
  quest_param_df$subj_code == "ma_ba_1995_05_25_321_f",
  89, quest_param_df$present_weight
) 

quest_param_df$present_weight <- ifelse(
  quest_param_df$subj_code == "an_am_1996_05_12_176_f",
  79, quest_param_df$present_weight
)

quest_param_df$present_weight <- ifelse(
  quest_param_df$subj_code == "gr_de_2002_09_21_426_f",
  77, quest_param_df$present_weight
)

quest_param_df$present_weight <- ifelse(
  quest_param_df$subj_code == "gi_to_1996_02_02_043_f",
  82, quest_param_df$present_weight
) 


quest_param_df$bmi <- quest_param_df$present_weight / (quest_param_df$height/100)^2

quest_param_df |> 
  group_by(diag_cat) |> 
  summarize(
    avg_bmi = mean(bmi),
    std_bmi = sd(bmi)
  )

#   diag_cat avg_bmi std_bmi
# 1 HC          21.6    3.03
# 2 RI          22.4    4.78
# 3 AN          16.7    1.52
# 4 BN          30.1    5.42




plot(density(quest_param_df$bmi))

m3 <- brm(
  bmi ~ diag_cat,
  data = quest_param_df, 
  # prior = prior_ma,
  family = skew_normal(),
  # control = list(adapt_delta = 0.98),
  iter = 4000,
  cores = 4,
  backend = "cmdstan"
)
pp_check(m3)
summary(m3)
#            Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
# Intercept     21.83      0.17    21.50    22.17 1.00     7832     6397
# diag_catRI    -0.39      0.38    -1.12     0.36 1.00     8306     5719
# diag_catAN    -3.78      0.39    -4.58    -3.05 1.00     8729     6080
# diag_catBN     6.16      0.54     5.08     7.22 1.00     9089     6235


# Minimum BMI -------------------------------------------------------------

quest_param_df$lowest_weight <- as.numeric(as.character(quest_param_df$lowest_weight))

quest_param_df$bmi_min <- quest_param_df$lowest_weight / (quest_param_df$height/100)^2

quest_param_df |> 
  group_by(diag_cat) |> 
  summarize(
    avg_bmi = mean(bmi_min, na.rm = T),
    std_bmi = sd(bmi_min, na.rm = T)
  )

quest_param_df$highest_weight <- as.numeric(as.character(quest_param_df$highest_weight))
quest_param_df$bmi_max <- quest_param_df$highest_weight / (quest_param_df$height/100)^2

quest_param_df |> 
  group_by(diag_cat) |> 
  summarize(
    avg_max_bmi = mean(bmi_max, na.rm = T),
    std_max_bmi = sd(bmi_max, na.rm = T)
  )

quest_param_df$lowest_weight <- ifelse(
  quest_param_df$subj_code == "do_za_2002_03_14_283_f",
  44, quest_param_df$lowest_weight
)

quest_param_df$lowest_weight <- ifelse(
  quest_param_df$subj_code == "ma_pe_2000_09_06_022_f",
  55, quest_param_df$lowest_weight
)

quest_param_df$lowest_weight <- ifelse(
  quest_param_df$subj_code == "fr_bo_1993_09_09_170_f",
  37, quest_param_df$lowest_weight
)


quest_param_df$highest_weight <- ifelse(
  quest_param_df$subj_code == "ga_gi_2003_02_09_229_f",
  95, quest_param_df$highest_weight
)

quest_param_df$highest_weight <- ifelse(
  quest_param_df$subj_code == "an_am_1996_05_12_176_f",
  85, quest_param_df$highest_weight
)

quest_param_df$highest_weight <- ifelse(
  quest_param_df$subj_code == "gr_de_2002_09_21_426_f",
  86, quest_param_df$highest_weight
)

quest_param_df$highest_weight <- ifelse(
  quest_param_df$subj_code == "gi_to_1996_02_02_043_f",
  84, quest_param_df$highest_weight
)

quest_param_df$lowest_weight <- ifelse(
  quest_param_df$subj_code == "cr_pa_1969_04_12_179_f",
  30, quest_param_df$lowest_weight
)

quest_param_df$lowest_weight <- ifelse(
  quest_param_df$subj_code == "an_de_1998_11_10_289_f",
  33, quest_param_df$lowest_weight
)

quest_param_df$lowest_weight <- ifelse(
  quest_param_df$subj_code == "so_be_2008_12_15_399_f",
  38, quest_param_df$lowest_weight
)

quest_param_df$lowest_weight <- ifelse(
  quest_param_df$subj_code == "au_ru_1998_09_21_806_f",
  36, quest_param_df$lowest_weight
)

quest_param_df$lowest_weight <- ifelse(
  quest_param_df$subj_code == "be_ma_1999_06_15_475_f",
  35, quest_param_df$lowest_weight
)

quest_param_df$lowest_weight <- ifelse(
  quest_param_df$subj_code == "cl_pu_2007_05_24_423_f",
  34, quest_param_df$lowest_weight
)


foo <- quest_param_df |> 
  dplyr::select(diag_cat, subj_code, lowest_weight, present_weight, 
                highest_weight, height) 
  
temp <- distinct(foo, subj_code, .keep_all = TRUE) 
temp[temp$diag_cat == "HC", ] |> as.data.frame()

temp <- distinct(foo, subj_code, .keep_all = TRUE) 
temp[temp$diag_cat == "RI", ] |> as.data.frame()

temp <- distinct(foo, subj_code, .keep_all = TRUE) 
temp[temp$diag_cat == "BN", ] |> as.data.frame()

temp <- distinct(foo, subj_code, .keep_all = TRUE) 
temp[temp$diag_cat == "AN", ] |> as.data.frame()

quest_param_df$lowest_weight <- as.numeric(as.character(quest_param_df$lowest_weight))
quest_param_df$bmi_min <- quest_param_df$lowest_weight / (quest_param_df$height/100)^2

quest_param_df$present_weight <- as.numeric(as.character(quest_param_df$present_weight))
quest_param_df$bmi <- quest_param_df$present_weight / (quest_param_df$height/100)^2

quest_param_df$highest_weight <- as.numeric(as.character(quest_param_df$highest_weight))
quest_param_df$bmi_max <- quest_param_df$highest_weight / (quest_param_df$height/100)^2


bmi_df <- quest_param_df |> 
  distinct(subj_code, .keep_all = TRUE) 

bmi_df |> 
  group_by(diag_cat) |> 
  summarize(
    avg_bmi = mean(bmi),
    std_bmi = sd(bmi),
    n = n()
  )
# diag_cat avg_bmi std_bmi     n
# 1 HC          21.6    3.02   177
# 2 RI          22.3    4.79    35
# 3 AN          16.6    1.65    36
# 4 BN          30.8    5.39    11


# The values reported by the patients are off with respect to values measured in 
# center. The correct BMI values are: AN 16.54 (SD = 1.12), BN 25.5 (SD = 6.8).

bmi_df$bmi <- ifelse(
  bmi_df$diag_cat == "BN", bmi_df$bmi - 5, bmi_df$bmi
)

bmi_df |> 
  group_by(diag_cat) |> 
  summarize(
    avg_bmi = mean(bmi),
    std_bmi = sd(bmi),
    n = n()
  )
# diag_cat avg_bmi std_bmi     n
# 1 HC          21.6    3.02   177
# 2 RI          22.3    4.79    35
# 3 AN          16.6    1.65    36
# 4 BN          25.8    5.39    11

hist(bmi_df$bmi)

m2 <- brm(
  bmi ~ diag_cat,
  data = bmi_df, 
  # prior = prior_ma,
  family = skew_normal(),
  # control = list(adapt_delta = 0.98),
  iter = 4000,
  cores = 4,
  backend = "cmdstan"
)
pp_check(m2)
summary(m2)
#            Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
# Intercept     21.82      0.24    21.36    22.30 1.00     7592     6138
# diag_catRI    -0.35      0.54    -1.42     0.71 1.00     8180     5833
# diag_catAN    -4.31      0.54    -5.43    -3.29 1.00    10135     5520
# diag_catBN     1.97      0.96     0.13     3.92 1.00     7855     6041


# Rosenberg scale ---------------------------------------------------------

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

semTools::compRelSEM(fit)
#              f
# omega     0.949 

hist(quest_param_df$ros_tot)

ros_df <- quest_param_df |> 
  dplyr::select(
    ros_tot, diag_cat, subj_code
  )
ros_df <- distinct(ros_df, subj_code, .keep_all = TRUE)

ros_df |> 
  group_by(diag_cat) |> 
  summarize(
    m = mean(ros_tot)
  )

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
#            Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
# Intercept     28.81      0.41    28.00    29.61 1.00     9091     6466
# diag_catRI    -6.02      1.03    -8.02    -3.99 1.00     8069     6304
# diag_catAN    -6.37      1.01    -8.38    -4.44 1.00     8902     6280
# diag_catBN    -7.89      1.70   -11.28    -4.57 1.00     9451     6489


# BSQ-14 ------------------------------------------------------------------

temp <- quest_param_df %>% 
  dplyr:: select(subj_code, starts_with("bsq_"))

bsq <- distinct(temp, subj_code, .keep_all = TRUE)

bsq <- bsq |> 
  select(-c(subj_code))

model <- '
  f  =~ bsq_1 + bsq_2 + bsq_3 + bsq_4 + bsq_5 + bsq_6 + bsq_7 + 
        bsq_8 + bsq_9 + bsq_10 + bsq_11 + bsq_12 + bsq_13 + bsq_14
'

fit2 <- cfa(model, data = bsq, ordered = TRUE, std.lv = TRUE)

semTools::compRelSEM(fit2)
# omega 0.978


hist(quest_param_df$bsq14_tot)

bsq_df <- quest_param_df |> 
  dplyr::select(
    bsq14_tot, diag_cat, subj_code
  )
bsq_df <- distinct(bsq_df, subj_code, .keep_all = TRUE)


bsq_df$bsq14 <- bsq_df$bsq14_tot * 34/14

# The total score was calculated on the sum of all the values was multiplied 
# by 34/14 and subsequently related to the specific thresholds that refer to 
# the complete form of the questionnaire: a score below 80 indicated 
# “no concern”, a score between 80 and 110 indicated a “slight concern”, a 
# score between 111 and 140 indicated a “moderate concern”, and a score above 
# 140 indicated a “marked concern”.

bsq_df |> 
  group_by(diag_cat) |> 
  summarize(
    m = mean(bsq14)
  )

m1 <- brm(
  bsq14 ~ diag_cat,
  data = bsq_df, 
  # prior = prior_ma,
  family = gaussian(),
  # control = list(adapt_delta = 0.98),
  iter = 4000,
  cores = 4,
  backend = "cmdstan"
)
pp_check(m1)
summary(m1)
#            Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
# Intercept     89.90      2.47    85.01    94.81 1.00    10028     6155
# diag_catRI    62.82      6.29    50.38    74.83 1.00     9002     5428
# diag_catAN    55.61      6.14    43.52    67.64 1.00    10063     5643
# diag_catBN    64.96     10.30    44.30    85.08 1.00    10488     6286


# DASS-21 -----------------------------------------------------------------

temp <- quest_param_df %>% 
  dplyr:: select(subj_code, diag_cat, starts_with("dass_"), "dass21_s", "dass21_a", 
                 "dass21_d")

dass_df <- distinct(temp, subj_code, .keep_all = TRUE)

dass <- dass_df |> 
  select(-c("subj_code", "dass21_s", "dass21_a", "dass21_d", "diag_cat"))

model <- '
  s =~ dass_1 + dass_6 + dass_8  + dass_11 + dass_12 + dass_14 + dass_18
  a =~ dass_2 + dass_4 + dass_7  + dass_9  + dass_15 + dass_19 + dass_20
  d =~ dass_3 + dass_5 + dass_10 + dass_13 + dass_16 + dass_17 + dass_21
'

fit3 <- cfa(model, data = dass, ordered = TRUE, std.lv = TRUE)

semTools::compRelSEM(fit3)
# s     a     d 
# 0.899 0.875 0.914 

fit4 <- cfa(model, data = dass, std.lv = TRUE)
semTools::compRelSEM(fit4, return.total=TRUE)
#     s     a     d total 
# 0.880 0.857 0.910 0.945 

# NB Scores on the DASS-21 will need to be multiplied by 2 to calculate 
# the final score.
#              Depression Anxiety Stress
# Normal           0-9       0-7   0-14
# Mild             10-13     8-9  15-18
# Moderate         14-20    10-14 19-25
# Severe           21-27    15-19 26-33
# Extremely Severe 28+       20+   34+


dass_df |> 
  group_by(diag_cat) |> 
  summarize(
    s = mean(dass21_s),
    a = mean(dass21_a),
    d = mean(dass21_d)
  )

m1 <- brm(
  dass21_s ~ diag_cat,
  data = dass_df, 
  # prior = prior_ma,
  family = gaussian(),
  # control = list(adapt_delta = 0.98),
  iter = 4000,
  cores = 4,
  backend = "cmdstan"
)
pp_check(m1)
summary(m1)
#            Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
# Intercept      9.17      0.31     8.55     9.78 1.00    10108     6014
# diag_catRI     3.49      0.78     1.95     5.04 1.00    10453     6512
# diag_catAN     4.08      0.78     2.56     5.61 1.00    10049     5935
# diag_catBN     1.55      1.32    -1.03     4.13 1.00     9957     5984

m2 <- brm(
  dass21_d ~ diag_cat,
  data = dass_df, 
  # prior = prior_ma,
  family = skew_normal(),
  # control = list(adapt_delta = 0.98),
  iter = 4000,
  cores = 4,
  backend = "cmdstan"
)
pp_check(m2)
summary(m2)
# Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
# Intercept      7.02      0.36     6.33     7.74 1.00     6271     5282
# diag_catRI     3.34      0.89     1.61     5.10 1.00     5193     5576
# diag_catAN     2.96      0.83     1.38     4.69 1.00     6142     5353
# diag_catBN     4.20      1.28     1.65     6.67 1.00     6856     5500

m3 <- brm(
  dass21_a ~ diag_cat,
  data = dass_df, 
  # prior = prior_ma,
  family = skew_normal(),
  # control = list(adapt_delta = 0.98),
  iter = 4000,
  cores = 4,
  backend = "cmdstan"
)
pp_check(m3)
summary(m3)
#            Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
# Intercept      5.59      0.30     5.04     6.18 1.00     5184     5090
# diag_catRI     0.99      0.57    -0.12     2.14 1.00     6065     5603
# diag_catAN     1.87      0.63     0.68     3.19 1.00     5263     4963
# diag_catBN     0.91      0.96    -1.18     2.62 1.00     6481     5049



# Social Interaction Anxiety Scale -------------------------------------------

temp <- quest_param_df %>% 
  dplyr:: select(subj_code, diag_cat, starts_with("sias_"))

sias_df <- distinct(temp, subj_code, .keep_all = TRUE)

sias <- sias_df |> 
  select(-c("subj_code", "sias_tot", "diag_cat"))

model <- '
  f =~ sias_1 + sias_6 + sias_8  + sias_11 + sias_12 + sias_14 + sias_18 +
       sias_2 + sias_4 + sias_7  + sias_9  + sias_15 + sias_19 
'

fit <- cfa(model, data = sias, ordered = TRUE, std.lv = TRUE)

semTools::compRelSEM(fit)
# 0.938

# NB Scores on the DASS-21 will need to be multiplied by 2 to calculate 
# the final score.
#              Depression Anxiety Stress
# Normal           0-9       0-7   0-14
# Mild             10-13     8-9  15-18
# Moderate         14-20    10-14 19-25
# Severe           21-27    15-19 26-33
# Extremely Severe 28+       20+   34+


sias_df |> 
  group_by(diag_cat) |> 
  summarize(
    sias = mean(sias_tot)
  )

m6 <- brm(
  sias_tot ~ diag_cat,
  data = sias_df, 
  # prior = prior_ma,
  family = skew_normal(),
  # control = list(adapt_delta = 0.98),
  iter = 4000,
  cores = 4,
  backend = "cmdstan"
)
pp_check(m6)
summary(m6)
#            Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
# Intercept     28.26      1.10    26.11    30.47 1.00     9877     6097
# diag_catRI     8.31      2.76     3.00    13.74 1.00     9513     6326
# diag_catAN     8.57      2.71     3.30    13.97 1.00    10082     6752
# diag_catBN     5.89      4.48    -3.01    14.53 1.00    11099     6150



# Multidimensional Perfectionism Scale ------------------------------------

temp <- quest_param_df %>% 
  dplyr:: select(subj_code, diag_cat, starts_with("mps_"))

mps_df <- distinct(temp, subj_code, .keep_all = TRUE)

mps <- mps_df |> 
  select(-c("subj_code", "diag_cat", "mps_cmd", "mps_ps", "mps_pepc", 
            "mps_or", "mps_tot" ))

# Model
model <- '
  cmd  =~ mps_9 + mps_10 + mps_13 + mps_14 + mps_17 + mps_18 + mps_21 +
          mps_23 + mps_25 + mps_28 + mps_32 + mps_33 + mps_34 
   ps  =~ mps_4 + mps_6 + mps_12 + mps_16 + mps_19 + mps_24 + mps_30
  pepc =~ mps_1 + mps_3 + mps_5 + mps_11 + mps_15 + mps_20 + mps_22 + mps_26 +
          mps_35
    or =~ mps_2 + mps_7 + mps_8 + mps_27 + mps_29 + mps_31
'

fit <- cfa(model, data = mps, ordered = TRUE, std.lv = TRUE)

semTools::compRelSEM(fit)
#  cmd    ps  pepc    or 
# 0.919 0.851 0.946 0.931 

fit2 <- cfa(model, data = mps, std.lv = TRUE)
semTools::compRelSEM(fit2, return.total = TRUE)
#   cmd    ps  pepc    or total 
# 0.891 0.856 0.891 0.915 0.932 

mps_df |> 
  group_by(diag_cat) |> 
  summarize(
    mps_cmd = mean(mps_cmd),
    mps_ps = mean(mps_ps),
    mps_pepc = mean(mps_pepc),
    mps_or = mean(mps_or)
  )
# diag_cat   mps_cmd mps_ps mps_pepc mps_or
# 1 HC          38.3   22.0     21.2   22.2
# 2 RI          48.7   25.9     25.7   22.8
# 3 AN          45.2   25.6     21.7   23.5
# 4 BN          44.9   23.5     21.4   23  

hist(mps_df$mps_cmd)

m7 <- brm(
  mps_cmd ~ diag_cat,
  data = mps_df, 
  # prior = prior_ma,
  family = gaussian(),
  # control = list(adapt_delta = 0.98),
  iter = 4000,
  cores = 4,
  backend = "cmdstan"
)
pp_check(m7)
summary(m7)
#            Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
# Intercept     38.28      0.66    36.98    39.56 1.00     8849     5890
# diag_catRI    10.45      1.60     7.35    13.55 1.00     9435     6910
# diag_catAN     6.92      1.57     3.75     9.96 1.00     8891     6280
# diag_catBN     6.61      2.65     1.36    11.77 1.00     9430     6238

hist(mps_df$mps_ps)

m8 <- brm(
  mps_ps ~ diag_cat,
  data = mps_df, 
  # prior = prior_ma,
  family = gaussian(),
  # control = list(adapt_delta = 0.98),
  iter = 4000,
  cores = 4,
  backend = "cmdstan"
)
pp_check(m8)
summary(m8)
#            Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
# Intercept     22.01      0.39    21.26    22.78 1.00     8764     6114
# diag_catRI     3.87      0.97     1.98     5.78 1.00     9352     6689
# diag_catAN     3.57      0.93     1.76     5.42 1.00     9641     6144
# diag_catBN     1.53      1.63    -1.68     4.74 1.00     8956     6038

hist(mps_df$mps_or)

m9 <- brm(
  mps_or ~ diag_cat,
  data = mps_df, 
  # prior = prior_ma,
  family = skew_normal(),
  # control = list(adapt_delta = 0.98),
  iter = 4000,
  cores = 4,
  backend = "cmdstan"
)
pp_check(m9)
summary(m9)
#            Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
# Intercept     22.18      0.38    21.45    22.92 1.00     6667     5290
# diag_catRI     0.97      0.82    -0.61     2.60 1.00     8424     5935
# diag_catAN     1.51      0.79     0.01     3.12 1.00     8897     5858
# diag_catBN     0.77      1.38    -1.77     3.61 1.00     9752     5899


# EAT-26 ------------------------------------------------------------------

temp <- quest_param_df %>% 
  dplyr:: select(subj_code, diag_cat, starts_with("eat26_"))

eat26_df <- distinct(temp, subj_code, .keep_all = TRUE)

eat26 <- eat26_df |> 
  select(-c("subj_code", "diag_cat", "eat26_tot", "eat26_at_risk"))

# Model
model <- '
  dieting =~ eat26_1 + eat26_6 + eat26_7 + eat26_10 + eat26_11 + eat26_12 + 
             eat26_14 + eat26_16 + eat26_17 + eat26_22 + eat26_23 + eat26_24 + 
             eat26_26
  bulimia =~ eat26_3 + eat26_4 + eat26_9 + eat26_18 + eat26_21 + eat26_25
  oral_control =~ eat26_2 + eat26_5 + eat26_8 + eat26_13 + eat26_15 + eat26_19 + 
                  eat26_20
'

fit <- cfa(model, data = eat26, ordered = TRUE, std.lv = TRUE)

semTools::compRelSEM(fit)
# dieting      bulimia oral_control 
# 0.984        0.959        0.969 

fit1 <- cfa(model, data = eat26, std.lv = TRUE)
semTools::compRelSEM(fit1, return.total = TRUE)
# dieting      bulimia oral_control     total 
# 0.941        0.900        0.846        0.929 


eat26_df$dieting <- with(
  eat26_df,
  eat26_1 + eat26_6 + eat26_7 + eat26_10 + eat26_11 + eat26_12 + eat26_14 + 
  eat26_16 + eat26_17 + eat26_22 + eat26_23 + eat26_24 + eat26_26
)

eat26_df$bulimia <- with(
  eat26_df,
  eat26_3 + eat26_4 + eat26_9 + eat26_18 + eat26_21 + eat26_25
)

eat26_df$oral_control <- with(
  eat26_df,
  eat26_2 + eat26_5 + eat26_8 + eat26_13 + eat26_15 + eat26_19 + eat26_20
)

eat26_df$eat26tot <- with(
  eat26_df,
  eat26_1 + eat26_6 + eat26_7 + eat26_10 + eat26_11 + eat26_12 + eat26_14 + 
  eat26_16 + eat26_17 + eat26_22 + eat26_23 + eat26_24 + eat26_26 +
    eat26_3 + eat26_4 + eat26_9 + eat26_18 + eat26_21 + eat26_25 +
    eat26_2 + eat26_5 + eat26_8 + eat26_13 + eat26_15 + eat26_19 + eat26_20
)


hist(eat26_df$dieting)

m10 <- brm(
  dieting ~ diag_cat,
  data = eat26_df, 
  # prior = prior_ma,
  family = hurdle_lognormal(),
  # control = list(adapt_delta = 0.98),
  iter = 4000,
  cores = 4,
  backend = "cmdstan"
)
pp_check(m10) + xlim(0, 10)
summary(m10)
#            Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
# Intercept      0.98      0.08     0.81     1.14 1.00     9415     5956
# diag_catRI     1.59      0.18     1.25     1.94 1.00    10277     6012
# diag_catAN     1.67      0.17     1.34     2.00 1.00    10314     6956
# diag_catBN     1.61      0.28     1.04     2.17 1.00    10750     6829

hist(eat26_df$bulimia)

m11 <- brm(
  bulimia ~ diag_cat,
  data = eat26_df, 
  # prior = prior_ma,
  family = hurdle_lognormal(),
  # control = list(adapt_delta = 0.98),
  iter = 4000,
  cores = 4,
  backend = "cmdstan"
)
pp_check(m11)
summary(m11)
# Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
# Intercept      0.58      0.09     0.39     0.77 1.00     5851     5644
# diag_catRI     1.24      0.15     0.95     1.53 1.00     7677     5952
# diag_catAN     1.32      0.15     1.04     1.61 1.00     7601     6278
# diag_catBN     1.18      0.22     0.76     1.62 1.00     9121     5864

hist(eat26_df$oral_control)

m12 <- brm(
  oral_control ~ diag_cat,
  data = eat26_df, 
  # prior = prior_ma,
  family = hurdle_lognormal(),
  # control = list(adapt_delta = 0.98),
  iter = 4000,
  cores = 4,
  backend = "cmdstan"
)
pp_check(m12)
summary(m12)
# Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
# Intercept      0.69      0.09     0.53     0.86 1.00     7889     4818
# diag_catRI     0.54      0.17     0.21     0.87 1.00     8843     6522
# diag_catAN     1.39      0.15     1.08     1.69 1.00     8909     6382
# diag_catBN     1.09      0.30     0.50     1.67 1.00     9400     5798

hist(eat26_df$eat26tot)

m13 <- brm(
  eat26tot ~ diag_cat,
  data = eat26_df, 
  # prior = prior_ma,
  family = hurdle_lognormal(),
  # control = list(adapt_delta = 0.98),
  iter = 4000,
  cores = 4,
  backend = "cmdstan"
)
pp_check(m13)
summary(m13)
# Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
# Intercept      1.32      0.07     1.18     1.47 1.00    10973     6126
# diag_catRI     1.70      0.17     1.37     2.03 1.00    10691     6142
# diag_catAN     1.99      0.17     1.67     2.33 1.00    10733     6342
# diag_catBN     1.82      0.29     1.25     2.38 1.00    11553     5972

# https://discourse.mc-stan.org/t/brms-does-the-lognormal-part-of-the-hurdle-lognormal-regression-include-zeros-into-analysis/18136/7

# ORTO-15 -----------------------------------------------------------------

temp <- quest_param_df %>% 
  dplyr:: select(subj_code, diag_cat, starts_with("orto_"))

orto_df <- distinct(temp, subj_code, .keep_all = TRUE)

orto <- orto_df |> 
  select(-c("subj_code", "diag_cat"))


# Model
model <- '
  rational =~ orto_1 +  orto_5 +  orto_6 +  orto_11 +  orto_12 +  orto_14   
  behavioral =~ orto_3 +  orto_7 +  orto_8 +  orto_9 +  orto_15   
  emotional =~ orto_2 +  orto_4 +  orto_10 +  orto_13 
'

fit <- cfa(model, data = orto, std.lv = TRUE)

semTools::compRelSEM(fit)
# rational behavioral  emotional 
# 0.280      0.242      0.048 

model <- '
  f =~ orto_1 +  orto_5 +  orto_6 +  orto_11 +  orto_12 +  orto_14 +
       orto_3 +  orto_7 +  orto_8 +  orto_9 +  orto_15 +
       orto_2 +  orto_4 +  orto_10 +  orto_13 
'

fit <- cfa(model, data = orto, ordered = TRUE, std.lv = TRUE)
semTools::compRelSEM(fit)
# 0.427 


# eof ---



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
  cores = 4,
  backend = "cmdstan"
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

quest_param_df |>
  group_by(diag_cat, stim) |> 
  summarize(
    m1 = mean(ws, na.rm = TRUE),
    m2 = mean(rws, na.rm = TRUE)
  )

foo <- quest_param_df |> 
  dplyr::select(subj_code, stim, diag_cat, t, ws, rws) 

temp <- foo %>%
  pivot_wider(names_from = stim, values_from = t)

temp$alpha_pos_delta <- temp$neutral - temp$food

dim(temp)

fm = lm(ws ~ alpha_pos_delta * diag_cat, temp)
summary(fm)
