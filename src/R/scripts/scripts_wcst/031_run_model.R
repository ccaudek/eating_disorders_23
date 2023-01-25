# Script name: 31_run_model.R
# Project: Eating disorders Montecatini
# Script purpose: run the Stan models for the WCST data
# @author: Corrado Caudek <corrado.caudek@unifi.it>
# Date Created: date_created
# Last Modified Date: date_modified
#
# 👉 

library("cmdstanr")
library("posterior")
library("bayesplot")
library("recipes")
library("MCMCpack")
library("bayestestR")
library("insight")

# chose model
file <- file.path("code", "scripts", "scripts_wcst", "_stan", "01_AU_rpdi.stan") 
file <- file.path("code", "scripts", "scripts_wcst", "_stan", "02_AU_1pd1.stan") 
file <- file.path("code", "scripts", "scripts_wcst", "_stan", "03_MBRL.stan") 
file <- file.path("code", "scripts", "scripts_wcst", "_stan", "04_PRL.stan") 
file <- file.path("code", "scripts", "scripts_wcst", "_stan", "05_MBRL_without_inertia.stan") #
file <- file.path("code", "scripts", "scripts_wcst", "_stan", "06_PRL_without_inertia.stan")
file <- file.path("code", "scripts", "scripts_wcst", "_stan", "07_PRL_weighting.stan") 
file <- file.path("code", "scripts", "scripts_wcst", "_stan", "08_PRL_weighting_without_inertia.stan") 


# Parmeter of interest
params_mod1 <- c("mu_r","mu_p","mu_d","mu_i","r","p","d","i")

params_mod2 <- c("mu_p","mu_d","p","d")

params_mod3 <- c("mu_MB_Arew", "mu_MB_Apun", "mu_MB_gamma", "mu_temp",
                 "MB_Arew", "MB_Apun", "MB_gamma","temp","log_lik","y_pred")

params_mod5 <-  c("mu_MB_Arew", "mu_MB_Apun", "mu_temp",
                  "MB_Arew", "MB_Apun", "temp","log_lik","y_pred")  #### PRL

params_mod4 <-  c("mu_MB_Arew", "mu_MB_Apun", "mu_MB_gamma", "mu_MF_Arew", 
                  "mu_MF_Apun", "mu_MF_gamma","mu_temp", "MB_Arew", "MB_Apun", 
                  "MB_gamma","MF_Arew", "MF_Apun", "MF_gamma","temp","log_lik",
                  "y_pred")
  
params_mod6 <-  c("mu_MB_Arew", "mu_MB_Apun", "mu_MF_Arew", "mu_MF_Apun", 
                  "mu_temp","MB_Arew", "MB_Apun", "MF_Arew", "MF_Apun", "temp",
                  "log_lik","y_pred")

params_mod7 <-  c("mu_MB_Arew", "mu_MB_Apun", "mu_MB_gamma", "mu_MF_Arew", 
                  "mu_MF_Apun", "mu_MF_gamma","mu_temp","mu_w", "MB_Arew", 
                  "MB_Apun", "MB_gamma","MF_Arew", "MF_Apun", "MF_gamma","temp",
                  "w","log_lik","y_pred")

params_mod8 <-  c("mu_MB_Arew", "mu_MB_Apun", "mu_MF_Arew", "mu_MF_Apun",
                  "mu_temp","mu_w", "MB_Arew", "MB_Apun", "MF_Arew", "MF_Apun", 
                  "temp","log_lik","y_pred")


# compile model
mod <- cmdstan_model(file)

mod$print()
mod$exe_file()


params_mod4 <-  c("MB_Arew", "MB_Apun", "MF_Arew", "MF_Apun", "temp")

params_mod7 <-  c("mu_MB_Arew", "mu_MB_Apun", "mu_MB_gamma", "mu_MF_Arew", 
                  "mu_MF_Apun", "mu_MF_gamma","mu_temp","mu_w", "MB_Arew", 
                  "MB_Apun", "MB_gamma","MF_Arew", "MF_Apun", "MF_gamma","temp")


data_list <- data

# Obtain a posterior mode (penalized maximum likelihood) estimate.
fit_mle <- mod$optimize(data = data_list, seed = 123)
res_mle <- fit_mle$summary(params_mod7) 
res_mle %>% as.data.frame()


# unique(res_mle$variable)

# res_mle$variable <- res_mle$variable %>% 
#   str_replace_all("\\[|\\]", " ")

res_mle$variable <- res_mle$variable %>% 
  str_replace_all("\\[", " ")

res_mle$variable <- res_mle$variable %>% 
  str_replace_all("\\]", "")

foo <- tibble(x = res_mle$variable)

temp <- str_split_fixed(foo$x, " ", 2) %>% 
  as.data.frame()

colnames(temp) <- c("params", "id")
temp$estimate <- res_mle$estimate

# controls <- temp %>% 
#   pivot_wider(names_from = params, values_from = estimate)

patients <- temp %>% 
  pivot_wider(names_from = params, values_from = estimate)


# saveRDS(
#   controls, 
#   here("data", "processed", "wcst", "res_mle_controls.rds")
# )

saveRDS(
  patients, 
  here("data", "processed", "wcst", "res_mle_patients.rds")
)



# Run MCMC.
fit <- mod$sample(data = data_list, seed = 123)

params_mod7 <-  c("mu_MB_Arew", "mu_MB_Apun", "mu_MB_gamma", "mu_MF_Arew", 
                  "mu_MF_Apun", "mu_MF_gamma","mu_temp","mu_w")

res_mod7_patients <- fit$summary(params_mod7)

saveRDS(
  res_mod5_patients, 
  here("data", "processed", "wcst", "res_mod5_patients.rds")
)

# variable        mean       median          sd          mad           q5         q95
# 1    mu_MB_Arew 0.854047331 0.9927465000 0.279241830 0.0093500169 1.498296e-01 0.999757100
# 2    mu_MB_Apun 0.001003529 0.0004059875 0.001500722 0.0005933970 1.161868e-06 0.004094497
# 3       mu_temp 0.623374750 0.7802760000 0.289056459 0.0327817686 8.753999e-02 0.819946900

# e n d ------------------------------------------



# Run 'variational' method to approximate the posterior (default is meanfield ADVI)
fit_vb <- mod$variational(data = data_list, seed = 123)
fit_vb$summary(params_mod7) 
# Plot approximate posterior using bayesplot
mcmc_hist(fit_vb$draws("mu_MB_Arew"))





# model 7 -----------------------------------------------------------

out <- fit_mle$summary(params_mod6) %>% 
  as.data.frame()

out[1:100, ]

# MB_Arew 9:86, 87:101
# MB_Apun 102:179  180:194
# MB_gamma 195:272 273:287

# MF_Arew 288:365 366:380
# MF_Apun 381:458 459:473
# MF_gamma 474:551  552:556

# temp 567:644 645:659
# w 660:737 738:752

MB_Arew_c  <- out$estimate[9:86]
MB_Arew_p  <- out$estimate[87:101]
# MB_Apun_c  <- out$estimate[102:179]
# MB_Apun_p  <- out$estimate[180:194]
MB_gamma_c <- out$estimate[195:272]
MB_gamma_p <- out$estimate[273:287]

MF_Arew_c  <- out$estimate[288:365]
MF_Arew_p  <- out$estimate[366:380]
MF_Apun_c  <- out$estimate[381:458]
MF_Apun_p  <- out$estimate[459:473]
MF_gamma_c <- out$estimate[474:551]
MF_gamma_p <- out$estimate[552:566]

# temp_c <- out$estimate[567:644]
# temp_p <- out$estimate[645:659]

hist(MF_gamma_c)

MB_Arew <- c(MB_Arew_c, MB_Arew_p)
MB_gamma <- c(MB_gamma_c, MB_gamma_p)
MF_Arew <- c(MF_Arew_c, MF_Arew_p)
MF_Apun <- c(MF_Apun_c, MF_Apun_p)
MF_gamma <- c(MF_gamma_c, MF_gamma_p)
grp <- c(rep(0, length(MB_Arew_c)), rep(1, length(MB_Arew_p)))

param_df <- data.frame(
  MB_Arew, MB_gamma, MF_Arew, MF_Apun, MF_gamma, grp
)


fit1 <- brm(
  mvbind(MB_Arew, MB_gamma, MF_Arew, MF_Apun, MF_gamma) ~ grp,
  data = param_df, 
  family = shifted_lognormal(),
  chains = 4, 
  cores = 4,
  backend = "cmdstan"
)

fit1 <- add_criterion(fit1, "loo")
loo(fit1)

pp_check(fit1, resp = "MBArew")
pp_check(fit1, resp = "MBgamma")
pp_check(fit1, resp = "MFArew")
pp_check(fit1, resp = "MFApun")
pp_check(fit1, resp = "MFgamma")

round(bayes_R2(fit1), 2)
#           Estimate Est.Error Q2.5 Q97.5
# R2MBArew      0.01      0.02    0  0.07
# R2MBgamma     0.01      0.02    0  0.05
# R2MFArew      0.02      0.03    0  0.09
# R2MFApun      0.02      0.03    0  0.10
# R2MFgamma     0.02      0.02    0  0.09

summary(fit1)
#                   Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
# MBArew_grp            0.04      0.08    -0.12     0.21 1.00     2031     1724
# MBgamma_grp          -0.01      0.20    -0.40     0.40 1.00     3109     2076
# MFArew_grp            0.03      0.09    -0.15     0.21 1.01      886     1569
# MFApun_grp           -0.02      0.03    -0.09     0.02 1.01     1214     1483
# MFgamma_grp           0.05      0.06    -0.06     0.19 1.00     2069     1731

# effectsize::standardize_parameters(fit1, method = "refit", ci = 0.95)


conditional_effects(fit1, "grp", resp = "MBArew")


# bishara -----------------------------------------------------------

file <- file.path("scripts", "_wcst", "_stan", "01_AU_rpdi.stan")
mod <- cmdstan_model(file)
fit_mle <- mod$optimize(data = data_list, seed = 123)

params_mod1 <- c("mu_r","mu_p","mu_d","mu_i","r","p","d","i")

out1 <- fit_mle$summary(params_mod1) %>% 
  as.data.frame()

r <- out$estimate[5:97]
p <- out$estimate[98:190]
d <- out$estimate[191:283]
i <- out$estimate[284:376]

grp <- c(rep(0, 78), rep(1, 15))

param1_df <- data.frame(
  r, p, d, i, grp
)

hist(param1_df$p)

fit2 <- brm(
  mvbind(r, p, d, i) ~ grp,
  data = param1_df, 
  family = Beta(),
  chains = 4, 
  cores = 4,
  backend = "cmdstan"
)

fit2 <- add_criterion(fit2, "loo")
loo(fit2)

pp_check(fit2, resp = "r")
pp_check(fit2, resp = "p")
pp_check(fit2, resp = "d")
pp_check(fit2, resp = "i")

round(bayes_R2(fit2), 2)
#     Estimate Est.Error Q2.5 Q97.5
# R2r     0.04      0.04    0  0.16
# R2p     0.00      0.00    0  0.00
# R2d     0.09      0.08    0  0.29
# R2i     0.03      0.04    0  0.14

summary(fit2)
#             Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
# r_grp           0.13      0.15    -0.16     0.43 1.00     7123     3100
# p_grp          -0.10      0.28    -0.67     0.44 1.00     6494     2966
# d_grp           0.13      0.13    -0.13     0.37 1.00     6704     2810
# i_grp           0.17      0.20    -0.23     0.56 1.00     6033     2842





bayes.t.test(
  n=10000,
  burnin=5000,
  firstComp = i[1:78],
  secondComp= i[79:93],
  sd="sd",
  plot="all",
  ci=0.95,
  hyperpars="custom",
  q=0.1
)






y <- c(yc, yp)
grp <- c(rep(0, length(yc)), rep(1, length(yp)))

dat_mod7 <- data.frame(grp, y)

model <- stan_glm(y ~ grp, data=dat_mod7)

posteriors <- insight::get_parameters(model)
bayestestR::hdi(posteriors$grp, ci=0.89)

rope_value <- rope_range(model)
rope_value

rope(posteriors$grp, range = rope_range, ci=0.89)






# Running MCMC
fit <- mod$sample(
  data = data,
  iter_sampling = 500,
  iter_warmup = 100,
  seed = 123,
  chains  = 3, # must be exactly 3
  refresh = 10,
  thin = 1
)

fit$cmdstan_diagnose()
fit$summary()

params_mod5 <- c("mu_MB_Arew", "mu_MB_Apun", "mu_temp",
  "MB_Arew", "MB_Apun", "temp","log_lik")

params_mod2 <- c("mu_p","mu_d","p","d")

params_mod3 <- c("mu_MB_Arew", "mu_MB_Apun", "mu_MB_gamma", "mu_temp",
       "MB_Arew", "MB_Apun", "MB_gamma", "temp")

params_mod7 <- c("mu_MB_Arew", "mu_MB_Apun", "mu_MB_gamma", "mu_MF_Arew", 
                 "mu_MF_Apun", "mu_MF_gamma","mu_temp","mu_w",
                 "MB_Arew", "MB_Apun", "MB_gamma","MF_Arew", "MF_Apun", 
                 "MF_gamma","temp","w")
     



fit$summary(params_mod5)[, c(1, 2, 4, 6, 7)] %>% 
  as.data.frame()

fit$summary(params_mod2)[, c(1, 2, 4, 6, 7)] %>% 
  as.data.frame()

fit$summary(params_mod3) %>% 
  as.data.frame()


fit$summary("MB_Arew_pr[4]") 

# this is a draws_array object from the posterior package
draws_array <- fit$draws()
str(draws_array)

# convert to matrix or data frame 
draws_df <- as_draws_df(draws_array) # as_draws_matrix() for matrix
print(draws_df)

names(draws_df)

mcmc_hist(fit$draws("MB_Arew_pr[1]"))


fit_mle <- mod$optimize(data = data, seed = 123)
fit_mle$summary()[1:30] # includes lp__ (log prob calculated by Stan program)


fit_vb <- mod$variational(
  data = data, 
  seed = 123, 
  output_samples = 4000
)

fit_vb$summary()[1:60, c(1, 2, 4, 6, 7)] %>% 
  as.data.frame()


# mod 6
arew6 <- as.numeric(flatten(fit_vb$summary()[13:56, c(2)]))
apun6 <- as.numeric(flatten(fit_vb$summary()[(13+44):(56+44), c(2)]))
hist(arew6)
hist(apun6)


# mod 8
arew8 <- as.numeric(flatten(fit_vb$summary()[15:58, c(2)]))
apun8 <- as.numeric(flatten(fit_vb$summary()[(15+44):(58+44), c(2)]))
hist(arew8)
hist(apun8)

plot(arew6, apun6)

cor(arew6, arew8)
cor(apun6, apun8)

