## ----setup, include = FALSE, echo=FALSE----------------------------------

library(knitr)
library(papaja)
library(tidyverse)
library(brms)
library(lmerTest)
library(reshape2)
library(designr)

theme_set(theme_apa())


# set global chunk options, put figures into folder
options(warn=-1, replace.assign=TRUE)
opts_chunk$set(fig.path='figures/figure-', fig.align='center', fig.show='hold')
options(replace.assign=TRUE, width=75)
opts_chunk$set(echo = TRUE)



bysubj_prop$ly <- car::logit(bysubj_prop$y)

# parameter priors
priors <- c(set_prior("normal(0, 2.0)", class = "Intercept"),
            set_prior("normal(0, 2.0)", class = "b"),
            set_prior("normal(0, 2.5)", class = "sd")
            # set_prior("normal(60, 20)", class = "phi"),
            # set_prior("lkj(2)", class = "cor")
            )

mod1 <- brm(
  ly ~ 1 + diag_cat * stimulus_type + (1 + stimulus_type | subj_code),
  data = bysubj_prop, 
  prior = priors,
  family = gaussian(),
  save_pars = save_pars(all = TRUE),
  control = list(adapt_delta = 0.99, max_treedepth=15),
  # backend = "cmdstan",
  warmup = 1000, 
  iter = 2000, 
  cores = 4
)

mod0 <- brm(
  ly ~ 1 + diag_cat + (1 + stimulus_type | subj_code),
  data = bysubj_prop, 
  prior = priors,
  family = gaussian(),
  save_pars = save_pars(all = TRUE),
  control = list(adapt_delta = 0.99, max_treedepth=15),
  # backend = "cmdstan",
  warmup = 1000, 
  iter = 2000, 
  cores = 4
)

mod_Full <- bridge_sampler(mod1, silent=TRUE)
mod_Null <- bridge_sampler(mod0, silent=TRUE)
bayes_factor(mod_Full, mod_Null)$bf





priors <- c(
  set_prior("student_t(3, 0, 0.2)", class = "b", coef = "Intercept"),
  set_prior("student_t(3, 0, 0.2)", class = "b"),
  set_prior("student_t(3, 0, 0.2)", class = "sd")
  # set_prior("lkj(1)", class = "cor")
  # set_prior("normal(60, 20)", class = "phi")
)


bmod_biased_choices_01 <- brm(
  trg_chosen ~ 0 + Intercept + di * st + (1 + st | ID),
  data = dat,
  family = bernoulli(link = "logit"),
  control = list(adapt_delta = 0.999, max_treedepth = 15),
  prior = priors,
  # backend = "cmdstan",
  algorithm = "meanfield",
  warmup = 2000,
  iter = 10000,
  chains = 4,
  cores = 4,
  init = "0",
  seed = 12345
)
summary(bmod_biased_choices_01)

pp_check(bmod_biased_choices_01)

pred_prob <- dat %>%
  select(trg_chosen, di, st, ID) %>%
  distinct() %>%
  add_epred_draws(
    bmod_biased_choices_01,
    allow_new_levels = TRUE,
    value = "Probability"
  )





emmeans(bmod_biased_choices_01, specs = pairwise ~ st:di)

#get the adjusted means
warp_em <- emmeans(bmod_biased_choices_01,  ~ st | di)
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


ggplot(cont_posterior,
       aes(y = contrast, x = .value, fill = di, group = di)) +
  stat_halfeye(alpha = 0.5)+
  geom_vline(xintercept = 0, color = "red", lty = 2)

bmod_biased_choices_01 |> 
  emmeans( ~ st | di) %>%
  gather_emmeans_draws() %>%
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



