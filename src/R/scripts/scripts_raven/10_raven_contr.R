## ------------------------------------------------------------------
## 10_raven_eds.R
## 
## Project: EDs
## Purpose: 
## Author: Corrado Caudek
## Date: 
## ------------------------------------------------------------------

library("here")
library("tidyverse")
library("stringi")
library("brms")
library("cmdstanr")

GROUP <- "patients"

dir <- here("data", "raw", "raven", GROUP)

file_names <- as.character(list.files(path=dir, pattern = "raven_eds"))
n_files <- length(file_names)
n_files

d_list <- list()

for (i in 1:n_files) {
  d  <- read.table(here("data", "raw", "raven", GROUP, file_names[i]))
  d2 <- d %>% 
    dplyr::rename(
      item = V1,
      matrix_chosen = V2,
      rt = V3,
      is_correct = V4
    )
  
  d2$is_correct <- ifelse(d2$is_correct == 1, 1, 0)
  d2$code_psytoolkit <- file_names[i]
    
  d_list[[i]] <- d2
}
# convert list into data.frame
df <- do.call(rbind.data.frame, d_list)

# compute IRT 2PL theta
## specify a 2PL model
formula_2pl <- bf(
  is_correct ~ exp(logalpha) * eta,
  eta ~ 1 + (1 |i| item) + (1 | code_psytoolkit),
  logalpha ~ 1 + (1 |i| item),
  nl = TRUE
)

# specify some weakly informative priors
prior_2pl <- 
  prior("normal(0, 5)", class = "b", nlpar = "eta") +
  prior("normal(0, 1)", class = "b", nlpar = "logalpha") +
  prior("constant(1)", class = "sd", group = "code_psytoolkit", nlpar = "eta") + 
  prior("normal(0, 3)", class = "sd", group = "item", nlpar = "eta") +
  prior("normal(0, 1)", class = "sd", group = "item", nlpar = "logalpha")

# fit the 2PL model
# this models throws some convergence warnings which are false
# positives and can be safely ignored
fit <- brm(
  formula = formula_2pl,
  data = df,
  family = brmsfamily("bernoulli", "logit"),
  prior = prior_2pl,
  seed = 1234,
  backend = "cmdstan"
)

# extract person parameters
ranef_va_2pl <- ranef(fit)
(person_pars_va_2pl <- ranef_va_2pl$code_psytoolkit)

# plot person parameters
person_pars_va_2pl[, , "eta_Intercept"] %>%
  as_tibble() %>%
  rownames_to_column() %>%
  select(-Est.Error) %>%
  arrange(Estimate) %>%
  mutate(code_psytoolkit = seq_len(n())) %>%
  ggplot(aes(Estimate, code_psytoolkit, xmin = Q2.5, xmax = Q97.5)) +
  geom_pointrange(alpha = 0.7) +
  coord_flip() +
  labs(x = "Eta")

theta <- person_pars_va_2pl[, , "eta_Intercept"][, 1]

bysubj_raven_scores <- df %>% 
  group_by(code_psytoolkit) %>% 
  summarise(
    raven_score = sum(is_correct),
    avg_rt = mean(rt, trim = 0.1)
  ) 

bysubj_raven_scores$theta <- theta
plot(bysubj_raven_scores$raven_score, bysubj_raven_scores$theta)
cor(bysubj_raven_scores$raven_score, bysubj_raven_scores$theta)

raven_scores <- bysubj_raven_scores[c(-1), ]

if (GROUP == "patients") {
  saveRDS(
    raven_scores,
    here("data", "processed", "raven", "raven_patients_scores.rds")
  )
} else {
  saveRDS(
    raven_scores,
    here("data", "processed", "raven", "raven_controls_scores.rds")
  )
}



# e  n  d  ----

