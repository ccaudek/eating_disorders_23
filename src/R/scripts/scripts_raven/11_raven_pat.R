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


dir <- here("data", "raw", "raven", "patients")

file_names <- as.character(list.files(path=dir, pattern = "raven_eds"))
n_files <- length(file_names)
n_files

d_list <- list()

for (i in 1:n_files) {
  
  d  <- read.table(here("data", "raw", "raven", "patients", file_names[i]))
  
  d <- d %>% 
    dplyr::rename(
      matrix_name = V1,
      matrix_chosen = V2,
      rt = V3,
      is_correct = V4
    )
  
  d$subj_name <- stri_sub(file_names[i], 7, -5)
  d$gender <- stri_sub(file_names[i], -5, -5)
  
  d$is_correct <- ifelse(d$is_correct == 1, 1, 0)
  d_list[[i]] <- d
  
}

# convert list into data.frame
df <- do.call(rbind.data.frame, d_list)

df$code_psytoolkit1 <- df$subj_name
df$code_psytoolkit2 <- paste("raven", df$code_psytoolkit1, sep="_")
df$code_psytoolkit3 <- paste(df$code_psytoolkit2, sep=".", "txt")

df <- df %>% 
  dplyr::rename(
    code_psytoolkit = code_psytoolkit3
  )

df$item <- df$matrix_name
df$id <- df$code_psytoolkit

# compute IRT 2PL theta

## specify a 2PL model
formula_2pl <- bf(
  is_correct ~ exp(logalpha) * eta,
  eta ~ 1 + (1 |i| item) + (1 | id),
  logalpha ~ 1 + (1 |i| item),
  nl = TRUE
)

# specify some weakly informative priors
prior_2pl <- 
  prior("normal(0, 5)", class = "b", nlpar = "eta") +
  prior("normal(0, 1)", class = "b", nlpar = "logalpha") +
  prior("constant(1)", class = "sd", group = "id", nlpar = "eta") + 
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
(person_pars_va_2pl <- ranef_va_2pl$id)

# plot person parameters
person_pars_va_2pl[, , "eta_Intercept"] %>%
  as_tibble() %>%
  rownames_to_column() %>%
  select(-Est.Error) %>%
  arrange(Estimate) %>%
  mutate(id = seq_len(n())) %>%
  ggplot(aes(id, Estimate, ymin = Q2.5, ymax = Q97.5)) +
  geom_pointrange(alpha = 0.7) +
  coord_flip() +
  labs(x = "Person Number (Sorted)")

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

saveRDS(
  raven_scores,
  here("data", "processed", "raven", "raven_patients_scores.rds")
)

# e  n  d  ----
