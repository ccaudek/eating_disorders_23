

setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) # set wd to this file path

# load pkgs ====
library("tidyverse");library("rio");


# tracs = import("ddm_grptrcs.csv")
# str(tracs)
# tr.nms = names(tracs)
# 
# 
# delta <- c(tracs$`pos_alpha(AN.food)`, tracs$`pos_alpha(AN.neutral)`)
# 
# df <- data.frame(delta = delta, grp = rep(0:1, each = 1000))

# load ddm raw traces (without burn-in) ====
tracs = import("ddm_alltrcs.csv")
str(tracs)
tr.nms = names(tracs)

alpha_pars <- tracs.alpha %>%
  as.data.frame() %>%
  mutate(draw = seq(1, n()), parameter = "alpha")


# Select columns
alpha_pars_an <- tracs %>% 
  dplyr::select(
    starts_with("alpha_subj(AN.food)")
    )

alpha_an <- colMeans(alpha_pars_an) %>% as.numeric()

alpha_pars_hc <- tracs %>% 
  dplyr::select(
    starts_with("alpha_subj(HC.food)")
  )

alpha_hc <- colMeans(alpha_pars_hc) %>% as.numeric()

y <- c(alpha_an, alpha_hc)

gr <- c(
  rep(1, length(alpha_an)),
  rep(0, length(alpha_hc))
) %>% as.factor()

dat <- data.frame(
  y=y, gr=gr
)

(d <- effectsize::cohens_d(y ~ gr, data = dat))


# Within-subjects ---------------------------------------------------------

# AN pos_alpha food ----
# Select columns
pos_alpha_food_an <- tracs %>% 
  dplyr::select(
    starts_with("pos_alpha_subj(AN.food)")
  )

bysubj_pos_alpha_food_an <- colMeans(pos_alpha_food_an) %>% as.numeric()

# Get columns names.  After the dot there is the number of the participant.
x <- colnames(pos_alpha_food_an) %>% as.character()
# Get the participant number
matches <- regmatches(x, gregexpr("[[:digit:]]+", x))
id <- as.numeric(unlist(matches))

food_df <- data.frame(
  id = id,
  val = bysubj_pos_alpha_food_an
)


# AN pos_alpha neutral ----

pos_alpha_neutral_an <- tracs %>% 
  dplyr::select(
    starts_with("pos_alpha_subj(AN.neutral)")
  )

bysubj_pos_alpha_neutral_an <- colMeans(pos_alpha_neutral_an) %>% as.numeric()

x <- colnames(pos_alpha_neutral_an) %>% as.character()
matches <- regmatches(x, gregexpr("[[:digit:]]+", x))
id <- as.numeric(unlist(matches))

neutral_df <- data.frame(
  id = id,
  val = bysubj_pos_alpha_neutral_an
)

# join
all <- left_join(food_df, neutral_df, by = "id")

sum(all$val.x < all$val.y, na.rm = TRUE)

# There are 3 NAs.  So, there are 28 successes in 32 cases


delta <- pos_alpha_neutral_an$`pos_alpha_subj(AN.neutral).6` -
  pos_alpha_food_an$`pos_alpha_subj(AN.food).6` 
mean(delta)
tidybayes::median_qi(delta, .width = 0.5)

#-----------




sim_d_and_fit <- function(seed, n) {
  
  mu_t <- 0.85
  mu_c <- 0
  
  set.seed(seed)
  
  d <-
    tibble(group     = rep(c("control", "treatment"), each = n)) %>% 
    mutate(treatment = ifelse(group == "control", 0, 1),
           y         = ifelse(group == "control", 
                              rnorm(n, mean = mu_c, sd = 1),
                              rnorm(n, mean = mu_t, sd = 1)))
  
  update(fit,
         newdata = d, 
         seed = seed) %>% 
    fixef() %>% 
    data.frame() %>% 
    rownames_to_column("parameter") %>% 
    filter(parameter == "treatment")
}



n_sim <- 10

t5 <- Sys.time()
s3 <-
  tibble(seed = 1:n_sim) %>% 
  mutate(b1 = map(seed, sim_d_and_fit, n = 35)) %>% 
  unnest(b1)
t6 <- Sys.time()

t6 - t5

s3 %>% 
  ggplot(aes(x = seed, y = Estimate, ymin = Q2.5, ymax = Q97.5)) +
  geom_hline(yintercept = c(0, .5), color = "white") +
  geom_pointrange(fatten = 1/2) +
  labs(x = "seed (i.e., simulation index)",
       y = expression(beta[1]))

s3 %>% 
  mutate(check = ifelse(Q2.5 > 0, 1, 0)) %>% 
  summarise(power = mean(check))

  



