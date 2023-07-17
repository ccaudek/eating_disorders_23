
library(here)
library(tidyverse)
library(brms)

d <- rio::import(
  here::here(
    "data", "processed", "prl", "input_for_hddmrl", "three_groups", 
    "ed_prl_data.csv"
  )
)

d <- rio::import(
  "/Users/corrado/_repositories/eating_disorders_23/data/processed/prl/input_for_hddmrl/three_groups/ed_prl_data.csv"
)

diagcat_df <- d |> 
  dplyr::select(subj_code, diag_cat) |> 
  distinct()


get_win_stay_lose_shift <- function(d, index) {
  
  # data of one subject
  d <- d %>% 
    dplyr::filter(
      subj_idx == index
    )
  
  # add outcome and choice in the previous trial
  d <- d %>%
    group_by(stim) %>%
    mutate(
      feedback_prev1 = lag(feedback, 1),
      feedback_follow1 = lead(feedback, 1),
      response_prev1 = lag(response, 1)
    ) %>%
    ungroup()
  # compute win-stay
  # if there was a positive feedback in the previous trial, the image chosen
  # in the current trial is the same as in the previous trial?
  df_after_win <- d %>%
    dplyr::filter(feedback_prev1 == 1) %>%
    mutate(
      win_stay = ifelse(response == response_prev1, 1, 0)
    )
  
  df_ws_by_subj_imgpair <- df_after_win %>%
    group_by(stim) %>%
    summarise(
      win_stay = mean(win_stay, na.rm = TRUE)
    ) %>%
    ungroup()
  
  # compute lose-shift
  # if there was a negative feedback in the previous trial,the image chosen
  # in the current trial differs from the image shown in the previous trial?
  df_after_loss <- d %>%
    group_by(stim) %>%
    dplyr::filter(feedback_prev1 == 0) %>%
    mutate(
      lose_shift = ifelse(response != response_prev1, 1, 0)
    ) %>%
    ungroup()
  
  df_ls_by_subj_imgpair <- df_after_loss %>%
    group_by(stim) %>%
    summarise(
      lose_shift = mean(lose_shift, na.rm = TRUE)
    ) %>%
    ungroup()
  
  # merge winstay_mat and lose_shift by block
  df <- df_ws_by_subj_imgpair %>%
    inner_join(df_ls_by_subj_imgpair, 
               by = c("stim"))
  df$subj_code <- unique(d$subj_code)
  
  # message("Win-stay, lose-shift. Done processing subject number: ", 
  # subj_index)
  df
}


# number of participants
nsubj <- length(unique(d$subj_code))
nsubj

# Create list where to store the computed win-stay, lose-shift values for 
# each partecipant and each block of trials.
wsls_list <- list()

# Obtain the stay, lose-shift values.
for (i in 1:nsubj) {
  wsls_list[[i]] <- get_win_stay_lose_shift(d, i)
}

# Trasform the list in a data.frame.
wsls_df <- do.call(rbind.data.frame, wsls_list)

hist(wsls_df$lose_shift)

df <- left_join(wsls_df, diagcat_df, by = "subj_code")

df |> 
  group_by(diag_cat, stim) |> 
  summarize(
    win_stay = median(win_stay),
    lose_shift = median(lose_shift)
    
  )

d |> 
  group_by(diag_cat, stim) |> 
  summarise(
    y = mean(response, na.rm = T)
  )

fm <- glmer(
  response ~ stim * diag_cat + (stim | subj_code),
  family = binomial(),
  d
)


an_df <- df |> 
  dplyr::filter(diag_cat == "AN")

bf_ws <- bf(
  win_stay ~ stim + (stim | subj_code),
  phi ~ stim + (1 | subj_code)
) 

m1 <- brm(
  bf_ws,
  data = an_df,
  family = zero_one_inflated_beta(),
  chains = 4,
  cores = parallel::detectCores(),
  iter = 5000,
  warmup = 1000,
  control = list(adapt_delta = 0.99),
  init_r = 0.05,
  seed = 12345
  #backend = "cmdstanr"
)
pp_check(m1)
summary(m1)
bayes_R2(m1)
