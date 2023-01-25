# Script name: 30_recoding_for_stan.R
# Project: Eating disorders Montecatini
# Script purpose: Generate list for PRL Stan model.
# @author: Corrado Caudek <corrado.caudek@unifi.it>
# Date Created: Wed May 25 05:57:16 2022
# Last Modified Date: Wed May 25 05:57:16 2022
#
# 👉 Steinke's algorithm.

# rule_choice : which category is rewarded.
#   color  : 1
#   shape  : 2
#   number : 3


# resp_choice rew resp_color resp_shape resp_number

# Prelims
library("here")
library("tidyverse")
library("stringi")


# Select group.
GROUP <- "patients"  # "controls" "patients"

dir <- here("data", "raw", "wcst", GROUP)

if (GROUP == "patients") {
  file_names <- as.character(list.files(path=dir, pattern="wcst_pazienti"))
} else {
  file_names <- as.character(list.files(path=dir, pattern="wcst_eds1"))
}

n_files <- length(file_names)
n_files

d_list <- list()

for (i in 1:n_files) {
  
  d  <- read.table(
    here("data", "raw", "wcst", GROUP, file_names[i]), 
    header = FALSE
  )
  
  d$subj_name <- file_names[i]
  d$block <- rep(1:6, each = 10)
  
  d$card_shown <- d$V1
  d$correct_card <- d$V2
  d$card_chosen_if_perseveration <- d$V3
  d$trial_in_a_sequence <- d$V4 
  d$name_of_task <- d$V5
  d$card_shape <- d$V6
  d$card_number_of_symbols <- d$V7
  d$card_color <- d$V8
  d$rt <- d$V9
  d$is_correct <- d$V10 # 1=correct, 2=wrong card, 3=too slow
  d$chosen_card <- d$V11 # a number between 1 and 4, or 0 if none clicked
  d$is_error <- d$V12 
  # If 1, this trial was an error (otherwise 0); 0 or 1
  d$is_perseverative_error <- d$V13 
  # If 1, this trial was a perseveration error (otherwise 0); 0, 1
  d$is_non_perseverative_error <- d$V14
  # If 1, this trial was not a perseveration error (otherwise 0); 0, 1
  
  d_list[[i]] <- d
}

# convert list into data.frame
df0 <- do.call(rbind.data.frame, d_list)

length(unique(df0$subj_name))

# Examine accuracy.
bysubj_acc <- df0 %>% 
  group_by(subj_name) %>% 
  summarise(
    error_rate = mean(is_error)
  ) %>% as.data.frame()

# good_subj <- c(
#   "wcst_pazienti.2021-01-13-1802.data.df6df439-588d-4838-95d6-1b8e917d0143.txt",
#   "wcst_pazienti.2021-06-01-1529.data.7c4e8f61-1fcd-4b54-ac64-4b6dbe87746b.txt"
# )
# df <- df0[(df0$subj_name %in% good_subj), ]

bb <- boxplot(bysubj_acc$error_rate)
outliers_threshold <- bb$stats[5]

bad_subj_df <- bysubj_acc %>% 
  dplyr::filter(error_rate > outliers_threshold)

bad_subjects <- bad_subj_df$subj_name

# Remove bad subjects.
df1 <- df0[!(df0$subj_name %in% bad_subjects), ]
length(unique(df1$subj_name))


# df1 <- df1[1:60, ]
# df <- df1[1:(50*60), ]
# df <- df1[241:360, ]

df <- df1[!is.na(df1$chosen_card), ]

# df %>% 
#   group_by(subj_name) %>% 
#   summarise(
#     err = mean(is_error)
#   )


# Recoding as required by Steinke's algorithm. 
df <- df %>% 
  dplyr::rename(
    card_number = card_number_of_symbols
  )

length(unique(df$subj_name))

if (GROUP == "patients") {
  saveRDS(
    df,
    here::here("data", "processed", "wcst", "input_stan_patients.RDS")
  )
} else {
  saveRDS(
    df,
    here::here("data", "processed", "wcst", "input_stan_controls.RDS")
  )
}
  
  

# This function will be used by 101_get_mle_mod7.R.
get_one_subj_data_for_stan <- function(df) {
  
  ## ------------------------------------------------------------------
  ## rule_choice
  ## ------------------------------------------------------------------
  
  # rule_choice indicates the category (color = 1, shape = 2, 
  # number = 3) which is rewarded in each trial.
  
  df <- df %>%
    mutate(
      rule_choice = case_when(
        name_of_task == "color"  ~ 1,
        name_of_task == "shape"  ~ 2,
        name_of_task == "number" ~ 3,
        TRUE  ~ 999
      )
    )
  
  ## ------------------------------------------------------------------
  ## resp_choice
  ## ------------------------------------------------------------------
  
  # resp_choice indicates the key which the subject must press in order 
  # to provide a correct response in each trial. 
  # The four possible response keys are:
  # 1 -> one red circle
  # 2 -> two green triangles
  # 3 -> three blue crosses
  # 4 -> four yellow stars
  
  # in the psytoolbox output, this seems to correspond to correct_card.
  df$resp_choice <- df$correct_card
  
  ## ------------------------------------------------------------------
  ## resp_color
  ## ------------------------------------------------------------------
  
  # resp_color indicates the key that the subject would press if she
  # would chose the color dimension.
  
  df <- df %>%
    mutate(
      resp_color = case_when(
        card_color == "red"    ~ 1,
        card_color == "green"  ~ 2,
        card_color == "blue"   ~ 3,
        card_color == "yellow" ~ 4,
        TRUE  ~ 999
      )
    )
  
  ## ------------------------------------------------------------------
  ## resp_shape
  ## ------------------------------------------------------------------
  
  # resp_shape indicates the key that the subject would press if she
  # would chose the shape dimension.
  
  df <- df %>%
    mutate(
      resp_shape = case_when(
        card_shape == "circle"    ~ 1,
        card_shape == "triangle"  ~ 2,
        card_shape == "cross"     ~ 3,
        card_shape == "star"      ~ 4,
        TRUE  ~ 999
      )
    )
  
  ## ------------------------------------------------------------------
  ## resp_number
  ## ------------------------------------------------------------------
  
  # resp_number indicates the key that the subject would press if she
  # would chose the number dimension.
  
  df <- df %>%
    mutate(
      resp_number = case_when(
        card_number == 1  ~ 1,
        card_number == 2  ~ 2,
        card_number == 3  ~ 3,
        card_number == 4  ~ 4,
        TRUE  ~ 999
      )
    )
  
  ## ------------------------------------------------------------------
  ## rew
  ## ------------------------------------------------------------------
  
  # rew indicate whether a reward has been provided (1) or not (0)
  
  df$rew <- ifelse(df$is_correct == 1, 1, 0)
  
  
  ## ------------------------------------------------------------------
  ## los
  ## ------------------------------------------------------------------
  
  # low indicate whether a punishment has been provided (-1) or not (0)
  
  df$los <- ifelse(df$is_error == 1, -1, 0)
  
  
  ## ------------------------------------------------------------------
  ## holdout
  ## ------------------------------------------------------------------
  
  # holdout == 1 means that the subject is excluded from the analysis.
  # Here, holdout is always equal to 0.
  
  df$holdout <- rep(0, nrow(df))
  
  
  ## ------------------------------------------------------------------
  ## N
  ## ------------------------------------------------------------------
  
  # N is the number of subjects
  
  N <- length(unique(df$subj_name))
  
  
  ## ------------------------------------------------------------------
  ## T
  ## ------------------------------------------------------------------
  
  # T is the maximum number of trials for each subject. In our case
  # is always equal to 60.
  
  T <- 60
  
  
  ## ------------------------------------------------------------------
  ## Tsubj
  ## ------------------------------------------------------------------
  
  # Tsubj is the effective number of trials for each subject. In our
  # case is always equal to 60.
  
  Tsubj <- 60
  
  
  ## ------------------------------------------------------------------
  ## odd_response
  ## ------------------------------------------------------------------
  
  # odd_response seems to be the keypress of the subject (the chosen
  # category), with -1 indicating no response. Here, there are no -1.
  
  # In the psytoolbox output, this seems to correspond to chosen_card.
  df$odd_response <- df$chosen_card
  
  
  ## ------------------------------------------------------------------
  # Rearange data for modeling.
  ## ------------------------------------------------------------------
  
  Tsubj <- 60
  
  # each subject is in a single row.
  rew <- matrix(
    df$rew,
    nrow = N,
    byrow = TRUE
  )
  
  los <- matrix(
    df$los,
    nrow = N,
    byrow = TRUE
  )
  
  odd_response <- matrix(
    df$odd_response,
    nrow = N,
    byrow = TRUE
  )
  
  rule_choice <- matrix(
    df$rule_choice,
    nrow = N,
    byrow = TRUE
  )
  
  resp_choice <- matrix(
    df$resp_choice,
    nrow = N,
    byrow = TRUE
  )
  
  resp_color <- matrix(
    df$resp_color,
    nrow = N,
    byrow = TRUE
  )
  
  resp_shape <- matrix(
    df$resp_shape,
    nrow = N,
    byrow = TRUE
  )
  
  resp_number <- matrix(
    df$resp_number,
    nrow = N,
    byrow = TRUE
  )
  
  holdout = rep(0, N) # No data is hold out
  
  # Wrap-up data
  data <- list(
    N      = N,
    T      = T,
    Tsubj  = as.vector(rep(60, N)),
    rew    = rew,     
    los    = los,
    odd_response = odd_response,
    rule_choice = rule_choice,
    resp_choice = resp_choice,
    resp_color  = resp_color,
    resp_shape  = resp_shape,
    resp_number = resp_number,
    holdout     = rep(0, N) # No data is hold out
  )
  
  data
}


