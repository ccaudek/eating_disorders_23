## ------------------------------------------------------------------
## Filename.R
## 
## Project: 
## Purpose: 
## Author: Corrado Caudek
## Date: 
## ------------------------------------------------------------------


# Prelims
library("here")
source(here("scripts", "01_prelims.R"))
library("stringi")


library("cmdstanr")
library("posterior")
library("bayesplot")
color_scheme_set("brightblue")
library("recipes")

dir <- here("scripts", "_wcst", "_test_data")


file_names <- as.character(list.files(path=dir))
n_files <- length(file_names)


d_list <- list()

for (i in 1:n_files) {
  
  d  <- read.table(here("scripts", "_wcst", "_test_data", file_names[i]), header = FALSE)
  
  d$subj_name <- file_names[i]
  # d$block <- rep(1:6, each = 10)
  
  d$card_shown <- d$V1
  d$correct_card <- d$V2
  d$card_chosen_if_perseveration <- d$V3
  # d$trial_in_a_sequence <- d$V4 
  d$name_of_task <- d$V22 # myrule2
  d$card_shape <- d$V6
  d$card_number_of_symbols <- d$V7
  d$card_color <- d$V8
  d$rt <- d$V9
  d$is_correct <- d$V16 # 1=correct, -1=incorrect
  d$chosen_card <- d$V11 # a number between 1 and 4, or 0 if none clicked
  # d$is_error <- d$V12 
  # If 1, this trial was an error (otherwise 0); 0 or 1
  # d$is_perseverative_error <- d$V13 
  # If 1, this trial was a perseveration error (otherwise 0); 0, 1
  # d$is_non_perseverative_error <- d$V14
  # If 1, this trial was not a perseveration error (otherwise 0); 0, 1
  
  d_list[[i]] <- d
  
}

# convert list into data.frame
df <- do.call(rbind.data.frame, d_list)

df <- df %>% 
  dplyr::rename(
    card_number = card_number_of_symbols
  )


# Recoding as required by Steinke's algorithm. 

## ------------------------------------------------------------------
## rule_choice
## ------------------------------------------------------------------

df$rule_choice <- dplyr::recode(
  factor(df$name_of_task),
  `3` = "1",  # old = new
  `1` = "2",
  `2` = "3"
)


df$rule_choice <- as.numeric(as.character(df$rule_choice))


# data.frame(
#   df$rule_choice, df$name_of_task
# )[1:30, ]

# rule_choice indicates the category (color = 1, shape = 2, 
# number = 3) which is rewarded in each trial.


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
      card_color == "yellow" ~ 3,
      card_color == "blue"   ~ 4,
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
      card_shape == "triangle"  ~ 1,
      card_shape == "star"      ~ 2,
      card_shape == "cross"     ~ 3,
      card_shape == "circle"    ~ 4,
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
## resp_choice
## ------------------------------------------------------------------

# resp_choice indicates the key which the subject must press in order 
# to provide a correct response in each trial. 
# The four possible response keys are:
# 1 -> one red triangle
# 2 -> two green stars
# 3 -> three yellow crosses
# 4 -> four blue circles


# rule_choice indicates the category (color = 1, shape = 2, 
# number = 3) which is rewarded in each trial.


df <- df %>%
  mutate(
    resp_choice = case_when(
      rule_choice == 1  ~ resp_color,
      rule_choice == 2  ~ resp_shape,
      rule_choice == 3  ~ resp_number,
      TRUE  ~ 999
    )
  )


data.frame(
  df$rule_choice, df$resp_choice, df$resp_color, df$resp_shape, df$resp_number
)[1:30, ]



## ------------------------------------------------------------------
## rew
## ------------------------------------------------------------------

# rew indicate whether a reward has been provided (1) or not (0)

df$rew <- ifelse(df$is_correct == 1, 1, 0)


## ------------------------------------------------------------------
## los
## ------------------------------------------------------------------

# low indicate whether a punishment has been provided (-1) or not (0)

df$los <- ifelse(df$is_correct == -1, -1, 0)


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

T <- 120


## ------------------------------------------------------------------
## Tsubj
## ------------------------------------------------------------------

# Tsubj is the effective number of trials for each subject. In our
# case is always equal to 60.

Tsubj <- 120


## ------------------------------------------------------------------
## odd_response
## ------------------------------------------------------------------

# odd_response seems to be the keypress of the subject (the chosen
# category), with -1 indicating no response. Here, there are no -1.

df$odd_response <- ifelse(
  !(df$chosen_card == df$resp_color | 
      df$chosen_card == df$resp_shape | 
      df$chosen_card == df$resp_number), 1, 0
)

summary(df$odd_response)


## ------------------------------------------------------------------
# Rearange data for modeling.
## ------------------------------------------------------------------

Tsubj <- 120

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
  Tsubj  = as.vector(rep(120, N)),
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

# descriptive statistics of feedback
mean(rowSums(data$rew)/data$Tsubj) # positive
sd(rowSums(data$rew)/data$Tsubj)
mean(rowSums(abs(data$los))/data$Tsubj) # negative
sd(rowSums(abs(data$los))/data$Tsubj)


# compute perseverative errors ----

# color = 1, shape = 2, number = 3) 

foo <- data.frame(
  chosen_card = df$chosen_card,
  rule_choice = df$rule_choice,
  resp_choice=df$resp_choice,
  chosen_card=df$chosen_card,
  is_correct=df$is_correct,
  resp_color=df$resp_color,
  resp_shape=df$resp_shape,
  resp_number=df$resp_number
)

table(df$rule_choice)


foo$is_rule_changed <- rep(0, nrow(foo))

for (i in 2:nrow(foo)) {
  if (foo$rule_choice[i] == foo$rule_choice[i-1])
    foo$is_rule_changed[i] = 0
  else
    foo$is_rule_changed[i] = 1
}

head(foo, 100)


out_rle <- rle(foo$rule_choice)
length_of_first_epoch <- out_rle$lengths[1]

# which is the correct rule in the previous epoch?
foo$correct_rule_previous_epoch <- rep(0, nrow(foo))

for (i in length_of_first_epoch:nrow(foo)) {
  if (foo$is_rule_changed[i] == 1)
    foo$correct_rule_previous_epoch[i] = foo$rule_choice[i-1]
  else
    foo$correct_rule_previous_epoch[i] = foo$correct_rule_previous_epoch[i-1] 
}


# data.frame(
#   foo$rule_choice,
#   foo$is_rule_changed,
#   foo$correct_rule_previous_epoch
# )[1:100, ]


foo <- foo %>%
  mutate(
    cor_key_for_prev_epoch = case_when(
      correct_rule_previous_epoch == 1  ~ resp_color,
      correct_rule_previous_epoch == 2  ~ resp_shape,
      correct_rule_previous_epoch == 3  ~ resp_number,
      TRUE  ~ 999
    )
  )


data.frame(
  # col = foo$resp_color,
  # sh = foo$resp_shape,
  # nu = foo$resp_number,
  cor_rule_prev=foo$correct_rule_previous_epoch,
  cor_key_prev = foo$cor_key_for_prev_epoch,
  resp=foo$chosen_card,
  foo$is_rule_changed,
  is_correct = foo$is_correct
)


foo$is_persv_error <- rep(NA, nrow(foo))

for (i in length_of_first_epoch:nrow(foo)) {
  if (foo$is_correct[i] == -1 & 
      foo$is_correct[i-1] == -1 &
      foo$chosen_card[i] == foo$cor_key_for_prev_epoch[i] &
      foo$is_rule_changed[i-1] == 1) {
    foo$is_persv_error[i] <- 1
  } else if (foo$is_correct[i] == -1 &
      (foo$is_correct[i-1] == -1 & foo$is_correct[i-2] == -1) &
      foo$chosen_card[i] == foo$cor_key_for_prev_epoch[i] &
      foo$is_rule_changed[i-2] == 1) {
    foo$is_persv_error[i] <- 1
  } else if (foo$is_correct[i] == -1 &
             (foo$is_correct[i-1] == -1 & foo$is_correct[i-2] == -1 & foo$is_correct[i-3] == -1) &
             foo$chosen_card[i] == foo$cor_key_for_prev_epoch[i] &
             foo$is_rule_changed[i-3] == 1) {
    foo$is_persv_error[i] <- 1
  } 
  # else if (foo$is_correct[i] == -1 &
  #            # (foo$is_correct[i-1] == -1 & foo$is_correct[i-2] == -1 & foo$is_correct[i-3] == -1 & foo$is_correct[i-4] == -1) &
  #            foo$chosen_card[i] == foo$cor_key_for_prev_epoch[i] 
  #            #foo$is_rule_changed[i-4] == 1
  #            ) {
  #   foo$is_persv_error[i] <- 1
  # }
  else {
    foo$is_persv_error[i] <- 0
  }
}


table(df$rule_choice)

