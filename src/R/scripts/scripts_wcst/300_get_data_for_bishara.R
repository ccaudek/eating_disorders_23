## ------------------------------------------------------------------
## get_data_for_bishara.R
## 
## Project: Eating disorders and RL
## Purpose: Sequential learning models for the Wisconsin Card Sort 
##          Task data preparation
## Author: Corrado Caudek
## Date: "Sat Dec 26 07:25:43 2020"
## ------------------------------------------------------------------

# The 4 decks are the following:
# [1 red circle] [2 yellow triangles] [3 blue crosses] [4 green stars]
# The sequence of the correct dimension to be judged is:
# shape, number, color, shape, number, color.
# Each run (ex., shape) is made up of 10 trials.

# To get the subject data, run the 01_read_wcst_data.R script and get the df data.frame.
# df$name_of_task: which is the correct card dimension?
# detailed card description: card_shape, card_number_of_symbols, card_color
# The purpose is to generate the file correctdeck_matrix.txt.
# Is it different for each subject because the sequence of card presentation is different???

# I use the data of one subject for each run.  The data has to be repeated twice, because the 
# run_algorithm.R script expects at least two subjects!

# source("run_algorithm.R")

# The file correctdeck_matrix.txt looks like this:

# 1	2	3	4	5	6	7	8	9	10	11	12	13	14	15	16	17	18	19	20	21	22	23	24	25	26	27	28	29	30	31	32	33	34	35	36	37	38	39	40	41	42	43	44	45	46	47	48	49	50	51	52	53	54	55	56	57	58	59	60	61	62	63	64	65	66	67	68	69	70	71	72	73	74	75	76	77	78	79	80	81	82	83	84	85	86	87	88	89	90	91	92	93	94	95	96	97	98	99	100	101	102	103	104	105	106	107	108	109	110	111	112	113	114	115	116	117	118	119	120	121	122	123	124	125	126	127	128
# Color	2	1	4	1	2	3	4	1	2	3	4	1	4	3	2	4	1	3	1	3	4	2	3	4	1	3	4	1	2	1	2	1	4	3	2	3	4	3	2	4	1	4	3	2	1	3	4	3	2	3	4	2	1	2	4	2	1	3	1	2	4	3	1	2	2	1	4	1	2	3	4	1	2	3	4	1	4	3	2	4	1	3	1	3	4	2	3	4	1	3	4	1	2	1	2	1	4	3	2	3	4	3	2	4	1	4	3	2	1	3	4	3	2	3	4	2	1	2	4	2	1	3	1	2	4	3	1	2
# Form	1	3	1	4	2	3	1	4	3	4	2	1	3	2	1	4	2	4	1	4	2	3	2	1	2	1	3	4	2	3	4	2	4	3	2	1	4	2	4	3	1	2	4	1	2	3	4	1	3	2	1	2	4	1	3	4	3	1	3	4	2	3	1	3	1	3	1	4	2	3	1	4	3	4	2	1	3	2	1	4	2	4	1	4	2	3	2	1	2	1	3	4	2	3	4	2	4	3	2	1	4	2	4	3	1	2	4	1	2	3	4	1	3	2	1	2	4	1	3	4	3	1	3	4	2	3	1	3
# Number	1	4	2	1	4	1	4	3	4	2	1	3	2	1	3	4	2	3	4	1	2	3	2	3	4	2	3	4	2	1	4	1	3	4	1	4	2	3	1	4	1	3	4	2	3	2	1	3	2	4	1	3	2	4	1	3	2	1	3	2	4	3	2	1	1	4	2	1	4	1	4	3	4	2	1	3	2	1	3	4	2	3	4	1	2	3	2	3	4	2	3	4	2	1	4	1	3	4	1	4	2	3	1	4	1	3	4	2	3	2	1	3	2	4	1	3	2	4	1	3	2	1	3	2	4	3	2	1

# I need to know which is the correct card when the task is Color, or Form, or Number. 
# In the original file there are 128 trials.  I have only 60. 
# Test: run the original script with only 60 trials in the correctdeck_matrix.txt file.

# one_subj$chosen_card
# [1] 4 1 2 4 4 1 4 1 2 1 2 3 3 4 1 1 3 2 4 2 4 3 1 1 1 3 2 3 1 3 4 3 3 1 2 4 2 2 3 1 3 2 3 3 4 4 4
# [48] 2 2 2 3 2 2 3 4 4 4 1 4 2


# Prelims
library("here")
source(here("scripts", "01_prelims.R"))
source(here("libraries", "func_eating.R"))
library("stringi")


dir <- here("data", "raw", "wcst", "data")

file_names <- as.character(list.files(path=dir))
n_files <- length(file_names)

d_list <- list()

for (i in 1:n_files) {
  
  d  <- read.table(here("data", "raw", "wcst", "data", file_names[i]), header = FALSE)
  
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
df <- do.call(rbind.data.frame, d_list)


new_data <- df
n_subj <- length(unique(new_data$subj_name))

new_data$subjID <- rep(1:n_subj, each = 60)
new_data$trial <-  rep(seq(1:60), n_subj)

new_data$outcome <- ifelse(df$is_correct == 1, 1, 0)
new_data$choice <- df$chosen_card
new_data$block <- df$block

one_subj <- new_data %>% 
  dplyr::filter(subjID == 1)


# Create txt file with the responses of one subject.

resp <- one_subj$chosen_card
feedbk <- ifelse(one_subj$is_correct == 1, 1, 0)

temp <- c(resp, rep(0, 68), feedbk, rep(-1, 68))
row1 <- c(1, temp, 33, 33, 33, 33) # the first value in the row is 1!!

one_subj_data <- rbind(row1, row1)

write.table(
  one_subj_data,
  here("onesubjdata.txt"), 
  sep = "\t",
  quote = FALSE,
  row.names = FALSE,
  col.names = FALSE
)



# ---------------------------------------------------------------------------
# Generate the 'correctdeck matrix.txt' file
# tasks:
# shape, number, color, shape, number, color.
# decks:
# [1 red circle] [2 yellow triangles] [3 blue crosses] [4 green stars]

get_correct_desk <- function(df) {
  x <- case_when(
    # shape
    df$name_of_task == "shape" & df$card_shape == "circle"         ~ 1,
    df$name_of_task == "shape" & df$card_shape == "triangle"       ~ 2,
    df$name_of_task == "shape" & df$card_shape == "cross"          ~ 3,
    df$name_of_task == "shape" & df$card_shape == "star"           ~ 4,
    # number     
    df$name_of_task == "number" & df$card_number == 1   ~ 1,
    df$name_of_task == "number" & df$card_number == 2   ~ 2,
    df$name_of_task == "number" & df$card_number == 3   ~ 3,
    df$name_of_task == "number" & df$card_number == 4   ~ 4,
    # color     
    df$name_of_task == "color" & df$card_color == "red"            ~ 1,
    df$name_of_task == "color" & df$card_color == "green"          ~ 2,
    df$name_of_task == "color" & df$card_color == "blue"           ~ 3,
    df$name_of_task == "color" & df$card_color == "yellow"         ~ 4,
    
    TRUE                                                           ~ 9
  )
  x
}


if (0) {
  data.frame(
    get_correct_desk(one_subj),
    one_subj$correct_card,
    one_subj$name_of_task,
    one_subj$card_shown
  )
}


get_color_desk <- function(df) {
  x <- case_when(
    df$card_color == "red"     ~ 1,
    df$card_color == "green"   ~ 2,
    df$card_color == "blue"    ~ 3,
    df$card_color == "yellow"  ~ 4,
    TRUE                       ~ 9
  )
  x
}

get_form_desk <- function(df) {
  x <- case_when(
    df$card_shape == "circle"   ~ 1,
    df$card_shape == "triangle" ~ 2,
    df$card_shape == "cross"    ~ 3,
    df$card_shape == "star"     ~ 4,
    TRUE                        ~ 9
  )
  x
}

get_number_desk <- function(df) {
  x <- case_when(
    df$card_number == 1  ~ 1,
    df$card_number == 2  ~ 2,
    df$card_number == 3  ~ 3,
    df$card_number == 4  ~ 4,
    TRUE                            ~ 9
  )
  x
}


d <- data.frame()
d <- rbind(
  d, 
  data.frame(
    trial = 1:128,
    form = c(get_form_desk(one_subj), rep(0, 68)),
    number = c(get_number_desk(one_subj), rep(0, 68)),
    color = c(get_color_desk(one_subj), rep(0, 68))
    )
)


# transpose 
d_t <- as.data.frame(t(d))

d_t$names <- factor(c("  ", "Form", "Number", "Color"))

foo <- d_t %>% 
  dplyr::relocate(names, .before = V1)


write.table(
  foo,
  here("correctdeck.txt"), 
  sep = "\t",
  quote = FALSE,
  row.names = FALSE,
  col.names = FALSE
)


# --------------------------------------------------------------------------




source(here("scripts", "jmp", "run_algorithm.R"))











subj_names <- unique(df$subj_name)

s1 <- df %>% 
  dplyr::filter(subj_name == subj_names[1])

s2 <- df %>% 
  dplyr::filter(subj_name == subj_names[2])

row1 <- c(s1$chosen_card, rep(0, 68), ifelse(s1$is_correct == 1, 1, 0), rep(-1, 68))
row2 <- c(s2$chosen_card, rep(0, 68), ifelse(s2$is_correct == 1, 1, 0), rep(-1, 68))

length(row1)
length(row2)

foo <- rbind(row1, row2)
dim(foo)

# prl_ewa() needs a txt file as input
write.table(
  foo,
  "mydata.txt", 
  sep = "\t",
  quote = FALSE,
  row.names = FALSE,
  col.names = FALSE
)


