# Script name: 001_careless_responding.R
# Project: Eating disorders - Montecatini and controls
# Script purpose: flag participants for careless responding 
# @author: Corrado Caudek <corrado.caudek@unifi.it>
# Date Created: Sun Jun 20 08:40:10 2021
# Last Modified Date: Tue May 31 15:33:17 2022
# 
# Notes: 
#   Purpose: flag participants for careless responding on questionnaries data.


# Prelims
suppressPackageStartupMessages({
  library("here")
  library("tidyverse")
  library("careless")
})

# Increase max print
options(max.print = .Machine$integer.max)


source(here::here("scripts", "functions", "funs_input_careless_resp.R"))



# 1. Generate input for careless responding analysis ----------------------

# here::here("data", "processed", "quest", "input_careless_resp.rds")
save_input_careless_resp()



# 2.  Generate list of data.frames ----------------------------------------

# Read complete raw data.
input_careless_resp <- readRDS(
  here::here("data", "processed", "quest", "input_careless_resp.rds")
)

# BSQ-14
bsq_data <- input_careless_resp %>% 
  dplyr::select(num_range("bsq_", 1:14))
# table(bsq_data[, 1])

# Rosenberg
ros_data <- input_careless_resp %>% 
  dplyr::select(num_range("ros_", 1:10))
# table(ros_data[, 2])

# DASS-21
dass_data <- input_careless_resp %>% 
  dplyr::select(num_range("dass_", 1:21))
table(dass_data[, 2])

# SIAS
sias_data <- input_careless_resp %>% 
  dplyr::select(num_range("sias_", 1:19))
table(sias_data[, 2])

# MPS - Multidimensional Perfectionism Scale (MPS; Frost, Marten, Lahart e Rosenblate, 1990).
mps_data <- input_careless_resp %>% 
  dplyr::select(num_range("mps_", 1:35))
table(mps_data[, 8])

# ORTO
orto_data <- input_careless_resp %>%
  dplyr::select(num_range("orto_", 1:15))

# EAT-26
eat26_data <- input_careless_resp %>% 
  dplyr::select(num_range("eat26_", 1:26))


# Select only numerical values for the questionnaires data.
qdata <- bind_cols(
  bsq_data, ros_data, dass_data, sias_data, mps_data, eat26_data #  orto_data,
) 

# list of data.frames
qlist <- list(
  bsq_data, ros_data, dass_data, sias_data, mps_data, eat26_data # orto_data, 
) 


# Person total correlation ----

cm <- colMeans(qdata)
person_tot_cor <- apply(qdata, 1, function(x) cor(x, cm)) 

# out <- boxplot(person_tot_cor)
foo <- data.frame(
  subj_code = input_careless_resp$subj_code,
  person_tot_cor
  ) %>%
  dplyr::filter(person_tot_cor < out$stats[1])
# Flagged IDs and LPA data.frame.
bad_ids_person_tot_cor <- foo$subj_code  # <<-----------------------------------

person_tot_cor_df <- data.frame(
  subj_code = input_careless_resp$subj_code,
  person_tot_cor
)


# Longstring ----

# The Longstring index, available via longstring(), is defined as the longest 
# consecutive string of identical responses given by a person.  If a person 
# gives the same response consecutively over a long stretch of items, this can 
# be taken as an indication of careless responding.

# apply careless::longstring() function to the data.frame of each of the 
# seven questionnaires
out <- purrr::map(qlist, careless::longstring)

# convert list to data.frame and transpose
df <- do.call(rbind.data.frame, out) %>% 
  t() %>% 
  as.data.frame()
# remove row names (they are not meaningful here)
rownames(df) <- NULL
# provide columns' names
names_longstring_quest <- c(
  "bsq_longstring", "ros_longstring", "dass_longstring", "sias_longstring", 
  "mps_longstring", "eat26_longstring"
)
names(df) <- names_longstring_quest

df$subj_code <- input_careless_resp$subj_code

longstring_df <- df

boxplot(df[, 6])
out <- boxplot(df[, 2])
foo <- data.frame(
  subj_code = input_careless_resp$subj_code,
  y1 = df[, 2]
)
foo1 <- foo[, c(1, 2)] %>%
  dplyr::filter(y1 > out$stats[5])
bad_ids_longstring <- foo1$subj_code  # <<--------------------------------------


# Intra-individual response variability ----

# The intra-individual response variability (IRV) is defined as the 
# "standard deviation of responses across a set of consecutive item responses 
# for an individual" (Dunn et al. 2018).

out <- map(qlist, careless::irv)

irv_df <- do.call(rbind.data.frame, out) %>% 
  t() %>% 
  as.data.frame()
# remove row names (they are not meaningful here)
rownames(irv_df) <- NULL
# provide columns' names
names(irv_df) <- c(
  "bsq_irv", "ros_irv", "dass_irv", "sias_irv", "mps_irv", #"orto_irv", 
  "eat26_irv"
)

irv_df$subj_code <- input_careless_resp$subj_code

cor(
  irv_df[, -7]
) %>% round(2)

out <- boxplot(irv_df[, 1])
foo <- data.frame(
  subj_code = input_careless_resp$subj_code,
  y1 = irv_df[, 1]
)
foo1 <- foo %>%
  dplyr::filter(y1 > out$stats[5])
bad_ids_irv <- foo1$subj_code  <<-----------------------------------------------



# Mahalanobis Distance ----

out <- purrr::map(qlist, careless::mahad, plot = FALSE, flag = FALSE)

# convert list to data.frame, transpose
mahad_df <- do.call(rbind.data.frame, out) %>% 
  t() %>% 
  as.data.frame()
# remove row names (they are not meaningful here)
rownames(mahad_df) <- NULL
# provide columns' names
names(mahad_df) <- c(
  "bsq_mahad", "ros_mahad", "dass_mahad", "sias_mahad", 
  "mps_mahad", "eat26_mahad"
)

mahad_df$subj_code <- input_careless_resp$subj_code

boxplot(mahad_df[, 1])

out <- boxplot(mahad_df[, 6])
foo <- data.frame(
  subj_code = input_careless_resp$subj_code,
  y1 = mahad_df[, 1]
)
foo1 <- foo[, c(1, 2)] %>%
  dplyr::filter(y1 > out$stats[5])
bad_ids_maha1 <- foo1$subj_code

out <- boxplot(mahad_df[, 2])
foo <- data.frame(
  subj_code = input_careless_resp$subj_code,
  y1 = mahad_df[, 2]
)
foo1 <- foo[, c(1, 2)] %>%
  dplyr::filter(y1 > out$stats[5])
bad_ids_maha2 <- foo1$subj_code

out <- boxplot(mahad_df[, 3])
foo <- data.frame(
  subj_code = input_careless_resp$subj_code,
  y1 = mahad_df[, 3]
)
foo1 <- foo[, c(1, 2)] %>%
  dplyr::filter(y1 > out$stats[5])
bad_ids_maha3 <- foo1$subj_code

out <- boxplot(mahad_df[, 4])
foo <- data.frame(
  subj_code = input_careless_resp$subj_code,
  y1 = mahad_df[, 4]
)
foo1 <- foo[, c(1, 2)] %>%
  dplyr::filter(y1 > out$stats[5])
bad_ids_maha4 <- foo1$subj_code

out <- boxplot(mahad_df[, 5])
foo <- data.frame(
  subj_code = input_careless_resp$subj_code,
  y1 = mahad_df[, 5]
)
foo1 <- foo[, c(1, 2)] %>%
  dplyr::filter(y1 > out$stats[5])
bad_ids_maha5 <- foo1$subj_code

out <- boxplot(mahad_df[, 6])
foo <- data.frame(
  subj_code = input_careless_resp$subj_code,
  y1 = mahad_df[, 6]
)
foo1 <- foo[, c(1, 2)] %>%
  dplyr::filter(y1 > out$stats[5])
bad_ids_maha6 <- foo1$subj_code

u2 <- union(bad_ids_maha1, bad_ids_maha2)
u3 <- union(u2, bad_ids_maha3)
u4 <- union(u3, bad_ids_maha4)
u5 <- union(u4, bad_ids_maha5)
u6 <- union(u5, bad_ids_maha6)
bad_ids_maha <- u6  <<----------------------------------------------------------

rm(out)



# Evenodd -- NOT USED!

unidim_data <- list(
  bsq_data, ros_data, sias_data, orto_data
  )

lnc <- map(unidim_data, ncol)
do.call(rbind.data.frame, lnc)[, 1]

unidim_data2 <- bind_cols(
  bsq_data, ros_data, sias_data,  orto_data
)

careless_eo <- careless::evenodd(unidim_data2, factors = do.call(rbind.data.frame, lnc)[, 1])

evenodd_df <- tibble(
  eo = careless_eo,
  subj_code = input_careless_resp$subj_code
)

# boxplot(careless_eo)



# Psychometric Synonyms and Antonyms ---- 

# Synonyms
psychsyn_cor <- careless::psychsyn_critval(qdata)
head(psychsyn_cor)
boxplot(psychsyn_cor$Freq)
sum(psychsyn_cor$Freq > .60, na.rm = TRUE)

psychsyn <- careless::psychsyn(qdata, critval = .60)
boxplot(psychsyn, main = "Histogram of psychometrical synonyms index")
# Observations whose correlations are close to zero or even negative 
# should be inspected more closely.
# In the sample there are no values close to zero!

foo <- data.frame(
  subj_code = input_careless_resp$subj_code,
  psychsyn
) %>%
  dplyr::filter(psychsyn < .2)

# Flagged IDs and LPA data.frame.
bad_ids_psychsyn <- foo$subj_code  <<-------------------------------------------

psychsyn_df <- data.frame(
  subj_code = input_careless_resp$subj_code,
  psychsyn
)


# Antonyms

psychant_cor <- careless::psychsyn_critval(qdata, anto = TRUE)
head(psychant_cor)
sum(psychant_cor$Freq > .60, na.rm = TRUE)
psychanto <- careless::psychsyn(qdata, critval = .60, anto = TRUE)
boxplot(psychanto, main = "Histogram of psychometrical Antonyms index")
# There are a few values close to zero or positive.

# foo <- data.frame(
#   subj_code = input_careless_resp$subj_code,
#   psychanto
# ) %>% 
#   dplyr::filter(psychanto > -.2)

# Flagged IDs and LPA data.frame.
bad_ids_psychant <- foo$subj_code
length(bad_ids_psychant)

psychant_df <- data.frame(
  subj_code = input_careless_resp$subj_code,
  psychanto
)



#-------------------------------------------------------------------------------

# Find subset of participants who are flagged for potentian careless responding.

bad_ids <- unique(
  c(bad_ids_person_tot_cor, bad_ids_longstring, bad_ids_irv, bad_ids_maha, 
  bad_ids_psychsyn)
)

saveRDS(
  bad_ids,
  here::here("scripts", "R", "scripts_quest", "02_flag_careless_resp", 
             "bad_ids_from_careless_resp_indices.rds")
)



######## E N D 





# 
# # CR index ----
# 
# # To compute the CR index for each person, we multiplied 
# # person-total-correlation, psychometric synonyms, and IRV by -1, 
# # so that all measures of CR are scaled in the same direction (i.e., higher 
# # values indicating a stronger carelessness). 
# # Next, we z-transformed (i.e., standardized) the seven indices and then 
# # computed an aggregate score by averaging across indices. 
# 
# 
# 
# person_tot_cor_df$person_tot_cor <- -1 * person_tot_cor_df$person_tot_cor
# 
# psychsyn_df$psychsyn <- -1 * psychsyn_df$psychsyn
# 
# lpa_psychant
# 
# irv_df <- -1 * irv_df
# irv_df$subj_code <- input_careless_resp$subj_code
# 
# 
# 
# temp1 <- left_join(longstring_df, person_tot_cor_df)
# temp2 <- left_join(temp1, psychsyn_df)
# temp3 <- left_join(temp2, psychant_df)
# temp4 <- left_join(temp3, mahad_df)
# temp5 <- left_join(temp4, irv_df)
# 
# # temp6 <- left_join(rw_params, temp5, by = "subj_code")
# 
# careless_df <- temp5
# 
# dfs <- careless_df %>%
#   mutate(
#     across(
#       where(is.numeric), ~(scale(.) %>% as.vector)
#     )
#   )
# 
# foo <- dfs %>% 
#   dplyr::select(-subj_code)
# 
# print(cor(foo, use = "complete.obs"), 2)
# 
# 
# ee <- eigen(cor(foo, use = "complete.obs"))
# 
# 
# dfs$cr_index <- rowMeans(foo)
# 
# hist(dfs$cr_index)
# boxplot(dfs$cr_index)
# 
# out <- boxplot(dfs$cr_index)
# foo <- data.frame(
#   subj_code = quest_raw$subj_code,
#   cr_index = dfs$cr_index
# ) 
# 
# foo1 <- foo %>% 
#   dplyr::filter(cr_index > out$stats[5])
# 
# bad_ids_cr_index <- foo1$subj_code
# sort(bad_ids_cr_index)
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# dfs$subj_code <- NULL
# 
# 
# 
# cr_index <- colSums(dfs)
# 
# careless_long <- longstring(bsq_data)
# boxplot(careless_long, main = "Boxplot of Longstring index")
# 
# careless_long <- longstring(eat26_data, avg = TRUE)
# boxplot(careless_long$avg, main = "Boxplot of Averagestring index")
# 
# careless_irv <- irv(eat26_data, split = TRUE, num.split = 4)
# head(careless_irv)
# hist(careless_irv$irvTotal)
# 
# psychsyn_cor <- psychsyn_critval(eat26_data)
# head(psychsyn_cor)
# hist(psychsyn_cor$Freq)
# sum(psychsyn_cor$Freq > .60, na.rm = TRUE)
# 
# 
# example_psychsyn <- psychsyn(bsq_data, critval = .60)
# hist(example_psychsyn, main = "Histogram of psychometrical synonyms index")
# 
# psychant_cor <- psychsyn_critval(bsq_data, anto = TRUE)
# head(psychant_cor)
# hist(psychant_cor$Freq)
# 
# careless_mahad <- mahad_raw <- mahad(eat26_data)
# 
# 
# subj_code <- quest_raw$subj_code
# 
# mahad_fnc <- function(df, subj_code) {
#   
#   preds_mahad <- mahad(df, plot = FALSE, flag = TRUE, confidence = 0.99)
#   
#   data.frame(
#     subj_code = subj_code, 
#     mahad = preds_mahad$d_sq,
#     mahad_flag = preds_mahad$flagged
#   )
# }
# 
# temp1 <- mahad_fnc(bsq_data, subj_code)
# temp2 <- mahad_fnc(ros_data, subj_code)
# temp3 <- mahad_fnc(dass_data, subj_code)
# 
# temp3 %>% 
#   dplyr::filter(mahad_flag == TRUE) %>% 
#   dplyr::select(subj_code) 
# 
# 
# 
# 
# preds_psychant <- psychant(bsq_data)$cor %>% 
#   #replace_na(., 0) %>%
#   as.numeric 
# # hist(preds_psychant)
# 
# out <- longstring(bsq_data, avg = FALSE)
# boxplot(out)
# 
# 
# # Using solitude for anomaly detection
# # Not useful.
# set.seed(1)
# index = sample(ceiling(nrow(qdata) * 0.5))
# 
# # initiate an isolation forest
# iso = isolationForest$new(sample_size = length(index))
# # fit for attrition data
# iso$fit(qdata[index, ])
# 
# # Obtain anomaly scores
# scores_train = iso$predict(qdata[index, ])
# scores_train[order(anomaly_score, decreasing = TRUE)]
# 
# # predict scores for unseen data (50% sample)
# scores_unseen = iso$predict(qdata[-index, ])
# scores_unseen[order(anomaly_score, decreasing = TRUE)]

