# Script name: 01_get_traces.R
# Project: eating disorders
# Script purpose: get traces produced by 01_hddmrl_stim_grp_302.ipynb
# @author: Corrado Caudek <corrado.caudek@unifi.it>
# Date Created: date_created
# Last Modified Date: Tue Nov 22 16:24:58 2022
#
# 👉 

# load pkgs ====
library("here")
library("tidyverse")
library("rio")

# load ddm raw traces (without burn-in) ====
tracs <- rio::import(
  here::here("src", "python", "PRL", "01_get_hddmrl_params", "ddm", "ddm_grptrcs.csv")
  )

str(tracs)
tr.nms = names(tracs)

#-----------------------------------------------
# Threshold (a) traces 
#-----------------------------------------------
tracs.a <- tracs[, str_detect(tr.nms, "^a\\(")]

a_pars <- tracs.a %>%
  as.data.frame() %>%
  mutate(draw = seq(1, n()), parameter = "a")

# Select columns
a_pars <- a_pars %>% 
  dplyr::select(`a(AN.food)`, `a(AN.neutral)`, `a(HC.food)`, `a(HC.neutral)`,
                draw, parameter)

# Change columns names for easier handling.
a_new_names <- c(
  "AN.food", "AN.neutral", "HC.food", "HC.neutral", "draw", "parameter"
)
colnames(a_pars) <- a_new_names

# long format for easier handling
a_long <- pivot_longer(
  data = a_pars, values_to = "val",
  names_to = "task_idx", 
  cols = c("AN.food", "AN.neutral", "HC.food", "HC.neutral")
)

# Split task_idx column into diag_cat and stim.
a_long <- a_long %>% separate(task_idx, c('diag_cat', 'stim'))
str(a_long)

#-----------------------------------------------
# Drift-rate (v) traces
#-----------------------------------------------
tracs.v <- tracs[, str_detect(tr.nms, "^v\\(")]

v_pars <- tracs.v %>%
  as.data.frame() %>%
  mutate(draw = seq(1, n()), parameter = "v")

# Select columns
v_pars <- v_pars %>% 
  dplyr::select(`v(AN.food)`, `v(AN.neutral)`, `v(HC.food)`, `v(HC.neutral)`,
                draw, parameter)

# Change columns names for easier handling.
v_new_names <- c(
  "AN.food", "AN.neutral", "HC.food", "HC.neutral", "draw", "parameter"
)
colnames(v_pars) <- v_new_names

# long format for easier handling
v_long <- pivot_longer(
  data = v_pars, values_to = "val",
  names_to = "task_idx", 
  cols = c("AN.food", "AN.neutral", "HC.food", "HC.neutral")
)

# Split task_idx column into diag_cat and stim.
v_long <- v_long %>% separate(task_idx, c('diag_cat', 'stim'))
str(v_long)

#-----------------------------------------------
# NDT (t) traces
#-----------------------------------------------
tracs.t <- tracs[, str_detect(tr.nms, "^t\\(")]

t_pars <- tracs.t %>%
  as.data.frame() %>%
  mutate(draw = seq(1, n()), parameter = "t")

# Select columns
t_pars <- t_pars %>% 
  dplyr::select(`t(AN.food)`, `t(AN.neutral)`, `t(HC.food)`, `t(HC.neutral)`,
                draw, parameter)

# Change columns names for easier handling.
t_new_names <- c(
  "AN.food", "AN.neutral", "HC.food", "HC.neutral", "draw", "parameter"
)
colnames(t_pars) <- t_new_names

# long format for easier handling
t_long <- pivot_longer(
  data = t_pars, values_to = "val",
  names_to = "task_idx", 
  cols = c("AN.food", "AN.neutral", "HC.food", "HC.neutral")
)

# Split task_idx column into diag_cat and stim.
t_long <- t_long %>% separate(task_idx, c('diag_cat', 'stim'))
str(t_long)

#-----------------------------------------------
# alpha traces
#-----------------------------------------------
tracs.alpha <- tracs[, str_detect(tr.nms, "^alpha\\(")]

alpha_pars <- tracs.alpha %>%
  as.data.frame() %>%
  mutate(draw = seq(1, n()), parameter = "alpha")

# Select columns
alpha_pars <- alpha_pars %>% 
  dplyr::select(`alpha(AN.food)`, `alpha(AN.neutral)`, `alpha(HC.food)`, 
                `alpha(HC.neutral)`, draw, parameter)

# Change columns names for easier handling.
alpha_new_names <- c(
  "AN.food", "AN.neutral", "HC.food", "HC.neutral", "draw", "parameter"
)
colnames(alpha_pars) <- alpha_new_names

# long format for easier handling
alpha_long <- pivot_longer(
  data = alpha_pars, values_to = "val",
  names_to = "task_idx", 
  cols = c("AN.food", "AN.neutral", "HC.food", "HC.neutral")
)

# Split task_idx column into diag_cat and stim.
alpha_long <- alpha_long %>% separate(task_idx, c('diag_cat', 'stim'))
str(alpha_long)

#-----------------------------------------------
# pos_alpha traces
#-----------------------------------------------
tracs.pos_alpha <- tracs[, str_detect(tr.nms, "^pos_alpha\\(")]

pos_alpha_pars <- tracs.pos_alpha %>%
  as.data.frame() %>%
  mutate(draw = seq(1, n()), parameter = "pos_alpha")

# Select columns
pos_alpha_pars <- pos_alpha_pars %>% 
  dplyr::select(`pos_alpha(AN.food)`, `pos_alpha(AN.neutral)`, 
                `pos_alpha(HC.food)`, `pos_alpha(HC.neutral)`, draw, parameter)

# Change columns names for easier handling.
pos_alpha_new_names <- c(
  "AN.food", "AN.neutral", "HC.food", "HC.neutral", "draw", "parameter"
)
colnames(pos_alpha_pars) <- pos_alpha_new_names

# long format for easier handling
pos_alpha_long <- pivot_longer(
  data = pos_alpha_pars, values_to = "val",
  names_to = "task_idx", 
  cols = c("AN.food", "AN.neutral", "HC.food", "HC.neutral")
)

# Split task_idx column into diag_cat and stim.
pos_alpha_long <- pos_alpha_long %>% separate(task_idx, c('diag_cat', 'stim'))
str(pos_alpha_long)

#-----------------------------------------------
# Save all traces in Rdata to use within R
#-----------------------------------------------
# long and wide
save(
  file= here::here("data", "processed", "prl", "output_hddm", "traces.Rda"), 
  a_long,
  v_long,
  t_long,
  alpha_long,
  pos_alpha_long
)

# eof ---


