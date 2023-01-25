# Script name: 021_get_bmi.R
# Project: Eating disorders, Montecatini
# Script purpose: get BMI.
# @author: Corrado Caudek <corrado.caudek@unifi.it>
# Date Created: Mon Nov  8 14:59:08 2021
# Last Modified Date: Mon Nov  8 14:59:08 2021
# 
# Notes: 


# Prelims -----------------------------------------------------------------

suppressPackageStartupMessages({
  library("here")
  library("tidyverse")
})
library("cmdstanr")
library("brms")

# Increase max print
options(max.print = .Machine$integer.max)

cores <- parallel::detectCores()
cores

source(here("code", "functions", "funs_prl.R"))
source(here("code", "functions", "funs_quest.R"))
source(here("code", "functions", "funs_rescorla_wagner.R"))
source(here("code", "functions", "funs_careless_resp.R"))
source(here("code", "functions", "funs_gen_data_for_hddm.R"))
source(here("code", "functions", "funs_param_analyses.R"))
source(here("code", "functions", "funs_param_classification_analyses.R"))


if (
  !file.exists(here::here("data", "processed", "quest", 
                          "bmi_age_weight.rds"))
  ) save_bmi_age_weight()


df <- get_demo_info()




  
  
  
