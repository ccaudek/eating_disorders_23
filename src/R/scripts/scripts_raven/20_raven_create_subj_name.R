## ------------------------------------------------------------------
## 20_raven_create_subj_name.R
## 
## Project: 
## Purpose: 
## Author: Corrado Caudek
## Date: 
## ------------------------------------------------------------------


library("here")
library("tidyverse")
library("readxl")
library("stringi")


# d <- rio::import(
#   here("data", "raw", "raven", "controls", "data_controls.xlsx")
# )
d <- rio::import(
  here("data", "raw", "raven", "patients", "data_patients.xlsx")
)

d$mese_c <- ifelse(
  d$`mese:1` < 10, 
  stri_join("0", as.character(d$`mese:1`), sep=''), 
  as.character(d$`mese:1`)
)

d$giorno_c <- ifelse(
  d$`giorno:1` < 10, 
  stri_join("0", as.character(d$`giorno:1`), sep=''), 
  as.character(d$`giorno:1`)
)

d$cellulare_c <- ifelse(
  d$`cellulare:1` < 100, 
  stri_join("0", as.character(d$`cellulare:1`), sep=''), 
  as.character(d$`cellulare:1`)
)

d$sex <- ifelse(d$`sesso:1` == 1, "f",
                ifelse(d$`sesso:1` == 2, "m", NA))

d$subj_id <- tolower(
  stri_join(d$`nome:1`, d$`cognome:1`, d$`anno:1`, 
            d$mese_c, d$giorno_c, d$cellulare_c, d$sex, 
            sep='_')
)

d$code_psytoolkit <- d$`esperimento:1`

bysubj_codes <- d %>% 
  dplyr::select(
    subj_id, code_psytoolkit
  )

# saveRDS(
#   bysubj_codes, 
#   here("data", "processed", "raven", "controls_look_up_table.rds")
#   )
saveRDS(
  bysubj_codes, 
  here("data", "processed", "raven", "patients_look_up_table.rds")
)

# e  n  d  ---------------------------------------------------------------------









