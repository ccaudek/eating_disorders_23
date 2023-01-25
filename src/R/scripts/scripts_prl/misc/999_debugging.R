

source(here("lib", "ed_fnc.R"))

library("here")
library("tidyverse")


dd <- readRDS(
  here("data", "processed", "prl", "complete_raw_data", "tot_raw_data_prl.rds")
)

foo <- dd %>% 
  group_by(subj_name) %>% 
  summarise(
    n = n()
  )
table(foo$n)


dd$fdbk <- ifelse(dd$feedback == 1, 1, ifelse(dd$feedback == 2, 0, NA))
dd$left_resp <- ifelse(dd$resp == "sx", 1, 0)

foo <- dd %>% 
  group_by(subj_name) %>% 
  summarise(
    # n = n(),
    key = mean(keypress, na.rm = TRUE), 
    target_chosen = mean(is_target_img_chosen, na.rm = TRUE), 
    mfd = mean(fdbk, na.rm = TRUE),
    lresp = mean(left_resp, na.rm = TRUE),
    medrt = median(rt, na.rm = TRUE),
    madrt = mad(rt, na.rm = TRUE)
  )

library("tidyLPA")

foo %>%
  select(-subj_name) %>% 
  scale() %>%
  estimate_profiles(6, package = "MplusAutomation") %>% 
  plot_profiles()
  
m6 <- foo %>%
  select(-subj_name) %>% 
  scale() %>%
  estimate_profiles(6, package = "MplusAutomation") 

out <- get_data(m6)

foo$class <- out$Class

foo %>% 
  group_by(class) %>% 
  summarise(
    key = mean(key, na.rm = TRUE), 
    target_chosen = mean(target_chosen, na.rm = TRUE), 
    mfd = mean(mfd, na.rm = TRUE),
    lresp = mean(lresp, na.rm = TRUE),
    medrt = median(medrt, na.rm = TRUE),
    madrt = mad(madrt, na.rm = TRUE),
    n = n()
  )

good_data <- foo %>% 
  dplyr::filter(class == 4 | class == 2)

good_data$subj_name <- factor(good_data$subj_name)

good_ids <- unique(good_data$subj_name)


hist(foo$key)


temp <- dd %>% 
  dplyr::filter(subj_name == "al_ro_1989_04_25_160_f")

dim(temp)
temp$rt

foo <- raw_data %>% 
  group_by(subj_name) %>% 
  summarise(
    n = n()
  )

table(foo$n)

foo1 <- foo[foo$n == 480, ]
unique(foo1$subj_name)
# al_le_2001_10_19_705_m ma_pr_2000_09_18_430_f sa_li_2001_12_08_953_f

dd1 <- readRDS(
  here("data", "processed", "prl", "complete_raw_data", "tot_raw_data_prl.rds")
)

temp <- raw_data[raw_data$subj_name == "sa_li_2001_12_08_953_f", ]
temp$rt
