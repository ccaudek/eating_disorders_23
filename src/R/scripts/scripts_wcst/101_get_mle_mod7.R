
two_groups <- c("patients", "controls")

for (j in seq_along(two_groups)) {
  
  GROUP <- two_groups[j]
  
  if (GROUP == "patients") {
    df = readRDS(
      here::here("data", "processed", "wcst", "input_stan_patients.RDS")
    )
  } else {
    df = readRDS(
      here::here("data", "processed", "wcst", "input_stan_controls.RDS")
    )
  }
  
  n_subj <- length(unique(df$subj_name))
  n_subj
  
  # Assign numeric id to each subject.
  df$id <- as.numeric(factor(as.character(df$subj_name)))
  # sort(unique(df$id))
  
  res_list <- list()
  
  for (i in 1:n_subj) {
    
    one_subj  <- df %>% 
      dplyr::filter(id == i)
    
    new <- rbind(one_subj, one_subj, one_subj)
    
    data_list <- get_one_subj_data_for_stan(new)
    data_list$T <- 180
    data_list$Tsubj <- 180
    
    # Obtain a posterior mode (penalized maximum likelihood) estimate.
    fit_mle <- mod$optimize(data = data_list, seed = 123)
    res_mle <- fit_mle$summary(params_mod7) 
    res_mle$subj_name <- unique(one_subj$subj_name)
    
    res_list[[i]] <- res_mle
  }
  
  # convert list into data.frame
  params_df <- do.call(rbind.data.frame, res_list)
  
  # Convert in wide format
  param_list <- unique(params_df$variable)
  param_list
  
  params_wide <- params_df %>%
    pivot_wider(names_from = variable, values_from = estimate)
  
  if (GROUP == "patients") {
    params_wide$is_patient <- 1
    params_patients <- params_wide
  } else {
    params_wide$is_patient <- 0
    params_controls <- params_wide
  }
  
  print(j)
}



# Manage outliers

replace_outliers_with_na <- function(params_patients, column) {
  
  outliers_list = performance::check_outliers(
    params_patients[[column]],
    method = c("iqr")
  )
  outliers_info = as.data.frame(outliers_list)
  params_patients[[column]] = ifelse(
    outliers_info$Outlier == 1, NA, params_patients[[column]]
  )
  params_patients[[column]]
}

params_patients$mu_MB_Arew  <- replace_outliers_with_na(params_patients, "mu_MB_Arew")
params_patients$mu_MB_Apun  <- replace_outliers_with_na(params_patients, "mu_MB_Apun")
params_patients$mu_MB_gamma <- replace_outliers_with_na(params_patients, "mu_MB_gamma")
params_patients$mu_MF_Arew  <- replace_outliers_with_na(params_patients, "mu_MF_Arew")
params_patients$mu_MF_Apun  <- replace_outliers_with_na(params_patients, "mu_MF_Apun")
params_patients$mu_MF_gamma <- replace_outliers_with_na(params_patients, "mu_MF_gamma")
params_patients$mu_temp     <- replace_outliers_with_na(params_patients, "mu_temp")
params_patients$mu_w        <- replace_outliers_with_na(params_patients, "mu_w")

params_controls$mu_MB_Arew  <- replace_outliers_with_na(params_controls, "mu_MB_Arew")
params_controls$mu_MB_Apun  <- replace_outliers_with_na(params_controls, "mu_MB_Apun")
params_controls$mu_MB_gamma <- replace_outliers_with_na(params_controls, "mu_MB_gamma")
params_controls$mu_MF_Arew  <- replace_outliers_with_na(params_controls, "mu_MF_Arew")
params_controls$mu_MF_Apun  <- replace_outliers_with_na(params_controls, "mu_MF_Apun")
params_controls$mu_MF_gamma <- replace_outliers_with_na(params_controls, "mu_MF_gamma")
params_controls$mu_temp     <- replace_outliers_with_na(params_controls, "mu_temp")
params_controls$mu_w        <- replace_outliers_with_na(params_controls, "mu_w")

all_params <- rbind(params_patients, params_controls)

all_params$is_patient <- factor(all_params$is_patient)
all_params$subj_name <- factor(all_params$subj_name)

all_params_imp <- missRanger(all_params, num.trees = 100)



p1 <- all_params_imp %>% ggplot(aes(x=is_patient, y=mu_MB_Arew, fill=is_patient)) +
  geom_violin(width=1.4) +
  geom_boxplot(width=0.1, color="grey", alpha=0.2) +
  scale_fill_viridis(discrete = TRUE) +
  theme(
    legend.position="none",
    plot.title = element_text(size=11)
  ) +
  ggtitle("MB Arew") +
  ylab("")


p2 <- all_params_imp %>% ggplot(aes(x=is_patient, y=mu_MB_Apun, fill=is_patient)) +
  geom_violin(width=1.4) +
  geom_boxplot(width=0.1, color="grey", alpha=0.2) +
  scale_fill_viridis(discrete = TRUE) +
  theme(
    legend.position="none",
    plot.title = element_text(size=11)
  ) +
  ggtitle("MB Apun") +
  ylab("")

p3 <- all_params_imp %>% ggplot(aes(x=is_patient, y=mu_MB_gamma, fill=is_patient)) +
  geom_violin(width=1.4) +
  geom_boxplot(width=0.1, color="grey", alpha=0.2) +
  scale_fill_viridis(discrete = TRUE) +
  theme(
    legend.position="none",
    plot.title = element_text(size=11)
  ) +
  ggtitle("MB gamma") +
  ylab("")

p4 <- all_params_imp %>% ggplot(aes(x=is_patient, y=mu_MF_Arew, fill=is_patient)) +
  geom_violin(width=1.4) +
  geom_boxplot(width=0.1, color="grey", alpha=0.2) +
  scale_fill_viridis(discrete = TRUE) +
  theme(
    legend.position="none",
    plot.title = element_text(size=11)
  ) +
  ggtitle("MF Arew") +
  ylab("")

p5 <- all_params_imp %>% ggplot(aes(x=is_patient, y=mu_MF_Apun, fill=is_patient)) +
  geom_violin(width=1.4) +
  geom_boxplot(width=0.1, color="grey", alpha=0.2) +
  scale_fill_viridis(discrete = TRUE) +
  theme(
    legend.position="none",
    plot.title = element_text(size=11)
  ) +
  ggtitle("MF Apun") +
  ylab("")

p6 <- all_params_imp %>% ggplot(aes(x=is_patient, y=mu_MF_gamma, fill=is_patient)) +
  geom_violin(width=1.4) +
  geom_boxplot(width=0.1, color="grey", alpha=0.2) +
  scale_fill_viridis(discrete = TRUE) +
  theme(
    legend.position="none",
    plot.title = element_text(size=11)
  ) +
  ggtitle("MF gamma") +
  ylab("")

p7 <- all_params_imp %>% ggplot(aes(x=is_patient, y=mu_temp, fill=is_patient)) +
  geom_violin(width=1.4) +
  geom_boxplot(width=0.1, color="grey", alpha=0.2) +
  scale_fill_viridis(discrete = TRUE) +
  theme(
    legend.position="none",
    plot.title = element_text(size=11)
  ) +
  ggtitle("Temperature") +
  ylab("")

p8 <- all_params_imp %>% ggplot(aes(x=is_patient, y=mu_w, fill=is_patient)) +
  geom_violin(width=1.4) +
  geom_boxplot(width=0.1, color="grey", alpha=0.2) +
  scale_fill_viridis(discrete = TRUE) +
  theme(
    legend.position="none",
    plot.title = element_text(size=11)
  ) +
  ggtitle("Weighting MB/MF") +
  ylab("")


(p1 | p2 | p3 | p4) /
  (p5 | p6 | p7 | p8)

ggsave("model7_params.pdf", width = 8, height = 4)


saveRDS(
  all_params_imp,
  here::here("data", "processed", "wcst", "all_params_imp.RDS")
)


