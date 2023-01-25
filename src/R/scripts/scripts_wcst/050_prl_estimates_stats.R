


controls <- readRDS(
  here("data", "processed", "wcst", "res_mle_controls.rds")
)
controls$is_patient <- 0
controls$id <- as.numeric(controls$id)


patients <- readRDS(
  here("data", "processed", "wcst", "res_mle_patients.rds")
)
patients$is_patient <- 1
patients$id <- as.numeric(patients$id)
patients$id <- patients$id + 100


d <- rbind(controls, patients)


t.test(MB_Arew ~ is_patient, d)
t.test(MB_Apun ~ is_patient, d)
t.test(MF_Arew ~ is_patient, d)
t.test(MF_Apun ~ is_patient, d)
t.test(temp ~ is_patient, d)


