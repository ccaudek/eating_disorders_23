This folder includes the R scripts used for analyzing the csv files that contain the posterior estimates of the parameters of the hddmrl model.

1. The script `01_get_traces.R` loads the ddm raw traces produced by `01_hddmrl_stim_grp_303.ipynb`, generates a vector of values for each parameter, and save the results in the file `here::here("data", "processed", "prl", "output_hddm", "traces.Rda")`.

2. The script `02_ddm_inference.R` reads the data `here::here("data", "processed", "prl", "output_hddm", "traces.Rda")` and generates the figures for the paper.

Wed Nov 23 10:23:47 2022
