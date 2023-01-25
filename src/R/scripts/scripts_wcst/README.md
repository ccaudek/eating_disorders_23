

1. Create a data.frame for each group by using the script 30_recoding_for_stan.R.
By changing the parameter GROUP you decide which group to consider.
The script produces a data.frame called df.

2. Once the df data.frame is created, use the script 100_get_mle_mod7.R.
With this function you can estimate the parameters of Model 7 of Steinke, Lange 
& Kopp (2020) -- their best model. The others that they present do not fit our
data.

With the function get_one_subj_data_for_stan() you can generate the `data` object
which is required for the Stan fit. 

From df we get the data of one subject at the time. Passing these data to 
get_one_subj_data_for_stan() we get the list object required for Stan.

Then we get the posterior mode (penalized maximum likelihood) estimate for the
parameters of Model 7.


3. Convert the list containing the estimates of each subject into the data.frame
params_df. 

4. Obtain the params_df data.frame for each group. They will be called: 

```
param_patients <- params_df
param_controls <- params_df
```

5. Create the data.frame dt with the following columns:

```
 mu_MB_Arew is_patient mu_MB_Apun mu_MB_gamma mu_MF_Arew mu_MF_Apun mu_MF_gamma mu_temp  mu_w
```

6. 

