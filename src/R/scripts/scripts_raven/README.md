# Raven Matrices

The raw data for the Raven Matrices are in:

data/raw/raven

Remember that ONLY the subject data files must contain the pattern "raven_rds". NO OTHER FILE! Otherwise the scripts will not work.

You need to move the csv files with max, min, median, etc, to the folder 

additional_information

To generate the data.file 

raven_scores_final.rds

you must run the scripts:

10_raven_eds.R
20_raven_create_subj_name.R
30_raven_combine_codes.R
















