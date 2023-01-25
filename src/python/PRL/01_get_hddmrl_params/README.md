# Eating disorders project

1. The script `00_model_comparison.ipynb` is used to compute the DIC statistic in order to select
the correct model for analyzing the PRL data. The DIC values are saved in the script. They must be
recovered and presented in a table.

2. To run the hddm function in python, use as input the csv file

~/_repositories/ed_montecatini/data/processed/prl/input_for_hddmrl/hddm_input_v3.csv

There are 302 subjects.

3. The notebook to run the analysis is `01_hddmrl_stim_grp_303.ipynb`

The csv files with the posterior estimates of the parameters, which will be analyzed by R, are saved here:

```
grptrcs.to_csv('ddm/ddm_grptrcs.csv')
alltrcs.to_csv('ddm/ddm_alltrcs.csv')
```

4. The script `for_colab/hddmrl_montecatini_20220715.ipynb` has been written to run on Colab.  
Note that it must be changed to include the changes made in the correct script in `01_hddmrl_stim_grp_303.ipynb`!!!






