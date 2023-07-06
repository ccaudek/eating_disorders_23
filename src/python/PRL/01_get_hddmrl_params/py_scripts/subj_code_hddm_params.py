# %%
"""

EATING DISORDERS MONTECATINI

Purpose: Obtain the posterior means for the parameters of the HDDMrl model 
         for each participant using "ddm/traces.csv". In the present case, 
         the data of model M8 are used. 
         Obtain subj_code from the file used as input to hddm.
         Create a file with both subj_code and the posterior means of the 
         HDDMrl parameters.

Written by Corrado Caudek (2023-07-06)

"""

# %%

import os, time, csv, sys
import glob

import numpy as np
import pandas as pd
import xarray as xr
import matplotlib.pyplot as plt
import seaborn as sns
from statsmodels.distributions.empirical_distribution import ECDF
from scipy import stats

import hddm

import pymc
import pymc as pm
import arviz as az

# Data management
pd.options.display.max_colwidth = 100


# %%
data = pd.read_csv("ddm/traces.csv")
data = data.drop(data.columns[0], axis=1)

# %%
mean_values = data.mean()

# Create a new DataFrame 'df' with column names and mean values
df = pd.DataFrame({"params": mean_values.index, "val": mean_values.values})
df["params"] = df["params"].astype(str)  # Convert the column to string type
# Filter rows where the first column contains the string 'subj'
df = df[df["params"].str.contains("subj")]
df["params"] = df["params"].astype(str)  # Convert the 'params' column to string type
df["subj_idx"] = df["params"].apply(lambda x: x.split(".")[-1])
df["par"] = df["params"].str.split("_sub").str[0]
df["grpstim"] = df["params"].str.extract(r"\((.*?)\)\.")
df[["diag_cat", "stim"]] = df["grpstim"].str.split(".", expand=True)
df = df.drop("grpstim", axis=1)
df["subj_idx"] = pd.to_numeric(df["subj_idx"])

# %%
working_directory = "/Users/corrado/_repositories/eating_disorders_23/"

codes_tbl_path = os.path.join(
    working_directory,
    "data",
    "processed",
    "prl",
    "input_for_hddmrl",
    "three_groups",
    "ed_prl_data.csv",
)

codes_tbl = pd.read_csv(codes_tbl_path)
codes_tbl = codes_tbl[["subj_idx", "subj_code"]]
codes_tbl = codes_tbl.drop_duplicates()
codes_tbl["subj_idx"] = pd.to_numeric(codes_tbl["subj_idx"])

# %%
merged_df = pd.merge(df, codes_tbl, on="subj_idx")
merged_df.shape

# %%
merged_df.head()

# %%
mean_df = merged_df.groupby(["par", "diag_cat", "stim"])["val"].mean().reset_index()

# %%
saved_file_path = os.path.join(
    working_directory,
    "data",
    "processed",
    "prl",
    "subj_hddm_params",
    "subj_code_hddm_params.csv",
)

merged_df.to_csv(
    saved_file_path,
    index=False,
)
