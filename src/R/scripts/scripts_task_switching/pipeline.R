# Script name: pipeline.R
# Project: Eating disorders Montecatini
# Script purpose: Run the task-switching analyses.
# @author: Corrado Caudek <corrado.caudek@unifi.it>
# Date Created: Tue Jun  7 22:27:06 2022
# Last Modified Date: Tue Jun  7 22:27:06 2022
#
# 👉 

suppressPackageStartupMessages({
  library("here")
  library("tidyverse")
  library("brms")
  library("tidybayes")        # Manipulate Stan objects in a tidy way
  library("broom")            # Convert model objects to data frames
  library("broom.mixed")      # Convert brms model objects to data frames
  library("ggdist")           # Special geoms for posterior distributions
  library("gghalves")         # Special half geoms
  library("ggbeeswarm")       # Special distribution-shaped point jittering
  library("ggrepel")          # Automatically position labels
  library("patchwork")        # Combine ggplot objects
  library("emmeans")          # Calculate marginal effects in even fancier ways
  library("modelsummary")     # Create side-by-side regression tables
  library("gridExtra")
  library("miceRanger")
  library("pROC")
  library("papaja")
  library("makepipe")
})


# Source functions
source(here::here("src", "R", "functions", "funs_task_switching.R"))



# Obtain raw Task Swithing data and remove bad subjects ------------------------

make_with_recipe(
  note = "Obtain raw Task Swithing data and remove bad subjects",
  recipe = {
    read_and_tidy_switch_data()
  },
  targets = here::here(
    "data", "processed", "task_switching", "data_for_descript_stats",
    "task_switch_data_for_descript_stats_clean.rds"
  ),
  dependencies = here::here(
    "data", "processed", "task_switching", "data_for_descript_stats",
    "task_switch_data_for_descript_stats.rds"
  )
)


# Generate figure with RTs and error rates -------------------------------------

make_with_recipe(
  note = "Obtain task switch performance figure",
  recipe = {
    gen_task_switch_performance_fig()
  },
  targets = here::here(
    "data", "figures", "task_switch_performance.pdf"
  ),
  dependencies = here::here(
    "data", "processed", "task_switching", "data_for_descript_stats", 
    "task_switch_data_for_descript_stats_clean.rds"
  )
)


# Save brm object for accuracy analysis ----------------------------------------

make_with_recipe(
  note = "Save fitted model accuracy",
  recipe = {
    save_fitted_err_rate_model_task_switch()
  },
  targets = here::here(
    "scripts", "R", "scripts_task_switching", "brm_files", "mod_error_rate.rds"
    ),
  dependencies = here::here(
    "data", "processed", "task_switching", "data_for_descript_stats", 
    "task_switch_data_for_descript_stats_clean.rds"
  )
)

# Save brm object for RT analysis ----------------------------------------------

make_with_recipe(
  note = "Save fitted model RTs",
  recipe = {
    save_fitted_rt_model_task_switch()
  },
  targets = here::here(
    "scripts", "R", "scripts_task_switching", "brm_files", "mod_rt.rds"
  ),
  dependencies = here::here(
    "data", "processed", "task_switching", "data_for_descript_stats", 
    "task_switch_data_for_descript_stats_clean.rds"
  )
)


# Display pipeline visualisation -----------------------------------------------
makepipe::show_pipeline()

pipe <- get_pipeline()
pipe$clean()


# End of file -------------------------------------------------------------



if (res$executed) {
  # Check that the `hist_dat` object registered in `data_prep.R` is a table
  stopifnot(is.table(res$result$hist_dat)) 
}


# Read raw switching data
task_switch_df <- read_clean_task_switch_data()



# Histogram --------------------------------------------------------------------
res <- makepipe::make_with_source(
  note = "Calculate a histogram of word lengths",
  source = "data_prep.R",
  targets = "data/histogram.tsv",
  dependencies = "data/words.txt"
)

res # Print execution meta-data 
if (res$executed) {
  # Check that the `hist_dat` object registered in `data_prep.R` is a table
  stopifnot(is.table(res$result$hist_dat)) 
}

# Figure -----------------------------------------------------------------------
makepipe::make_with_source(
  note = "Generate a figure of this histogram",
  source = "data_viz.R",
  targets = "data/histogram.png",
  dependencies = "data/histogram.tsv"
)

if (file.exists("Rplots.pdf")) {
  file.remove("Rplots.pdf") # Clean up unwanted .pdf by-product
}

# Report -----------------------------------------------------------------------
makepipe::make_with_recipe(
  note = "Render a R Markdown report in HTML",
  recipe = rmarkdown::render(
    "report.Rmd",
    output_file = "output/report.html"
  ),
  targets = "output/report.html",
  dependencies = c("report.Rmd", "data/histogram.png")
)

# Display pipeline visualisation -----------------------------------------------
makepipe::show_pipeline()
