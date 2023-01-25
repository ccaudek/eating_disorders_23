#' ---
#' title: "Eating disorders Montecatini"
#' subtitle: "PRL parameters for patients/controls classification"
#' author: "Corrado Caudek"
#' date: "First version 2022-06-08. Last modified `r format(Sys.Date())`."
#' output:
#'   html_document:
#'     theme: readable
#'     toc: true
#'     toc_depth: 2
#'     toc_float: true
#'     code_download: true
#' ---

#' ## Purpose
#' 
#' A script to classify patients/controls by using the hDDM parameters. Only
#' the four alpha parameters are used, so as to have the same number of 
#' 'features' as in the task-switching experiment.
#' 
#' Notes: 
#' 
#' When the individual parameters of hDDMrl are computed without
#' the knowledge of the group (NO split_by = is_patient), there is
#' no shrinkage that makes more similar to each other the participants'
#' individual parameters of each group. In these conditions, by using
#' the parameters computed in the food condition, no ability for clinical 
#' classification emerges: AUC = 0.55.
#' 
#' To generate the html, run:
#' `rmarkdown::render("scripts/R/scripts_prl/02_hddmrl_params_analyses/classification_tutorial.R")`
#' 
#' ## Prelims

suppressPackageStartupMessages({
  library("rprojroot")
  root <- has_file("eating_disorders_montecatini.Rproj")$make_fix_file()
  library("tidyverse")
  library("tidyr") 
  library("cmdstanr")
  library("lemon")
  library("bayesplot")
  theme_set(bayesplot::theme_default(base_family = "sans"))
  library("pROC")
  library("tidymodels")  
  library("readr")       
  library("vip") 
})


# Increase max print
options(max.print = .Machine$integer.max)

cores <- parallel::detectCores()
cores

source(root("scripts/functions", "funs_param_analyses.R"))
source(root("scripts/functions", "funs_param_classification_analyses.R"))


#' ## Get parameter estimates

# prl_params <- rio::import(
#   here::here(
#     "data", "processed", "prl", "output_hddm", "hddmrl_output_clean2.txt"
#   )
# )

prl_params <- 
  rio::import(
    root("scripts/python/PRL/classification", 
         "output_hddmrl_dependson_stim_clean.txt")
  )

# Get subjects info.
subj_info <- rio::import(
  root("scripts/python/PRL/classification", "hddm_input_20220603v2.csv")
  ) %>% 
  group_by(
    subj_idx, subj_code, diag_cat
    ) %>% 
  summarise(
    mrt = mean(rt, na.rm = TRUE)
  ) %>% 
  dplyr::select(-mrt) %>% 
  ungroup()

prl_params <- prl_params %>% 
  dplyr::select(
    param_prl, stim, subj_idx, value_prl
  )
prl_params %>% head()

# Add diagnostic category.
d <- left_join(prl_params, subj_info, by = "subj_idx")



two_groups <- d[d$diag_cat %in% c("AN", "HC"), ]
two_groups$diag_cat <- factor(two_groups$diag_cat)

dat <- two_groups %>%
  pivot_wider(
    names_from = c(param_prl),
    values_from = value_prl
  )
# dat %>% head()

dat1 <- dat %>% 
  pivot_wider(
    names_from = stim, 
    values_from = c(a, v, t, alpha, pos_alpha)
  )


dat_hc <- dat1 %>% 
  dplyr::filter(diag_cat == "HC")
dat_hc$is_patient <- 0

dat_an <- dat1 %>% 
  dplyr::filter(diag_cat == "AN")
dat_an$is_patient <- 1
# length(unique(dat_an$subj_idx))

summary(dat_an)


#' ## Start the loop here!


  
  # Select random 31 subjects from the HC group
  subj_idx_hc <- unique(dat_hc$subj_idx)
  chosen_subjects <- sample(subj_idx_hc, 38)
  dat31_hc <- dat_hc[dat_hc$subj_idx %in% chosen_subjects, ]
  
  # Select random 31 subjects from the AN group
  subj_idx_an <- unique(dat_an$subj_idx)
  chosen_subjects <- sample(subj_idx_an, 38)
  dat31_an <- dat_an[dat_an$subj_idx %in% chosen_subjects, ]
  
  two_groups31_df <- rbind(dat31_an, dat31_hc)
  
  
  dat <- two_groups31_df %>% 
    dplyr::select(
      is_patient, 
      alpha_food, pos_alpha_food, 
      alpha_neutral, pos_alpha_neutral, 
      a_food, a_neutral, v_food, v_neutral, t_food, t_neutral 
    )
  
  dat$is_patient <- ifelse(
    dat$is_patient == 1, "patient", "control"
  )
  
  dat <- dat %>%  
    mutate_if(is.character, as.factor)
  
  summary(dat)
  
  #' ## Follow the tutorial 
  #' 
  #' The tutorial is provided in the following blog post: 
  #' [Tidymodels: tidy machine learning in R](https://www.rebeccabarter.com/blog/2020-03-25_machine_learning/)
  #' 
  #' First, let’s split our dataset into training and testing data. The training 
  #' data will be used to fit our model and tune its parameters, where the testing 
  #' data will be used to evaluate our final model’s performance.
  #' This split can be done automatically using the inital_split() function 
  #' (from rsample) which creates a special “split” object.
  #' 
  # split the data into trainng (60%) and testing (40%)
  diabetes_split <- initial_split(dat, prop = 0.6)
  diabetes_split
  
  #' The training and testing sets can be extracted from the “split” object using 
  #' the training() and testing() functions. Although, we won’t actually use these 
  #' objects in the pipeline (we will be using the diabetes_split object itself).
  #' 
  # extract training and testing sets
  diabetes_train <- training(diabetes_split)
  diabetes_test <- testing(diabetes_split)
  
  #' At some point we’re going to want to do some parameter tuning, and to do that 
  #' we’re going to want to use cross-validation. So we can create a 
  #' cross-validated version of the training set in preparation for that moment 
  #' using vfold_cv().
  #' 
  # create CV object from training data
  diabetes_cv <- vfold_cv(diabetes_train)
  
  #' ## Define a recipe
  #' 
  #' Recipes allow you to specify the role of each variable as an outcome or 
  #' predictor variable (using a “formula”), and any pre-processing steps you want 
  #' to conduct.
  #' 
  # define the recipe
  diabetes_recipe <- 
  # which consists of the formula (outcome ~ predictors)
  recipe(
    is_patient ~ ., 
    data = dat
    ) %>%
  # and some pre-processing steps
    step_normalize(all_numeric()) %>%
    step_knnimpute(all_predictors())
  
  diabetes_recipe
  
  #' If you want to extract the pre-processed dataset itself, you can first 
  #' prep() the recipe for a specific dataset and juice() the prepped recipe to 
  #' extract the pre-processed data. It turns out that extracting the 
  #' pre-processed data isn’t actually necessary for the pipeline, since this will 
  #' be done under the hood when the model is fit, but sometimes it’s useful 
  #' anyway.
  #' 
  diabetes_train_preprocessed <- diabetes_recipe %>%
    # apply the recipe to the training data
    prep(diabetes_train) %>%
    # extract the pre-processed training dataset
    juice()
  diabetes_train_preprocessed
  
  #' ## Specify the model
  #' 
  #' The next thing we want to specify is our model (using the parsnip package).
  #' Parsnip offers a unified interface for the massive variety of models that 
  #' exist in R. 
  #' 
  #' We want to fit a random forest model as implemented by the ranger package for 
  #' the purpose of classification and we want to tune the mtry parameter (the 
  #' number of randomly selected variables to be considered at each split in the 
  #' trees).
  #' 
  rf_model <- 
    # specify that the model is a random forest
    rand_forest() %>%
    # specify that the `mtry` parameter needs to be tuned
    set_args(mtry = tune()) %>%
    # select the engine/package that underlies the model
    set_engine("ranger", importance = "impurity") %>%
    # choose either the continuous regression or binary classification mode
    set_mode("classification") 
  
  #' Note that this code doesn’t actually fit the model. Like the recipe, it just 
  #' outlines a description of the model. Moreover, setting a parameter to tune() 
  #' means that it will be tuned later in the tune stage of the pipeline (i.e. the 
  #' value of the parameter that yields the best performance will be chosen).
  
  #' The following code instead specifies a logistic regression model from the 
  #' glm package.
  #' 
  lr_model <-
    # specify that the model is a random forest
    logistic_reg() %>%
    # select the engine/package that underlies the model
    set_engine("glm") %>%
    # choose either the continuous regression or binary classification mode
    set_mode("classification")
  
  
  #' ## Put it all together in a workflow
  #' 
  #' We’re now ready to put the model and recipes together into a workflow. You 
  #' initiate a workflow using workflow() (from the workflows package) and then 
  #' you can add a recipe and add a model to it.
  
  # set the workflow
  rf_workflow <- workflow() %>%
    # add the recipe
    add_recipe(diabetes_recipe) %>%
    # add the model
    add_model(rf_model)
  
  #' Note that we still haven’t yet implemented the pre-processing steps in the 
  #' recipe nor have we fit the model. We’ve just written the framework. It is 
  #' only when we tune the parameters or fit the model that the recipe and model 
  #' frameworks are actually implemented.
  
  #' ## Tune the parameters
  #' 
  #' Since we had a parameter that we designated to be tuned (mtry), we need to 
  #' tune it (i.e. choose the value that leads to the best performance) before 
  #' fitting our model. If you don’t have any parameters to tune, you can skip 
  #' this step.
  #' 
  #' Note that we will do our tuning using the cross-validation object 
  #' (diabetes_cv). To do this, we specify the range of mtry values we want to 
  #' try, and then we add a tuning layer to our workflow using tune_grid() (from 
  #' the tune package). Note that we focus on two metrics: accuracy and roc_auc 
  #' (from the yardstick package).
  
  
  # specify which values eant to try
  rf_grid <- expand.grid(mtry = c(2, 3, 4))
  # extract results
  rf_tune_results <- rf_workflow %>%
    tune_grid(resamples = diabetes_cv, #CV object
              grid = rf_grid, # grid of values to try
              metrics = metric_set(accuracy, roc_auc) # metrics we care about
    )
  
  #' It’s always a good idea to explore the results of the cross-validation. 
  #' collect_metrics() is a really handy function that can be used in a variety of 
  #' circumstances to extract any metrics that have been calculated within the 
  #' object it’s being used on. In this case, the metrics come from the 
  #' cross-validation performance across the different values of the parameters.
  
  # print results
  rf_tune_results %>%
    collect_metrics()
  #' Across both accuracy and AUC, mtry = 4 yields the best performance (just).
  
  #' ## Finalize the workflow
  #' 
  #' We want to add a layer to our workflow that corresponds to the tuned 
  #' parameter, i.e. sets mtry to be the value that yielded the best results. If 
  #' you didn’t tune any parameters, you can skip this step.
  #' 
  #' We can extract the best value for the accuracy metric by applying the 
  #' select_best() function to the tune object.
  
  param_final <- rf_tune_results %>%
    select_best(metric = "accuracy")
  param_final
  
  #' Then we can add this parameter to the workflow using the 
  #' finalize_workflow() function.
  
  rf_workflow <- rf_workflow %>%
    finalize_workflow(param_final)
  
  #' ## Evaluate the model on the test set
  #' 
  #' Now we’ve defined our recipe, our model, and tuned the model’s parameters, 
  #' we’re ready to actually fit the final model. Since all of this information 
  #' is contained within the workflow object, we will apply the last_fit() 
  #' function to our workflow and our train/test split object. This will 
  #' automatically train the model specified by the workflow using the training 
  #' data, and produce evaluations based on the test set.
  #' 
  rf_fit <- rf_workflow %>%
    # fit on the training set and evaluate on test set
    last_fit(diabetes_split)
  
  #' Note that the fit object that is created is a data-frame-like object; 
  #' specifically, it is a tibble with list columns.
  #' 
  rf_fit
  
  #' This is a really nice feature of tidymodels (and is what makes it work so 
  #' nicely with the tidyverse) since you can do all of your tidyverse operations 
  #' to the model object. While truly taking advantage of this flexibility 
  #' requires proficiency with purrr, if you don’t want to deal with purrr and 
  #' list-columns, there are functions that can extract the relevant information 
  #' from the fit object that remove the need for purrr as we will see below.
  #' 
  #' Since we supplied the train/test object when we fit the workflow, the metrics 
  #' are evaluated on the test set. Now when we use the collect_metrics() function 
  #' (recall we used this when tuning our parameters), it extracts the performance 
  #' of the final model (since rf_fit now consists of a single final model) 
  #' applied to the test set.
  #' 
  test_performance <- rf_fit %>% collect_metrics()
  test_performance
  

 



#' ## Generate predictions from the test set
#' 
#' You can also extract the test set predictions themselves using the 
#' collect_predictions() function. Note that there are 192 rows in the 
#' predictions object below which matches the number of test set observations 
#' (just to give you some evidence that these are based on the test set rather 
#' than the training set).
#' 
test_predictions <- rf_fit %>% collect_predictions()
test_predictions

#' Since this is just a normal data frame/tibble object, we can generate 
#' summaries and plots such as a confusion matrix.
#' 
# Generate a confusion matrix
test_predictions %>% 
  conf_mat(truth = is_patient, estimate = .pred_class)

#' We could also plot distributions of the predicted probability distributions 
#' for each class.
#' 
test_predictions %>%
  ggplot() +
  geom_density(
    aes(x = .pred_patient, fill = is_patient), 
    alpha = 0.5
  )

#' If you’re familiar with purrr, you could use purrr functions to extract the 
#' predictions column using pull(). The following code does almost the same 
#' thing as collect_predictions(). You could similarly have done this with the 
#' .metrics column.
#' 
test_predictions <- rf_fit %>% pull(.predictions)
test_predictions

#' ## Fitting and using your final model
#' 
#' The previous section evaluated the model trained on the training data using 
#' the testing data. But once you’ve determined your final model, you often want 
#' to train it on your full dataset and then use it to predict the response for 
#' new data.
#' 
#' If you want to use your model to predict the response for new observations, 
#' you need to use the fit() function on your workflow and the dataset that you 
#' want to fit the final model on (e.g. the complete training + testing dataset).
#' 
final_model <- fit(rf_workflow, dat)

#' The final_model object contains a few things including the ranger object 
#' trained with the parameters established through the workflow contained in 
#' rf_workflow based on the data in diabetes_clean (the combined training and 
#' testing data).
#' 
final_model

#' ## Variable importance
#' 
#' If you want to extract the variable importance scores from your model, as 
#' far as I can tell, for now you need to extract the model object from the 
#' fit() object (which for us is called final_model). The function that extracts 
#' the model is pull_workflow_fit() and then you need to grab the fit object 
#' that the output contains.
#' 
ranger_obj <- extract_fit_parsnip(final_model)$fit
ranger_obj

#' Then you can extract the variable importance from the ranger object itself 
#' (variable.importance is a specific object contained within ranger output - 
#' this will need to be adapted for the specific object type of other models).
#' 
ranger_obj$variable.importance

