#' ---
#' title: "Bayesian workflow book - Digits"
#' author: "Gelman, Vehtari, Simpson, et al"
#' date: "First version 2020-12-05. Last modified `r format(Sys.Date())`."
#' output:
#'   html_document:
#'     theme: readable
#'     toc: true
#'     toc_depth: 2
#'     toc_float: true
#'     code_download: true
#' ---

#' A workflow for deciding how many digits to report when summarizing
#' the posterior distribution, and how to check how many independent
#' Monte Carlo (MC) draws or dependent Markov chain Monte Carlo (MCMC)
#' draws are needed.
#' 
#' We analyse the trend in summer months average temperature 1952-2013
#' at Kilpisjärvi in northwestern Finnish Lapland (about 69°03'N,
#' 20°50'E).  We fit a simple linear model, and skip here many usual
#' workflow parts like model checking and focus on the question of how
#' to decide how many digits to report.
#' 
#' ## Summary of workflow for how many digits to report
#'
#' 1. Run inference with some default number of iterations
#' 2. Check convergence diagnostics for all parameters
#' 3. Check that ESS is big enough for reliable convergence
#'    diagnostics for all quantities of interest
#' 4. Look at the posterior for quantities of interest and decide how
#'    many significant digits is reasonable taking into account the
#'    posterior uncertainty (using SD, MAD, or tail quantiles)
#' 5. Check that MCSE is small enough for the desired accuracy of
#'    reporting the posterior summaries for the quantities of
#'    interest.
#' 
#'  - If the accuracy is not sufficient, report less digits or run
#'    more iterations.
#'  - Halving MCSE requires quadrupling the number of iterations.
#'  - Different quantities of interest have different MCSE and may
#'    require different number of iterations for the desired accuracy.
#'  - Some quantities of interest may have posterior distribution with
#'    infinite variance, and then the ESS and MCSE are not defined for
#'    the expectation. In such cases use, for example, median instead
#'    of mean and mean absolute deviation (MAD) instead of standard
#'    deviation. ESS and MCSE for (non-extreme) quantiles can be
#'    derived from the (non-extreme) cumulative probabilities that
#'    always have finite mean and variance.
#'  - For rough initial posterior mean estimates with about one
#'    significant digit accuracy, ESS>100 is needed. For about two
#'    significant digit accuracy, ESS>2000 is needed. These can be
#'    used as initial guesses for how many iterations of MCMC to run.
#' 
#' -------------
#' 

#+ setup, include=FALSE
knitr::opts_chunk$set(message=FALSE, error=FALSE, warning=FALSE, comment=NA)
# switch this to TRUE to save figures in separate files
savefigs <- FALSE

#' #### Load packages
library("here")
library("tidyr") 
library("dplyr") 
library("cmdstanr")
library("posterior")
options(pillar.negative = FALSE)
library("lemon")
library("ggplot2")
library("bayesplot")
theme_set(bayesplot::theme_default(base_family = "sans"))
SEED <- 48927 # set random seed for reproducibility

#' -------------
#'
#' ## Kilpisjärvi data and model
#'
#' ### Data
#' 
#' Load Kilpisjärvi summer month average temperatures 1952-2013:

hist(rnorm(100))

#' questo è un grafico.