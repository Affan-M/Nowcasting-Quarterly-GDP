
# Nowcasting: Controls ----------------------------------------------------

# This script is used to create controls on which scripts to run based on
# which actions we want to take. For example, we may not want to run experiment
# always. So have set experiment to "no" for now.

# Packages
library(tidyverse) # Main data manipulation
library(httr) # Web requests (used for API)
library(jsonlite) # JSON (used for API)
library(glue) # String manipulation
library(zoo) # Date manipulation
library(lubridate) # Date manipulation
library(glue) # Text manipulation
library(readxl) # Read excel files
library(openxlsx) # Write excel files
library(midasr) # MIDAS regression package
library(fpp3) # FPP3: Main regression package
library(nowcasting) # DFM package
library(tseries) # For stationarity tests
library(vars, include.only = c("VARselect")) # VAR lag selection
library(tidymodels) # Broad machine learning package
library(usemodels) # Used to show sample code
library(vip) # Check importance of variables
library(rsample) # Resampling/Cross-validation folds
library(glmnet) # Required to run Lasso, Ridge, ElasticNet
library(ranger) # Required to run random forest within tidymodels
library(kernlab) # Required to run support vector regression within tidymodels
library(xgboost) # Required to run XGBoost within tidymodels
library(stacks) # Ensemble model package
library(parallel) # Parallelization package
library(doParallel) # Parallelization package

forecasts <- "yes"
experiment <- "yes"

if (forecast == "yes" && experiment == "yes") {
  source("1 Data.R")
  source("2 Forecasts.R")
  source("3 Experiment.R")
} else if (forecast == "yes" && experiment == "no") {
  source("1 Data.R")
  source("2 Forecasts.R")
} else if (forecast == "no" && experiment == "yes") {
  source("1 Data.R")
  source("3 Experiment.R")
}
