#####################
## Analyse Saipan Reed Warbler daily vocalization patterns
## 
## This script organizes the workflow
## 
## author: Willson Gaul  willson.gaul@gmail.com
## created: 23 July 2022
## last modified: 30 Jan 2024
######################

warning("Set working directory to source file location.")

library(irr)
library(mgcv)
library(randomForest)
library(tidyverse)
library(GGally)
library(lubridate)
library(hms)

if(!dir.exists("./saved_objects")) dir.create("./saved_objects")

# which device is the script being run on?
data_in_wd <- TRUE # use this if data .csv files are stored in the same directory as the R scripts.
on_cloud <- FALSE
on_wglaptop <- FALSE

check_student_data <- TRUE # compare students' data for trustworthiness?
fit_rf <- TRUE # run the rf_srwaw.R script?
# fit rf models?  If false, model results will be loaded from saved objects
refit_rf_models <- TRUE 
# fit exploratory gams?  Takes time.  If FALSE, will try to load fitted models 
# from saved objects
fit_gam <- FALSE


## read in and clean data to analyze
source("./clean_data_srwa.R")

if(check_student_data) {
  source("compare_students_data_SRWA.R")
}

## fit models
if(fit_gam) source("./gam_raw_data_srwa.R")
if(fit_gam) source("./gam_binned_data_srwa.R")
if(fit_rf) source("./rf_srwa.R")

## evaluate models
source("./evaluate_models_srwa.R")
eval_df_ci

## graphs, tables, and numbers for text
source("./plots_main_text_srwa.R")


sink(file = "session_info.txt", append = FALSE)
print(Sys.time())
print(sessionInfo())
sink()

