#####################
## Analyse Saipan Reed Warbler daily vocalization patterns
## 
## This script organizes the workflow
## 
## author: Willson Gaul  willson.gaul@gmail.com
## created: 23 July 2022
## last modified: 26 Aug 2023
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

on_cloud <- FALSE
check_student_data <- FALSE # compare students' data for trustworthiness?
# fit gams?  Takes time.  If FALSE, will try to load fitted models from saved 
# objects
fit_gam <- FALSE
fit_rf <- TRUE

if(check_student_data) {
  source("compare_students_data_SRWA.R")
}

## read in and clean data to analyze
source("./clean_data_srwa.R")

## fit models
source("./gam_raw_data_srwa.R")
source("./gam_binned_data_srwa.R")
source("./rf_srwa.R")

## evaluate models
source("./evaluate_models_srwa.R")

## graphs, tables, and numbers for text
source("./plots_main_text_srwa.R")


sink(file = "session_info.txt", append = FALSE)
print(Sys.time())
print(sessionInfo())
sink()

