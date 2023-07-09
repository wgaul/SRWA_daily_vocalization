#####################
## Analyse Saipan Reed Warbler daily vocalization patterns
## 
## This script organizes the workflow
## 
## author: Willson Gaul  willson.gaul@gmail.com
## created: 23 July 2022
## last modified: 6 March 2023
######################

warning("Set working directory to source file location.")

library(irr)
library(mgcv)
library(tidyverse)
library(GGally)
library(lubridate)
library(hms)

check_student_data <- TRUE # compare students' data for trustworthiness?
if(check_student_data) {
  source("compare_students_data_SRWA.R")
}

## read in and clean data to analyze
source("./clean_data_srwa.R")

## fit models
source("./gam_srwa.R")

## graphs
srwa <- group_by(srwa, point_id)

# vocalization detected or not by time of day
ggplot(data = srwa, aes(x = time_of_day, y = as.numeric(SRWA))) + 
  geom_point() + 
  geom_smooth() +
  facet_wrap(~point_id, ncol = 2) + 
  theme_bw()

sink(file = "session_info.txt", append = FALSE)
print(Sys.time())
print(sessionInfo())
sink()

