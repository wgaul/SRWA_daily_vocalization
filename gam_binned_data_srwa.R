#####################
## Fit GAMs for Saipan Reed Warbler daily vocalization patterns
## 
## TODO: 
## 
## author: Willson Gaul  willson.gaul@gmail.com  & Ellie Roark
## created: 28 Oct 2022
## last modified: 21 Oct 2023
######################

# references:
# https://stats.stackexchange.com/questions/552880/by-group-random-effect-gam
# Zuur 2009 (pdf)
#     can cite Zuur for warnings about p-values and AIC being approximate 
#     (ch 12 or 13 I think)

### plot raw data
ggplot(data = srwa, aes(x = time_of_day, y = as.numeric(SRWA), 
                        color = season)) + 
  geom_smooth() + 
  geom_jitter(height = 0.1, alpha = 0.2) + 
  facet_wrap(~point_id, ncol = 3)

## plot binned data
ggplot(data = srwa_bin, aes(x = hour_of_day, y = as.numeric(prop_srwa_det))) + 
  geom_smooth() + 
  geom_jitter(height = 0.05, alpha = 0.2) + 
  facet_wrap(~point_id, ncol = 3) + 
  ggtitle("Proportion of minutes with detection")

ggplot(data = srwa_bin, aes(x = hour_of_day, y = n_det)) + 
  geom_smooth() + 
  geom_jitter(height = 0.05, alpha = 0.2) + 
  facet_wrap(~point_id, ncol = 3) + 
  ggtitle("Number of detections per hour")

ggplot(data = srwa_bin, aes(x = hour_of_day, 
                            y = as.numeric(SRWA_in_hour))) + 
  geom_smooth() + 
  geom_jitter(height = 0.05, alpha = 0.2) +
  facet_wrap(~point_id, ncol = 3) + 
  ggtitle("SRWA detected in hour")



#### model to use --------------------------------------------------
### model T/F detection of SRWA in each hour 
### with fixed effects for hour of day and observer
## random effect for location
if(fit_gam) {
  m_b04 <- gam(SRWA_in_hour ~ 1 + s(hour_of_day, k = 10, bs = "cc") + 
                 s(point_id, k = 11, bs = "re") +
                 observer, 
               data = srwa_bin, method = "REML", 
               family = binomial(), 
               knots = list(hour_of_day=c(0, 23)))
  write_rds(m_b04, "./saved_objects/m_b04.rds")
} else m_b04 <- read_rds("./saved_objects/m_b04.rds")

try(gam.check(m_b04))
try(plot(m_b04, pages = 1, all.terms = T, se = T))


## standardized predictions
standard_dat_binned$m_b04 <- predict(m_b04,
                                     newdata = standard_dat_binned, 
                                     type = "response")

#### end model to use --------------------------------





#### models that I tested but will not use
#### None of these have better residual situation
#### All are worse in some way (AIC, ease of understanding)
#### All show the same general conclusions

### model number of detections with fixed effects for hour of day and observer
## random effect for location
if(fit_gam) {
  m_b01 <- gam(n_det ~ 1 + s(hour_of_day, k = 10, bs = "cc") + 
                s(point_id, k = 11, bs = "re") +
                observer, 
              data = srwa_bin, method = "REML", 
              family = poisson(link = "log"), 
              knots = list(hour_of_day=c(0, 23)))
  write_rds(m_b01, "./saved_objects/m_b01.rds")
} else m_b01 <- read_rds("./saved_objects/m_b01.rds")

try(gam.check(m_b01))
try(plot(m_b01, pages = 1, all.terms = T))


### model number of detections with fixed effects for hour of day and observer
## random effect for location
if(fit_gam) {
  m_b02 <- gam(n_det ~ 1 + s(hour_of_day, k = 10, bs = "cc") + 
                 s(hour_of_day, point_id, k = 20, bs = "fs", 
                   m = 2) + 
                 observer, 
               data = srwa_bin, method = "REML", 
               family = poisson(link = "log"), 
               knots = list(hour_of_day=c(0, 23)))
  write_rds(m_b02, "./saved_objects/m_b02.rds")
} else m_b02 <- read_rds("./saved_objects/m_b02.rds")

try(gam.check(m_b02))
try(plot(m_b02, pages = 1, all.terms = T))


### model number of detections with fixed effects for hour of day and observer
## random effect for location
## quasi-poisson
if(fit_gam) {
  m_b03 <- gam(n_det ~ 1 + s(hour_of_day, k = 10, bs = "cc") + 
                 s(point_id, k = 11, bs = "re") +
                 observer, 
               data = srwa_bin, method = "REML", 
               family = quasipoisson(link = "log"), 
               knots = list(hour_of_day=c(0, 23)))
  write_rds(m_b03, "./saved_objects/m_b03.rds")
} else m_b03 <- read_rds("./saved_objects/m_b03.rds")

try(gam.check(m_b03))
try(plot(m_b03, pages = 1, all.terms = T))




### model T/F detection of SRWA in each hour 
### with fixed effects for hour of day and observer
## random effect for location
if(fit_gam) {
  m_b14 <- gam(SRWA_in_hour ~ 1 + s(hour_of_day, k = 10, bs = "cc") + 
                 s(hour_of_day, point_id, k = 20, bs = "fs", 
                   m = 2) +
                 observer, 
               data = srwa_bin, method = "REML", 
               family = binomial(), 
               knots = list(hour_of_day=c(0, 23)))
  write_rds(m_b14, "./saved_objects/m_b14.rds")
} else m_b14 <- read_rds("./saved_objects/m_b14.rds")

try(gam.check(m_b14))
try(plot(m_b14, pages = 1, all.terms = T))


########--------------
### model proportion of minutes with SRWA in each hour 
### with fixed effects for hour of day and observer
## random effect for location
if(fit_gam) {
  m_b05 <- gam(prop_srwa_det ~ 1 + s(hour_of_day, k = 10, bs = "cc") + 
                 s(point_id, k = 11, bs = "re") +
                 observer, 
               data = srwa_bin, method = "REML", 
               family = quasibinomial(), 
               weights = n_min_in_group,
               knots = list(hour_of_day=c(0, 23)))
  write_rds(m_b05, "./saved_objects/m_b05.rds")
} else m_b05 <- read_rds("./saved_objects/m_b05.rds")

try(gam.check(m_b05))
try(plot(m_b05, pages = 1, all.terms = T))

### model proportion of minutes with SRWA in each hour 
### with fixed effects for hour of day and observer
## random effect for location
if(fit_gam) {
  m_b06 <- gam(prop_srwa_det ~ 1 + s(hour_of_day, k = 10, bs = "cc") + 
                 s(hour_of_day, point_id, k = 20, bs = "fs", 
                   m = 2) + 
                 observer, 
               data = srwa_bin, method = "REML", 
               family = quasibinomial(), 
               knots = list(hour_of_day=c(0, 23)))
  write_rds(m_b06, "./saved_objects/m_b06.rds")
} else m_b06 <- read_rds("./saved_objects/m_b06.rds")

try(gam.check(m_b06))
try(plot(m_b06, pages = 1, all.terms = T))
