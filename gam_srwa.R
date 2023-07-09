#####################
## Fit GAMs for Saipan Reed Warbler daily vocalization patterns
## 
## 
## author: Willson Gaul  willson.gaul@gmail.com  & Ellie Roark
## created: 28 Oct 2022
## last modified: 9 July 2023
######################

# references:
# https://stats.stackexchange.com/questions/552880/by-group-random-effect-gam
# Zuur 2009 (pdf)

### plot raw data
ggplot(data = srwa, aes(x = time_of_day, y = as.numeric(SRWA), 
                        color = season)) + 
  geom_smooth() + 
  geom_jitter(height = 0.1, alpha = 0.2) + 
  facet_wrap(~point_id, ncol = 3)

# Does the shape of the smooth need to vary by location?
# YES, Rw04 has an afternoon peak in summer, Rw05 and Rw10 have morning peaks.

# Does the shape of the smooth need to vary by season?
# YES, the shape of the curve differs between seasons for: Rw04, Rw05, Rw06

## My strategy is to build the model that includes all terms I think will be
## relevant, and then to interpret those results (i.e. no removal of variables
## through model selection).  I will do some model selection to determine how 
## the smooths should be incorporated and allowed to vary by group.
## I do not know how to make a three way interaction that would allow the 
## smooth for time of day to vary by sampling period and location, such that 
## different locations have different smooth shapes, and those shapes are 
## different for different sampling periods at each location.


## models with random effect for location and day, 
## fixed effects for other variables
# global smoother plus group-level smoothers that have the same wiggliness (GS)
m_01 <- gam(SRWA ~ 1 + s(time_of_day_sec, k = 20, bs = "cc") + 
              s(time_of_day_sec, point_id, k = 10, bs = "fs", 
                m = 2) + 
              s(date_fac, k = 27, bs = "re") +
              season + 
              observer + rain_wind, 
            data = srwa, method = "REML", 
            family = binomial(), 
            knots = list(time_of_day_sec=c(0, 86400)))
gam.check(m_01)
plot(m_01, pages = 1, all.terms = T)

# global smoother plus group-level smoothers that have different wiggliness (GI)
m_02 <- gam(SRWA ~ 1 + s(time_of_day_sec, k = 20, m = 2, bs = "cc") + 
              s(time_of_day_sec, by = point_id, k = 10, m = 1, bs = "cc") +
              s(date_fac, k = 27, bs = "re") +
              season + 
              observer + rain_wind, 
            data = srwa, method = "REML", 
            family = binomial(), 
            knots = list(time_of_day_sec=c(0, 86400)))
gam.check(m_02)
plot(m_02, pages = 1, all.terms = T)

# global smoother plus group-level smoothers that have the same wiggliness (GS)
# No season fixed effect
m_03 <- gam(SRWA ~ 1 + s(time_of_day_sec, k = 20, bs = "cc") + 
              s(time_of_day_sec, point_id, k = 10, bs = "fs", 
                m = 2) + 
              s(date_fac, k = 27, bs = "re") +
              observer + rain_wind, 
            data = srwa, method = "REML", 
            family = binomial(), 
            knots = list(time_of_day_sec=c(0, 86400)))
gam.check(m_03)
plot(m_03, pages = 1, all.terms = T)


# global smoother plus group-level smoothers that have different wiggliness (GI)
# No season fixed effect
m_04 <- gam(SRWA ~ 1 + s(time_of_day_sec, k = 20, m = 2, bs = "cc") + 
              s(time_of_day_sec, by = point_id, k = 10, m = 1, bs = "cc") +
              s(date_fac, k = 27, bs = "re") +
              observer + rain_wind, 
            data = srwa, method = "REML", 
            family = binomial(), 
            knots = list(time_of_day_sec=c(0, 86400)))
gam.check(m_04)
plot(m_04, pages = 1, all.terms = T)


## Model comparison
aic <- AIC(m_01, m_02, m_03, m_04)
aic[order(aic$AIC), ]

summary(m_03)
summary(m_01)

# remove terms individually
m_01_noDay <- gam(SRWA ~ 1 + s(time_of_day_sec, k = 20, bs = "cc") + 
                    s(time_of_day_sec, point_id, k = 10, bs = "fs", 
                      m = 2) + 
                    season +
                    observer + rain_wind, 
                  data = srwa, method = "REML", 
                  family = binomial(), 
                  knots = list(time_of_day_sec=c(0, 86400)))

m_01_noLocation <- gam(SRWA ~ 1 + s(time_of_day_sec, k = 20, bs = "cc") + 
                         s(date_fac, k = 27, bs = "re") +
                         season +
                         observer + rain_wind, 
                       data = srwa, method = "REML", 
                       family = binomial(), 
                       knots = list(time_of_day_sec=c(0, 86400)))

m_01_noObserver <- gam(SRWA ~ 1 + s(time_of_day_sec, k = 20, bs = "cc") + 
                         s(time_of_day_sec, point_id, k = 10, bs = "fs", 
                           m = 2) + 
                         s(date_fac, k = 27, bs = "re") +
                         season +
                         rain_wind, 
                       data = srwa, method = "REML", 
                       family = binomial(), 
                       knots = list(time_of_day_sec=c(0, 86400)))

m_01_noWeather <- gam(SRWA ~ 1 + s(time_of_day_sec, k = 20, bs = "cc") + 
                        s(time_of_day_sec, point_id, k = 10, bs = "fs", 
                          m = 2) + 
                        s(date_fac, k = 27, bs = "re") +
                        season +
                        observer, 
                      data = srwa, method = "REML", 
                      family = binomial(), 
                      knots = list(time_of_day_sec=c(0, 86400)))

m_01_noSamplingPeriod <- gam(SRWA ~ 1 + s(time_of_day_sec, k = 20, bs = "cc") + 
                               s(time_of_day_sec, point_id, k = 10, bs = "fs", 
                                 m = 2) + 
                               s(date_fac, k = 27, bs = "re") +
                               observer + rain_wind, 
                             data = srwa, method = "REML", 
                             family = binomial(), 
                             knots = list(time_of_day_sec=c(0, 86400)))

aic_m01 <- AIC(m_01, m_01_noDay, m_01_noLocation, m_01_noObserver, 
               m_01_noSamplingPeriod, m_01_noWeather)
aic_m01 <- aic_m01[order(aic_m01$AIC), ]
aic_m01$model <- rownames(aic_m01)
aic_m01 <- aic_m01[, c(3, 1, 2)]
aic_m01$dev_expl <- NA

m_01_list <- list(m_01_noSamplingPeriod = m_01_noSamplingPeriod, 
                 m_01_noWeather = m_01_noWeather, 
                 m_01 = m_01, m_01_noObserver = m_01_noObserver, 
                 m_01_noDay = m_01_noDay, 
                 m_01_noLocation = m_01_noLocation)
for(i in 1:nrow(aic_m01)) {
  aic_m01$dev_expl[i] <- summary(m_01_list[names(m_01_list) == 
                                     aic_m01$model[i]][[1]])$dev.expl
}

aic_m01
#####














## This section follows a smooth selection process that I copied from somewhere,
## but I don't know where.  It would be good to find this again. (9 July 2023)
srwa_gam_H0 <- gam(SRWA ~ 1, 
                   family = binomial(), 
                   data = data.frame(srwa))
gam.check(srwa_gam_H0)


# Group level smoother with random effect letting intercept vary by location (G)
srwa_gamG <- gam(SRWA ~ 1 + s(time_of_day_sec, k = 20, bs = "cc") + 
                   s(point_id, k = 8, bs = "re"), 
                 data = srwa, method = "REML", 
                 family = binomial(), 
                 knots = list(time_of_day_sec=c(0, 86400)))
gam.check(srwa_gamG)
plot(srwa_gamG)
srwa$pred_gamG <- predict(srwa_gamG, type = "response")
ggplot(data = srwa, aes(x = pred_gamG, y = SRWA)) + 
  geom_point() + 
  facet_wrap(~point_id) + 
  ggtitle("Predicted v. observed")

ggplot(data = srwa, aes(x = time_of_day, y = pred_gamG)) + 
  geom_point() + 
  facet_wrap(~point_id) + 
  ggtitle("Predicted v. time of day")


# global smoother plus group-level smoothers that have the same wiggliness (GS)
srwa_gamGS <- gam(SRWA ~ 1 + s(time_of_day_sec, k = 20, bs = "cc") + 
                    s(time_of_day_sec, point_id, k = 10, bs = "fs", m = 2), 
                  data = srwa, method = "REML", 
                  family = binomial(), 
                  knots = list(time_of_day_sec=c(0, 86400)))
gam.check(srwa_gamGS)
plot(srwa_gamGS)
srwa$pred_gamGS <- predict(srwa_gamGS, type = "response")
ggplot(data = srwa, aes(x = pred_gamGS, y = SRWA)) + 
  geom_point() + 
  facet_wrap(~point_id) + 
  ggtitle("Predicted v. observed\nGS")

ggplot(data = srwa, aes(x = time_of_day, y = pred_gamGS)) + 
  geom_point(data = srwa[srwa$SRWA == TRUE, ], 
             aes(x = time_of_day, y = pred_gamGS)) +
  geom_line() + 
  facet_wrap(~point_id, ncol = 2) + 
  ggtitle("Predicted v. time of day\nGS\nDetections")

ggplot(data = srwa, aes(x = time_of_day, y = pred_gamGS)) + 
  geom_point(data = srwa[srwa$SRWA == FALSE, ], 
             aes(x = time_of_day, y = pred_gamGS)) +
  geom_line() + 
  facet_wrap(~point_id, ncol = 2) + 
  ggtitle("Predicted v. time of day\nGS\nNon-detections")


# global smoother plus group-level smoothers that have different wiggliness (GI)
srwa_gamGI <- gam(SRWA ~ 1 + s(time_of_day_sec, k = 20, m = 2, bs = "cc") + 
                    s(time_of_day_sec, by = point_id, k = 10, m = 1, bs = "cc"), 
                  data = srwa, method = "REML", 
                  family = binomial(), 
                  knots = list(time_of_day_sec=c(0, 86400)))
gam.check(srwa_gamGI)
plot(srwa_gamGI)
srwa$pred_gamGI <- predict(srwa_gamGI, type = "response")
ggplot(data = srwa, aes(x = pred_gamGI, y = SRWA)) + 
  geom_point() + 
  facet_wrap(~point_id) + 
  ggtitle("Predicted v. observed\nGI")

ggplot(data = srwa, aes(x = time_of_day, y = pred_gamGI)) + 
  geom_point() + 
  facet_wrap(~point_id, ncol = 2) + 
  ggtitle("Predicted v. time of day\nGI")


## models with random effect for day
# global smoother plus group-level smoothers that have the same wiggliness (GS)
srwa_gamGS_full <- gam(SRWA ~ 1 + s(time_of_day_sec, k = 10, bs = "cc") + 
                          s(time_of_day_sec, point_id, k = 10, bs = "fs", 
                            m = 2) + 
                          s(date_fac, k = 27, bs = "re") +
                          observer + 
                          rain, 
                        data = srwa, method = "REML", 
                        family = binomial(), 
                        knots = list(time_of_day_sec=c(0, 86400)))
gam.check(srwa_gamGS_full)
plot(srwa_gamGS_full)
srwa$pred_gamGS_full <- predict(srwa_gamGS_full, type = "response")
ggplot(data = srwa, aes(x = pred_gamGS_full, y = SRWA)) + 
  geom_point() + 
  facet_wrap(~point_id) + 
  ggtitle("Predicted v. observed\nGS_full")

ggplot(data = srwa, aes(x = time_of_day, y = pred_gamGS_full, color = season)) + 
  geom_point(data = srwa[srwa$SRWA == TRUE, ], 
             aes(x = time_of_day, y = pred_gamGS_full)) +
  geom_line() + 
  facet_wrap(~point_id, ncol = 2) + 
  ggtitle("Predicted v. time of day\nGS_full\nDetections")

## model with random effect for season
# global smoother plus group-level smoothers that have the same wiggliness (GS)
srwa_gamGS_season_full <- gam(
  SRWA ~ 1 + 
    s(time_of_day_sec, by = season, k = 10, bs = "cc") + 
    s(time_of_day_sec, point_id, k = 10, bs = "fs", 
      m = 2) + 
    # season +
    # s(date_fac, k = 27, bs = "re") +
    observer + 
    rain, 
  data = srwa, method = "REML", 
                              family = binomial(), 
                              knots = list(time_of_day_sec=c(0, 86400)))
gam.check(srwa_gamGS_season_full)
plot(srwa_gamGS_season_full)

srwa$pred_gamGS_season_full <- predict(srwa_gamGS_season_full, 
                                       type = "response", 
                                       exclude = "s(date_fac)")
ggplot(data = srwa, aes(x = pred_gamGS_season_full, y = SRWA)) + 
  geom_point() + 
  facet_wrap(~point_id) + 
  ggtitle("Predicted v. observed\nGS_season_full")

ggplot(data = srwa, aes(x = time_of_day, y = pred_gamGS_season_full, 
                        color = season)) + 
  geom_point(data = srwa[srwa$SRWA == TRUE, ], 
             aes(x = time_of_day, y = pred_gamGS_season_full)) +
  geom_line() + 
  facet_wrap(~point_id, ncol = 2) + 
  ggtitle("Predicted v. time of day\nGS_season_full\nDetections")



# model with rain but not day or observer
srwa_gamGS_noise <- gam(SRWA ~ 1 + s(time_of_day_sec, k = 10, bs = "cc") + 
                         s(time_of_day_sec, point_id, k = 10, bs = "fs", 
                           m = 2) + 
                         s(rain, k = 2, bs = "re"), 
                       data = srwa, method = "REML", 
                       family = binomial(), 
                       knots = list(time_of_day_sec=c(0, 86400)))
gam.check(srwa_gamGS_noise)
plot(srwa_gamGS_noise)

##### model comparison
AIC(srwa_gam_H0, srwa_gamG, srwa_gamGS, srwa_gamGI, srwa_gamGS_full, 
    srwa_gamGS_season_full, srwa_gamGS_noise)


summary(srwa_gamGS_full)
summary(srwa_gamGS_season_full)
summary(srwa_gamGS_noise)


summary(srwa_gamGS)
plot(srwa_gamGS, pages = 9, residuals = TRUE)
plot(srwa_gamGS, pages = 5, seWithMean = TRUE)






srwa$resids_GS <- srwa_gamGS$residuals
ggplot(data = srwa, aes(x = time_of_day, y = resids_GS)) + 
  geom_point() + 
  facet_wrap(~point_id) +
  ylim(-5, 20) + 
  ggtitle("GS")
srwa$resids_GI <- srwa_gamGI$residuals
ggplot(data = srwa, aes(x = time_of_day, y = resids_GI)) + 
  geom_point() + 
  facet_wrap(~point_id) +
  ylim(-5, 20) + 
  ggtitle("GI")

ggplot(data = srwa, aes(x = point_id, y = resids_GS)) + 
  geom_boxplot() + 
  ylim(-5, 20)
ggplot(data = srwa, aes(x = point_id, y = resids_GI)) + 
  geom_boxplot() + 
  ylim(-5, 20)



#############
## Standardized predictions
# standard_dat <- data.frame(point_id = NA, time_of_day_sec = NA, season = NA,
#                            observer = NA, date_fac = NA, rain = NA)
# for(i in 1:length(unique(srwa$point_id))) {
#   for(j in 1:length(unique(srwa$season))) {
#     this_dat <- data.frame(point_id = factor(unique(srwa$point_id))[i],
#                            time_of_day_sec = (0:1440)*60,
#                            season = factor(unique(srwa$season))[j])
#     standard_dat <- bind_rows(standard_dat, this_dat)
#   }
# }

standard_dat <- srwa
standard_dat <- standard_dat[, -which(colnames(standard_dat) == "SRWA")]
standard_dat$observer <- standard_dat$observer[1]
standard_dat <- standard_dat[order(standard_dat$time_of_day_sec), ]

standard_dat$pred_GS_season_full <- predict(srwa_gamGS_season_full, 
                                            newdata = standard_dat,
                                            type = "response", 
                                            exclude = c("s(date_fac)", 
                                                        "s(rain)"))
standard_dat$pred_GS_full <- predict(srwa_gamGS_full, 
                                     newdata = standard_dat,
                                     type = "response", 
                                     exclude = c("s(date_fac)", 
                                                 "s(rain)"))

ggplot(data = standard_dat, aes(x = time_of_day, y = pred_GS_season_full, 
                                color = season)) + 
  geom_line() + 
  facet_wrap(~point_id, ncol = 2) + 
  ggtitle("Standardized\nPredicted v. time of day\nGS_season_full\nDetections")

ggplot(data = standard_dat, aes(x = time_of_day, y = pred_GS_full, 
                                color = season)) + 
  geom_line() + 
  facet_wrap(~point_id, ncol = 2) + 
  ggtitle("Standardized\nPredicted v. time of day\nGS_full\nDetections")

# tst <- standard_dat[standard_dat$point_id == "FP02", ]
# tst <- tst[order(tst$time_of_day_sec, decreasing = FALSE), ]
# tst <- tst[tst$season == "summer", ]

ggplot(data = srwa, aes(x = time_of_day, y = SRWA, color = season)) + 
  geom_point() + 
  facet_wrap(~point_id, ncol = 2)

ggplot(data = srwa[srwa$point_id == "FP02", ], 
       aes(x = time_of_day, y = SRWA, color = season)) + 
  geom_jitter(height = 0.1) 
ggplot(data = srwa[srwa$point_id == "Rw04", ], 
       aes(x = time_of_day, y = SRWA, color = season)) + 
  geom_jitter(height = 0.1) 
ggplot(data = srwa[srwa$point_id == "Rw06", ], 
       aes(x = time_of_day, y = SRWA, color = season)) + 
  geom_jitter(height = 0.1) 
ggplot(data = srwa[srwa$point_id == "Rw05", ], 
       aes(x = time_of_day, y = SRWA, color = season)) + 
  geom_jitter(height = 0.1) 
ggplot(data = srwa[srwa$point_id == "Rw08", ], 
       aes(x = time_of_day, y = SRWA, color = season)) + 
  geom_jitter(height = 0.1) 
ggplot(data = srwa[srwa$point_id == "Rw11", ], 
       aes(x = time_of_day, y = SRWA, color = season)) + 
  geom_jitter(height = 0.1) 
ggplot(data = srwa[srwa$point_id == "Rw10", ], 
       aes(x = time_of_day, y = SRWA, color = season)) + 
  geom_jitter(height = 0.1) 
ggplot(data = srwa[srwa$point_id == "Rw14", ], 
       aes(x = time_of_day, y = SRWA, color = season)) + 
  geom_jitter(height = 0.1) 
ggplot(data = srwa[srwa$point_id == "Rw15", ], 
       aes(x = time_of_day, y = SRWA, color = season)) + 
  geom_jitter(height = 0.1) 
ggplot(data = srwa[srwa$point_id == "Rw13", ], 
       aes(x = time_of_day, y = SRWA, color = season)) + 
  geom_jitter(height = 0.1)
  


