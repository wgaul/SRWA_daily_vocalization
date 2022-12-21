#####################
## Fit GAMs for Saipan Reed Warbler daily vocalization patterns
## 
## 
## author: Willson Gaul  willson.gaul@gmail.com   & Ellie Roark
## created: 28 Oct 2022
## last modified: 21 Dec 2022
######################

srwa_gam_H0 <- gam(SRWA ~ 1, 
                   family = binomial(), 
                   data = data.frame(srwa))
gam.check(srwa_gam_H0)

# Group level smoother with random effect letting intercept vary by location (G)
srwa_gamG <- gam(SRWA ~ 1 + s(time_of_day_sec, k = 10, bs = "cc") + 
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
srwa_gamGS <- gam(SRWA ~ 1 + s(time_of_day_sec, k = 10, bs = "cc") + 
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
srwa_gamGI <- gam(SRWA ~ 1 + s(time_of_day_sec, k = 10, m = 2, bs = "cc") + 
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
                          s(date_fac, k = 12, bs = "re") +
                          s(observer, k = 2, bs = "re") + 
                          s(rain, k = 2, bs = "re"), 
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

ggplot(data = srwa, aes(x = time_of_day, y = pred_gamGS_full)) + 
  geom_point(data = srwa[srwa$SRWA == TRUE, ], 
             aes(x = time_of_day, y = pred_gamGS_full)) +
  geom_line() + 
  facet_wrap(~point_id, ncol = 2) + 
  ggtitle("Predicted v. time of day\nGS_full\nDetections")

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
    srwa_gamGS_noise)


summary(srwa_gamGS_full)
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


## try adding autocorrelation
# see http://r.qcbs.ca/workshop08/book-en/quick-intro-to-generalized-additive-mixed-models-gamms.html
acf(resid(srwa_gam_02)) # autocorrelation at all time points
pacf(resid(srwa_gam_02)) # autocorrelation at all lags after controlling for shorter lags

# gamm with no autocorrelation
srwa_gamm_ar0 <- gamm(SRWA ~ 1 + observer + factor(as.character(point_id)) + 
                        s(as.numeric(time_of_day), bs = "cc", k = 6) + 
                        s(as.numeric(time_of_day), bs = "cc", 
                          by = factor(as.character(point_id)), 
                          k = 6, m = 1),
                      family = binomial(), 
                      data = data.frame(srwa))

# autocorrelation at 1 minute lag
srwa_gamGS_ar1 <- gamm(SRWA ~ 1 + s(time_of_day_sec, k = 10, bs = "cc") + 
                        s(time_of_day_sec, point_id, k = 10, bs = "fs", m = 2), 
                      data = srwa, method = "REML", 
                      family = binomial(), 
                      correlation = corARMA(
                        form = ~1 | time_of_day_sec, p = 60),
                      knots = list(time_of_day_sec=c(0, 86400)))
  
  


# autocorrelation at 2 minute lag
srwa_gamm_ar2 <- gamm(SRWA ~ 1 + observer + factor(as.character(point_id)) + 
                        s(as.numeric(time_of_day), bs = "cc", k = 6) + 
                        s(as.numeric(time_of_day), bs = "cc", 
                          by = factor(as.character(point_id)), 
                          k = 6, m = 1), 
                      correlation = corARMA(
                        form = ~1 | as.numeric(time_of_day), p = 2),
                      family = binomial(), 
                      data = data.frame(srwa))

AIC(srwa_gamm_ar0$lme, srwa_gamm_ar1$lme)
