#####################
## Fit GAMs for Saipan Reed Warbler daily vocalization patterns
## 
## 
## author: Willson Gaul  willson.gaul@gmail.com   & Ellie Roark
## created: 28 Oct 2022
## last modified: 22 Dec 2022
######################

srwa_rarefied_gam_H0 <- gam(SRWA ~ 1, 
                   family = binomial(), 
                   data = data.frame(srwa_rarefied))
gam.check(srwa_rarefied_gam_H0)

# Group level smoother with random effect letting intercept vary by location (G)
srwa_rarefied_gamG <- gam(SRWA ~ 1 + s(time_of_day_sec, k = 10, bs = "cc") + 
                   s(point_id, k = 8, bs = "re"), 
                 data = srwa_rarefied, method = "REML", 
                 family = binomial(), 
                 knots = list(time_of_day_sec=c(0, 86400)))
gam.check(srwa_rarefied_gamG)
plot(srwa_rarefied_gamG)
srwa_rarefied$pred_gamG <- predict(srwa_rarefied_gamG, type = "response")
ggplot(data = srwa_rarefied, aes(x = pred_gamG, y = SRWA)) + 
  geom_point() + 
  facet_wrap(~point_id) + 
  ggtitle("Predicted v. observed")

ggplot(data = srwa_rarefied, aes(x = time_of_day, y = pred_gamG)) + 
  geom_point() + 
  facet_wrap(~point_id) + 
  ggtitle("Predicted v. time of day")


# global smoother plus group-level smoothers that have the same wiggliness (GS)
srwa_rarefied_gamGS <- gam(SRWA ~ 1 + s(time_of_day_sec, k = 10, bs = "cc") + 
                    s(time_of_day_sec, point_id, k = 10, bs = "fs", m = 2), 
                  data = srwa_rarefied, method = "REML", 
                  family = binomial(), 
                  knots = list(time_of_day_sec=c(0, 86400)))
gam.check(srwa_rarefied_gamGS)
plot(srwa_rarefied_gamGS)
srwa_rarefied$pred_gamGS <- predict(srwa_rarefied_gamGS, type = "response")
ggplot(data = srwa_rarefied, aes(x = pred_gamGS, y = SRWA)) + 
  geom_point() + 
  facet_wrap(~point_id) + 
  ggtitle("Predicted v. observed\nGS")

ggplot(data = srwa_rarefied, aes(x = time_of_day, y = pred_gamGS)) + 
  geom_point(data = srwa_rarefied[srwa_rarefied$SRWA == TRUE, ], 
             aes(x = time_of_day, y = pred_gamGS)) +
  geom_line() + 
  facet_wrap(~point_id, ncol = 2) + 
  ggtitle("Predicted v. time of day\nGS\nDetections\nRarefied Data")

ggplot(data = srwa_rarefied, aes(x = time_of_day, y = pred_gamGS)) + 
  geom_point(data = srwa_rarefied[srwa_rarefied$SRWA == FALSE, ], 
             aes(x = time_of_day, y = pred_gamGS)) +
  geom_line() + 
  facet_wrap(~point_id, ncol = 2) + 
  ggtitle("Predicted v. time of day\nGS\nNon-detections")


# global smoother plus group-level smoothers that have different wiggliness (GI)
srwa_rarefied_gamGI <- gam(SRWA ~ 1 + s(time_of_day_sec, k = 10, m = 2, bs = "cc") + 
                    s(time_of_day_sec, by = point_id, k = 10, m = 1, bs = "cc"), 
                  data = srwa_rarefied, method = "REML", 
                  family = binomial(), 
                  knots = list(time_of_day_sec=c(0, 86400)))
gam.check(srwa_rarefied_gamGI)
plot(srwa_rarefied_gamGI)
srwa_rarefied$pred_gamGI <- predict(srwa_rarefied_gamGI, type = "response")
ggplot(data = srwa_rarefied, aes(x = pred_gamGI, y = SRWA)) + 
  geom_point() + 
  facet_wrap(~point_id) + 
  ggtitle("Predicted v. observed\nGI")

ggplot(data = srwa_rarefied, aes(x = time_of_day, y = pred_gamGI)) + 
  geom_point() + 
  facet_wrap(~point_id, ncol = 2) + 
  ggtitle("Predicted v. time of day\nGI")


## models with random effect for day
# global smoother plus group-level smoothers that have the same wiggliness (GS)
srwa_rarefied_gamGS_full <- gam(SRWA ~ 1 + s(time_of_day_sec, k = 10, bs = "cc") + 
                         s(time_of_day_sec, point_id, k = 10, bs = "fs", 
                           m = 2) + 
                         s(date_fac, k = 12, bs = "re") +
                         s(observer, k = 2, bs = "re") + 
                         s(rain, k = 2, bs = "re"), 
                       data = srwa_rarefied, method = "REML", 
                       family = binomial(), 
                       knots = list(time_of_day_sec=c(0, 86400)))
gam.check(srwa_rarefied_gamGS_full)
plot(srwa_rarefied_gamGS_full)
srwa_rarefied$pred_gamGS_full <- predict(srwa_rarefied_gamGS_full, 
                                         type = "response")
ggplot(data = srwa_rarefied, aes(x = pred_gamGS_full, y = SRWA)) + 
  geom_point() + 
  facet_wrap(~point_id) + 
  ggtitle("Predicted v. observed\nGS_full")

ggplot(data = srwa_rarefied, aes(x = time_of_day, y = pred_gamGS_full)) + 
  geom_point(data = srwa_rarefied[srwa_rarefied$SRWA == TRUE, ], 
             aes(x = time_of_day, y = pred_gamGS_full)) +
  geom_line() + 
  facet_wrap(~point_id, ncol = 2) + 
  ggtitle("Predicted v. time of day\nGS_full\nDetections")

# model with rain but not day or observer
srwa_rarefied_gamGS_noise <- gam(SRWA ~ 1 + s(time_of_day_sec, k = 10, bs = "cc") + 
                          s(time_of_day_sec, point_id, k = 10, bs = "fs", 
                            m = 2) + 
                          s(rain, k = 2, bs = "re"), 
                        data = srwa_rarefied, method = "REML", 
                        family = binomial(), 
                        knots = list(time_of_day_sec=c(0, 86400)))
gam.check(srwa_rarefied_gamGS_noise)
plot(srwa_rarefied_gamGS_noise)

##### model comparison
AIC(srwa_rarefied_gam_H0, srwa_rarefied_gamG, srwa_rarefied_gamGS, 
    srwa_rarefied_gamGI, srwa_rarefied_gamGS_full, 
    srwa_rarefied_gamGS_noise)


summary(srwa_rarefied_gamGS_full)
summary(srwa_rarefied_gamGS_noise)


summary(srwa_rarefied_gamGS)
plot(srwa_rarefied_gamGS, pages = 9, residuals = TRUE)
plot(srwa_rarefied_gamGS, pages = 5, seWithMean = TRUE)






srwa_rarefied$resids_GS <- srwa_rarefied_gamGS$residuals
ggplot(data = srwa_rarefied, aes(x = time_of_day, y = resids_GS)) + 
  geom_point() + 
  facet_wrap(~point_id) +
  ylim(-5, 20) + 
  ggtitle("GS")
srwa_rarefied$resids_GI <- srwa_rarefied_gamGI$residuals
ggplot(data = srwa_rarefied, aes(x = time_of_day, y = resids_GI)) + 
  geom_point() + 
  facet_wrap(~point_id) +
  ylim(-5, 20) + 
  ggtitle("GI")

ggplot(data = srwa_rarefied, aes(x = point_id, y = resids_GS)) + 
  geom_boxplot() + 
  ylim(-5, 20)
ggplot(data = srwa_rarefied, aes(x = point_id, y = resids_GI)) + 
  geom_boxplot() + 
  ylim(-5, 20)
