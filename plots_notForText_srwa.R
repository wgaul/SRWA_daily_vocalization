#####################
## Graphs for Saipan Reed Warbler daily vocalization patterns
## Not for main text (experimental plots)
## 
## author: Willson Gaul  willson.gaul@gmail.com  & Ellie Roark
## created: 1 Oct. 2023
## last modified: 1 Oct. 2023
######################

# Graph RF CV predictions averaged over all days
rf_binCV_01_testData_summarised <- group_by(rf_binCV_01_testData, observer, 
                                            hour_of_day) %>%
  summarise(pred_rf_binCV01 = mean(pred_rf_binCV01), 
            SRWA_in_hour = mean(SRWA_in_hour))

ggplot(data = rf_binCV_01_testData_summarised,
       aes(x = hour_of_day, y = pred_rf_binCV01)) +
  geom_line(aes(color = observer)) + 
  geom_rug(data = srwa_bin[srwa_bin$SRWA_in_hour > 0, ],
           aes(x = hour_of_day, y = as.numeric(SRWA_in_hour)),
           sides = "t", length = unit(0.1, "npc")) +
  ggtitle("Standardized Predicted v. time of day\nrf_binCV01") +
  ylab("Probability") + xlab("Time of day (hour)") +
  theme_bw()

ggplot(data = rf_binCV_01_testData_summarised,
       aes(x = hour_of_day, y = pred_rf_binCV01)) +
  geom_line(aes(color = observer)) + 
  geom_point(
    aes(x = hour_of_day, y = as.numeric(SRWA_in_hour))) +
  # scale_color_viridis_d(name = "Location", option = "magma",
  #                       begin = 0.0, end = 0.8) +
  ggtitle("Standardized Predicted v. time of day\nrf_binCV01") +
  ylab("Probability") + xlab("Time of day (hour)") +
  theme_bw()




# average prediction over the entire year
standard_dat_avg_date <- group_by(standard_dat, point_id, time_of_day_sec) %>%
  summarise(pred_rf_01 = mean(pred_rf_01), pred_rf_02 = mean(pred_rf_02))



## Random forest standardized predictions
# Model with no location
ggplot(data = standard_dat, aes(x = time_of_day_sec, y = pred_rf_01)) + 
  geom_line() + 
  facet_wrap(~factor(doy)) + 
  ggtitle("rf_01") + 
  theme_bw()

# Model with location
ggplot(data = standard_dat, aes(x = time_of_day_sec, y = pred_rf_02)) + 
  geom_line(aes(color = point_id)) + 
  facet_wrap(~factor(doy)) + 
  ggtitle("rf_02") + 
  theme_bw()

ggplot(data = standard_dat, aes(x = time_of_day_sec, y = pred_rf_02)) + 
  geom_line(aes(color = factor(doy))) + 
  facet_wrap(~factor(point_id)) + 
  ggtitle("rf_02") + 
  theme_bw()

ggplot(data = standard_dat_avg_date, 
       aes(x = time_of_day_sec, y = pred_rf_02)) + 
  geom_line() + 
  geom_point(data = srwa, aes(x = time_of_day_sec, y = as.numeric(SRWA))) + 
  facet_wrap(~factor(point_id)) + 
  ggtitle("rf_02") + 
  theme_bw()





# view partial plots for time of day for all location block CV models
for(i in 1:length(rf_cv01)) {
  partialPlot(rf_cv01[[i]]$mod, pred.data = data.frame(rf_cv01[[i]]$train_dat[, colnames(rf_cv01[[i]]$train_dat) %in% rf_cv01[[i]]$pred_names]), x.var = time_of_day_sec, which.class = TRUE)
}



### rf_binCV_02
### graph RF predicting number of detections per hour
rf_binCV_02_testData_summarised <- group_by(rf_binCV_02_testData, observer, 
                                            hour_of_day) %>%
  summarise(pred_rf_binCV02 = mean(pred_rf_binCV02), 
            n_det = mean(n_det))

ggplot(data = rf_binCV_02_testData_summarised, aes(x = hour_of_day)) + 
  geom_line(aes(y = pred_rf_binCV02, color = observer)) + 
  geom_point(aes(y = n_det)) + 
  theme_bw()

## Graph RF predictions for location/day combinations
## TODO: Re-order the date factor to go in chronological order
ggplot(data = standard_dat, aes(x = time_of_day_sec, y = pred_rf_03, 
                                color = date_fac)) + 
  geom_line(show.legend = c(color = F, line.type = FALSE)) + 
  geom_rug(data = srwa[srwa$SRWA == T, ], 
           aes(x = time_of_day_sec, y = as.numeric(SRWA)), 
           sides = "t", length = unit(0.1, "npc")) +
  facet_wrap(~point_id, ncol = 2) + 
  scale_color_viridis_d(name = "Sampling Period", option = "magma", 
                        begin = 0.0, end = 0.8) + 
  ggtitle("Standardized Predicted v. time of day\nm_01") +
  ylab("Probability") + xlab("Time of day (seconds)") + 
  theme_bw()


## Graph GAM predictions for location/day combinations 
ggplot(data = standard_dat, aes(x = time_of_day_sec, y = pred_m06, 
                                color = season, line.type = date_fac)) + 
  geom_line(show.legend = c(color = TRUE, line.type = FALSE)) + 
  # geom_jitter(data = srwa[srwa$SRWA == T, ], 
  #            aes(x = time_of_day_sec, y = as.numeric(SRWA), 
  #                color = season), size = 0.8, height = 0.05) + 
  geom_rug(data = srwa[srwa$SRWA == T, ], 
           aes(x = time_of_day_sec, y = as.numeric(SRWA), color = season), 
           sides = "t", length = unit(0.1, "npc")) +
  facet_wrap(~point_id, ncol = 2) + 
  scale_color_viridis_d(name = "Sampling Period", option = "magma", 
                        begin = 0.0, end = 0.8) + 
  ggtitle("Standardized Predicted v. time of day\nm_06") +
  ylab("Probability") + xlab("Time of day (seconds)") + 
  theme_bw()

ggplot(data = standard_dat, aes(x = time_of_day_sec, y = pred_m06, 
                                color = date_fac)) + 
  geom_line(show.legend = c(color = F, line.type = FALSE)) + 
  # geom_jitter(data = srwa[srwa$SRWA == T, ], 
  #            aes(x = time_of_day_sec, y = as.numeric(SRWA), 
  #                color = season), size = 0.8, height = 0.05) + 
  geom_rug(data = srwa[srwa$SRWA == T, ], 
           aes(x = time_of_day_sec, y = as.numeric(SRWA), color = date_fac), 
           sides = "t", length = unit(0.1, "npc")) +
  facet_wrap(~point_id, ncol = 2) + 
  scale_color_viridis_d(option = "magma", 
                        begin = 0.0, end = 0.8) + 
  ggtitle("Standardized Predicted v. time of day\nm_06") +
  ylab("Probability") + xlab("Time of day (seconds)") + 
  theme_bw()

# m_01
ggplot(data = standard_dat, aes(x = time_of_day_sec, y = pred_m01, 
                                color = season, line.type = date_fac)) + 
  geom_line(show.legend = c(color = TRUE, line.type = FALSE)) + 
  # geom_jitter(data = srwa[srwa$SRWA == T, ], 
  #            aes(x = time_of_day_sec, y = as.numeric(SRWA), 
  #                color = season), size = 0.8, height = 0.05) + 
  geom_rug(data = srwa[srwa$SRWA == T, ], 
           aes(x = time_of_day_sec, y = as.numeric(SRWA), color = season), 
           sides = "t", length = unit(0.1, "npc")) +
  facet_wrap(~point_id, ncol = 2) + 
  scale_color_viridis_d(name = "Sampling Period", option = "magma", 
                        begin = 0.0, end = 0.8) + 
  ggtitle("Standardized Predicted v. time of day\nm_01") +
  ylab("Probability") + xlab("Time of day (seconds)") + 
  theme_bw()

# m02
ggplot(data = standard_dat, aes(x = time_of_day_sec, y = pred_m02, 
                                color = season, line.type = date_fac)) + 
  geom_line(show.legend = c(color = TRUE, line.type = FALSE)) + 
  # geom_jitter(data = srwa[srwa$SRWA == T, ], 
  #            aes(x = time_of_day_sec, y = as.numeric(SRWA), 
  #                color = season), size = 0.8, height = 0.05) + 
  geom_rug(data = srwa[srwa$SRWA == T, ], 
           aes(x = time_of_day_sec, y = as.numeric(SRWA), color = season), 
           sides = "t", length = unit(0.1, "npc")) +
  facet_wrap(~point_id, ncol = 2) + 
  scale_color_viridis_d(name = "Sampling Period", option = "magma", 
                        begin = 0.0, end = 0.8) + 
  ggtitle("Standardized Predicted v. time of day\nm_02\nGroupsHaveDifferentWiggliness") +
  ylab("Probability") + xlab("Time of day (seconds)") + 
  theme_bw()

# m01
ggplot(data = standard_dat, aes(x = time_of_day_sec, y = pred_m01, 
                                color = point_id, line.type = date_fac)) + 
  geom_line(show.legend = c(color = TRUE, line.type = FALSE)) + 
  # geom_jitter(data = srwa[srwa$SRWA == T, ], 
  #            aes(x = time_of_day_sec, y = as.numeric(SRWA), 
  #                color = season), size = 0.8, height = 0.05) + 
  facet_wrap(~season, ncol = 2) + 
  ggtitle("Standardized Predicted v. time of day\nm_01") +
  ylab("Probability") + xlab("Time of day (seconds)") + 
  theme_bw()







###### old graphs
# vocalization detected or not by time of day
# geom_smooth (not GAM model)
ggplot(data = srwa, aes(x = time_of_day, y = as.numeric(SRWA))) + 
  geom_point() + 
  geom_smooth() +
  facet_wrap(~point_id, ncol = 2) + 
  theme_bw() +
  ggtitle("Geom smooth\nNot GAM")