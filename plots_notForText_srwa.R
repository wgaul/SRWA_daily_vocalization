#####################
## Graphs for Saipan Reed Warbler daily vocalization patterns
## Not for main text (experimental plots)
## 
## author: Willson Gaul  willson.gaul@gmail.com  & Ellie Roark
## created: 1 Oct. 2023
## last modified: 1 Oct. 2023
######################

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










###### old graphs
# vocalization detected or not by time of day
# geom_smooth (not GAM model)
ggplot(data = srwa, aes(x = time_of_day, y = as.numeric(SRWA))) + 
  geom_point() + 
  geom_smooth() +
  facet_wrap(~point_id, ncol = 2) + 
  theme_bw() +
  ggtitle("Geom smooth\nNot GAM")