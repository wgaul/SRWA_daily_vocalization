#####################
## Graphs for Saipan Reed Warbler daily vocalization patterns
## Also tables and numbers for main text
## 
## author: Willson Gaul  willson.gaul@gmail.com  & Ellie Roark
## created: 4 Aug 2023
## last modified: 28 Oct 2023
######################

t_size <- 30

srwa <- group_by(srwa, point_id)


## Numbers for text
# N locations per sampling period
loc_dat_comb <- select(srwa, date_fac, point_id) %>%
  unique()
loc_dat_comb <- data.frame(table(loc_dat_comb$date_fac, loc_dat_comb$point_id))
loc_dat_comb <- loc_dat_comb[loc_dat_comb$Freq > 0, ]
loc_dat_comb[grepl("2021.*", loc_dat_comb$Var1), ] # N locations in 2021

loc_period_comb <- select(srwa, season, point_id) %>%
  unique()
loc_period_comb <- data.frame(table(loc_period_comb$season, 
                                    loc_period_comb$point_id))
loc_period_comb <- loc_period_comb[loc_period_comb$Freq > 0, ]
# N locations per sampling period
rowSums(table(loc_period_comb$Var1, loc_period_comb$Var2)) 
table(loc_period_comb$Var1, loc_period_comb$Var2)
table(loc_period_comb$Var1)

# total number of locations
length(unique(srwa$point_id))

# N minutes analyzed
dim(srwa)

# N hours analyzed
dim(srwa_bin)

# earliest detection
srwa[which(srwa$time_of_day == 
                         min(srwa$time_of_day[srwa$SRWA == TRUE]) & 
             srwa$SRWA == TRUE), ]
srwa$time_of_day[which(srwa$time_of_day == 
             min(srwa$time_of_day[srwa$SRWA == TRUE]) & 
             srwa$SRWA == TRUE)]

# latest detection
srwa[which(srwa$time_of_day == 
             max(srwa$time_of_day[srwa$SRWA == TRUE]) & 
             srwa$SRWA == TRUE), ]
srwa$time_of_day[which(srwa$time_of_day == 
             max(srwa$time_of_day[srwa$SRWA == TRUE]) & 
             srwa$SRWA == TRUE)]
# 95% of detections
quantile(srwa$time_of_day[srwa$SRWA == TRUE], probs = c(0.025, 0.975))

# hours with detections
table(srwa$hour_of_day[srwa$SRWA == T])

# N minutes analyzed by multiple observers
str(combined_dat_wide)
nrow(combined_dat_wide)

krp # look at Krippendorf's alpha

# overall model performance
eval_df_ci


### bootstrap graphs -------------
# Graph bootstrapped RF CV predictions averaged over all days
rf_boot_binCV_01_testData_summarised <- bind_rows(
  lapply(rf_boot_bin_cv01, function(x) x$preds_test_data))

rf_boot_binCV_01_testData_summarised <- group_by(
  rf_boot_binCV_01_testData_summarised, hour_of_day) %>%
  summarise(pred_rf_binCV01_mean = mean(pred_rf_binCV01), 
            SRWA_in_hour = mean(SRWA_in_hour), 
            up_95_ci = as.numeric(quantile(pred_rf_binCV01, probs = 0.975)), 
            low_95_ci = as.numeric(quantile(pred_rf_binCV01, probs = 0.025)))

# graph
daily_pattern_plot <- ggplot(data = rf_boot_binCV_01_testData_summarised,
                             aes(x = hour_of_day)) +
  geom_line(aes(y = pred_rf_binCV01_mean), color = "blue", size = 0.1*t_size) + 
  geom_line(linetype = "dashed", aes(y = up_95_ci), color = "blue", 
            size = 0.07*t_size) + 
  geom_line(linetype = "dashed", aes(y = low_95_ci), color = "blue", 
            size = 0.07*t_size, alpha = 2) + 
  geom_jitter(data = srwa_bin, 
              aes(x = hour_of_day, y = as.numeric(SRWA_in_hour)), 
              height = 0.01, width = 0.4, alpha = 0.08, size = 0.1*t_size) + 
  # ggtitle("Predicted probability of detecting a SRWA\nin an hour\nif you listen to four minutes from that hour\nrf_binCV01 bootstrapped") +
  ylab("Probability") + xlab("Time of day (hour)") +
  # xlim(-0.01, 24.1) + # go left of 0 to allow for jittered points
  theme_bw()
daily_pattern_plot

### end bootstrap graphs ---------


### Save plots ---------------------------------------------
## save as jpg
ggsave("Fig1.jpg", daily_pattern_plot + 
         theme(text = element_text(size = t_size)), 
       width = 25, height = 25, units = "cm", 
       device = "jpg")

## save as eps
ggsave("Figure 1.eps", daily_pattern_plot, width = 25, height = 25, 
       units = "cm", device = "eps")
