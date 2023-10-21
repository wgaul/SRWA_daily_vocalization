#####################
## Graphs for Saipan Reed Warbler daily vocalization patterns
## Also tables and numbers for main text
## 
## author: Willson Gaul  willson.gaul@gmail.com  & Ellie Roark
## created: 4 Aug 2023
## last modified: 19 Aug 2023
######################

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

# N minutes analyzed
dim(srwa)

# N minutes analyzed by multiple observers
str(combined_dat_wide)
nrow(combined_dat_wide)

krp # look at Krippendorf's alpha

# overall model performance


## Table of model outputs
var_imp_rf_01

aic_m06
anova(m_06)

aic_m01
anova(m_01)
plot(m_01)

# summary_m_01 <- summary(m_01)
# fixed_effects_m_01 <- summary_m_01$p.table
# smooth_terms_m_01 <- summary_m_01$s.table
# fixed_effects_m_01
# smooth_terms_m_01


### bootstrap graphs -------------
# Graph bootstrapped RF CV predictions averaged over all days
rf_boot_binCV_01_testData_summarised <- bind_rows(
  lapply(rf_boot_bin_cv01, function(x) x$preds_test_data))

rf_boot_binCV_01_testData_summarised <- group_by(
  rf_boot_binCV_01_testData_summarised, hour_of_day) %>%
  summarise(pred_rf_binCV01_mean = mean(pred_rf_binCV01), 
            SRWA_in_hour = mean(SRWA_in_hour), 
            up_95_ci = as.numeric(quantile(pred_rf_binCV01, probs = 0.95)), 
            low_95_ci = as.numeric(quantile(pred_rf_binCV01, probs = 0.05)))

ggplot(data = rf_boot_binCV_01_testData_summarised,
       aes(x = hour_of_day)) +
  geom_line(aes(y = pred_rf_binCV01_mean)) + 
  geom_line(linetype = "dotted", aes(y = up_95_ci)) + 
  geom_line(linetype = "dotted", aes(y = low_95_ci)) + 
  geom_rug(data = srwa_bin[srwa_bin$SRWA_in_hour > 0, ],
           aes(x = hour_of_day, y = as.numeric(SRWA_in_hour)),
           sides = "t", length = unit(0.1, "npc")) +
  ggtitle("Predicted probability of detecting a SRWA\nin an hour\nif you listen to four minutes from that hour\nrf_binCV01 bootstrapped") +
  ylab("Probability") + xlab("Time of day (hour)") +
  theme_bw()


### end bootstrap graphs ---------





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


