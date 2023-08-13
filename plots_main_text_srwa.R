#####################
## Graphs for Saipan Reed Warbler daily vocalization patterns
## Also tables and numbers for main text
## 
## author: Willson Gaul  willson.gaul@gmail.com  & Ellie Roark
## created: 4 Aug 2023
## last modified: 13 Aug 2023
######################

srwa <- group_by(srwa, point_id)

# vocalization detected or not by time of day
# geom_smooth (not GAM model)
ggplot(data = srwa, aes(x = time_of_day, y = as.numeric(SRWA))) + 
  geom_point() + 
  geom_smooth() +
  facet_wrap(~point_id, ncol = 2) + 
  theme_bw() +
  ggtitle("Geom smooth\nNot GAM")



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
aic_m01

summary_m_01 <- summary(m_01)
fixed_effects_m_01 <- summary_m_01$p.table
smooth_terms_m_01 <- summary_m_01$s.table

fixed_effects_m_01
smooth_terms_m_01

## Graph RF predictions for location/day combinations
ggplot(data = standard_dat, aes(x = time_of_day_sec, y = pred_rf, 
                                color = season, line.type = date_fac)) + 
  geom_line(show.legend = c(color = TRUE, line.type = FALSE)) + 
  geom_rug(data = srwa[srwa$SRWA == T, ], 
           aes(x = time_of_day_sec, y = as.numeric(SRWA), color = season), 
           sides = "t", length = unit(0.1, "npc")) +
  facet_wrap(~point_id, ncol = 2) + 
  scale_color_viridis_d(name = "Sampling Period", option = "magma", 
                        begin = 0.0, end = 0.8) + 
  ggtitle("Standardized Predicted v. time of day\nm_01") +
  ylab("Probability") + xlab("Time of day (seconds)") + 
  theme_bw()


## Graph GAM predictions for location/day combinations 
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

# Graph RF predictions for locations, averaged over all days in each season
ggplot(data = standard_dat_averaged_over_days, 
       aes(x = time_of_day_sec, y = pred_rf, 
           color = season)) + 
  geom_line(show.legend = c(color = TRUE, line.type = FALSE)) + 
  geom_rug(data = srwa[srwa$SRWA == T, ], 
           aes(x = time_of_day_sec, y = as.numeric(SRWA), color = season), 
           sides = "t", length = unit(0.1, "npc")) +
  facet_wrap(~point_id, ncol = 2) + 
  scale_color_viridis_d(name = "Sampling Period", option = "magma", 
                        begin = 0.0, end = 0.8) + 
  ggtitle("Standardized Predicted v. time of day\nm_01") +
  ylab("Probability") + xlab("Time of day (seconds)") + 
  theme_bw()


