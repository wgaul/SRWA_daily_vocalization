#####################
## Fit Random Forests for Saipan Reed Warbler daily vocalization patterns
## 
## 
## author: Willson Gaul  willson.gaul@gmail.com  & Ellie Roark
## created: 13 Aug 2023
## last modified: 1 Oct 2023
######################

### plot raw data
ggplot(data = srwa, aes(x = time_of_day, y = as.numeric(SRWA), 
                        color = season)) + 
  geom_smooth() + 
  geom_jitter(height = 0.1, alpha = 0.2) + 
  facet_wrap(~point_id, ncol = 3)

## Model with no location
# choose predictors
pred_names <- c("time_of_day_sec", "doy", "observer", "rain_wind")
mtry <- floor(sqrt(length(pred_names)))

# model (no cross validation)
rf_01 <- randomForest( 
  x = srwa[, which(colnames(srwa) %in% pred_names)],
  y = factor(srwa$SRWA),
  ntree = 1000, 
  mtry = mtry,   
  nodesize = 1, 
  replace = TRUE, classwt = NULL, 
  importance = TRUE, 
  keep.forest = TRUE)

rf_01

# predict to original data
srwa$pred_rf_01 <- predict(rf_01, 
                           newdata = srwa[, colnames(srwa) %in% pred_names], 
                           type = "prob")[, 2]

# predict to standardized data
standard_dat$pred_rf_01 <- predict(
  rf_01, 
  newdata = standard_dat[, colnames(standard_dat) %in% pred_names], 
  type = "prob")[, 2]

# # average predictions over all days at each location in each season
# standard_dat_averaged_over_days <- group_by(standard_dat, 
#                                             time_of_day_sec, point_id, 
#                                             season) %>%
#   summarise(pred_m01 = mean(pred_m01), pred_rf = mean(pred_rf))

# variable importance
varImpPlot(rf_01)
var_imp_rf_01 <- data.frame(rf_01$importance)
var_imp_rf_01$variable <- row.names(var_imp_rf_01)
rownames(var_imp_rf_01) <- NULL
var_imp_rf_01 <- var_imp_rf_01[, c("variable", "MeanDecreaseGini")]
var_imp_rf_01 <- var_imp_rf_01[order(var_imp_rf_01$MeanDecreaseGini, 
                                     decreasing = T), ]
var_imp_rf_01

### Model with location
# choose predictors
pred_names <- c("time_of_day_sec", "doy", "observer", "rain_wind", "point_id")
mtry <- floor(sqrt(length(pred_names)))

# model (no cross validation)
rf_02 <- randomForest( 
  x = srwa[, which(colnames(srwa) %in% pred_names)],
  y = factor(srwa$SRWA),
  ntree = 1000, 
  mtry = mtry,   
  nodesize = 1, 
  replace = TRUE, classwt = NULL, 
  importance = TRUE, 
  keep.forest = TRUE)

rf_02

# predict to original data
srwa$pred_rf_02 <- predict(rf_02, 
                           newdata = srwa[, colnames(srwa) %in% pred_names], 
                           type = "prob")[, 2]

# predict to standardized data
standard_dat$pred_rf_02 <- predict(
  rf_02, 
  newdata = standard_dat[, colnames(standard_dat) %in% pred_names], 
  type = "prob")[, 2]

# variable importance
varImpPlot(rf_02)
var_imp_rf_02 <- data.frame(rf_02$importance)
var_imp_rf_02$variable <- row.names(var_imp_rf_02)
rownames(var_imp_rf_02) <- NULL
var_imp_rf_02 <- var_imp_rf_02[, c("variable", "MeanDecreaseGini")]
var_imp_rf_02 <- var_imp_rf_02[order(var_imp_rf_02$MeanDecreaseGini, 
                                     decreasing = T), ]
var_imp_rf_02


