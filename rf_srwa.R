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

#### Model with location
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


#### Model with factor date, rather than smooth
# choose predictors
pred_names <- c("time_of_day_sec", "date_fac", "observer", "rain_wind", 
                "point_id")
mtry <- floor(sqrt(length(pred_names)))

# model (no cross validation)
rf_03 <- randomForest( 
  x = srwa[, which(colnames(srwa) %in% pred_names)],
  y = factor(srwa$SRWA),
  ntree = 1000, 
  mtry = mtry,   
  nodesize = 1, 
  replace = TRUE, classwt = NULL, 
  importance = TRUE, 
  keep.forest = TRUE)

rf_03

# predict to original data
srwa$pred_rf_03 <- predict(rf_03, 
                           newdata = srwa[, colnames(srwa) %in% pred_names], 
                           type = "prob")[, 2]

# predict to standardized data
standard_dat$pred_rf_03 <- predict(
  rf_03, 
  newdata = standard_dat[, colnames(standard_dat) %in% pred_names], 
  type = "prob")[, 2]

# variable importance
varImpPlot(rf_03)
var_imp_rf_03 <- data.frame(rf_03$importance)
var_imp_rf_03$variable <- row.names(var_imp_rf_03)
rownames(var_imp_rf_03) <- NULL
var_imp_rf_03 <- var_imp_rf_03[, c("variable", "MeanDecreaseGini")]
var_imp_rf_03 <- var_imp_rf_03[order(var_imp_rf_03$MeanDecreaseGini, 
                                     decreasing = T), ]
var_imp_rf_03


## Model with no location or doy (b/c doy is strongly correlated with location,
## allowing model to fit a different response to each location or sampling
## event)
# choose predictors
pred_names <- c("time_of_day_sec", "observer", "rain_wind")
mtry <- floor(sqrt(length(pred_names)))
if(mtry < 2 ) mtry <- 2

# model (no cross validation)
rf_04 <- randomForest( 
  x = srwa[, which(colnames(srwa) %in% pred_names)],
  y = factor(srwa$SRWA),
  ntree = 1000, 
  mtry = mtry,   
  nodesize = 1, 
  replace = TRUE, classwt = NULL, 
  importance = TRUE, 
  keep.forest = TRUE)

rf_04

# predict to original data
srwa$pred_rf_04 <- predict(rf_04, 
                           newdata = srwa[, colnames(srwa) %in% pred_names], 
                           type = "prob")[, 2]

# predict to standardized data
standard_dat$pred_rf_04 <- predict(
  rf_04, 
  newdata = standard_dat[, colnames(standard_dat) %in% pred_names], 
  type = "prob")[, 2]

# variable importance
varImpPlot(rf_04)
var_imp_rf_04 <- data.frame(rf_04$importance)
var_imp_rf_04$variable <- row.names(var_imp_rf_04)
rownames(var_imp_rf_04) <- NULL
var_imp_rf_04 <- var_imp_rf_04[, c("variable", "MeanDecreaseGini")]
var_imp_rf_04 <- var_imp_rf_04[order(var_imp_rf_04$MeanDecreaseGini, 
                                     decreasing = T), ]
var_imp_rf_04










###### Models with location block cross validation ------------------------
## Model with no location
# choose predictors
pred_names <- c("time_of_day_sec", "observer", "rain_wind")
mtry <- floor(sqrt(length(pred_names)))
locs <- as.character(unique(srwa$point_id))
names(locs) <- locs

rf_cv01 <- lapply(locs, function(holdout_loc, dat, pred_names, mtry, 
                                 standard_dat) {
  train_dat <- dat[as.character(dat$point_id) != holdout_loc, ]
  test_dat <- dat[as.character(dat$point_id) == holdout_loc, ]
  mod <- randomForest( 
    x = train_dat[, which(colnames(train_dat) %in% pred_names)],
    y = factor(train_dat$SRWA),
    ntree = 1000, 
    mtry = mtry,   
    nodesize = 1, 
    replace = TRUE, classwt = NULL, 
    importance = TRUE, 
    keep.forest = TRUE)
  test_dat$pred_rw_cv01 <-  predict(
    mod, newdata = test_dat[, colnames(test_dat) %in% pred_names], 
    type = "prob")[, 2]
  standard_dat$pred_rw_cv01 <- predict(
    mod, 
    newdata = standard_dat[, colnames(standard_dat) %in% pred_names], 
    type = "prob")[, 2]
  
  list(mod = mod, train_dat = train_dat, test_dat = test_dat, 
       pred_names = pred_names, standard_preds = standard_dat)
}, dat = srwa, pred_names=pred_names, mtry=mtry, standard_dat = standard_dat)

# bind all predictions to test folds together into one df
rf_cv01_testData <- bind_rows(lapply(rw_cv01, function(x) {x$test_dat}))


