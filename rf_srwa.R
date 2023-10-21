#####################
## Fit Random Forests for Saipan Reed Warbler daily vocalization patterns
## 
## 
## author: Willson Gaul  willson.gaul@gmail.com  & Ellie Roark
## created: 13 Aug 2023
## last modified: 21 Oct 2023
######################
set.seed(211020231354)

### plot raw data
ggplot(data = srwa, aes(x = time_of_day, y = as.numeric(SRWA), 
                        color = season)) + 
  geom_smooth() + 
  geom_jitter(height = 0.1, alpha = 0.2) + 
  facet_wrap(~point_id, ncol = 3)



###### Models with location block cross validation ------------------------
## Models for binned data
# choose predictors
pred_names_cv01 <- c("hour_of_day", "observer", "n_min_in_group")
mtry <- floor(sqrt(length(pred_names_cv01)))
locs <- as.character(unique(srwa$point_id))
names(locs) <- locs

## fit location block cross-validated models.  Each CV fold has one location
## removed, so the number of folds is equal to the number of locations.
rf_binCV_01 <- lapply(locs, function(holdout_loc, dat, pred_names, mtry, 
                                     standard_dat) {
  train_dat <- dat[as.character(dat$point_id) != holdout_loc, ]
  test_dat <- dat[as.character(dat$point_id) == holdout_loc, ]
  mod <- randomForest( 
    x = train_dat[, which(colnames(train_dat) %in% pred_names)],
    y = factor(train_dat$SRWA_in_hour),
    ntree = 1000, 
    mtry = mtry,   
    nodesize = 1, 
    replace = TRUE, classwt = NULL, 
    importance = TRUE, 
    keep.forest = TRUE)
  test_dat$pred_rf_binCV01 <-  predict(
    mod, newdata = test_dat[, colnames(test_dat) %in% pred_names], 
    type = "prob")[, 2]
  standard_dat$pred_rf_binCV01 <- predict(
    mod, 
    newdata = standard_dat[, colnames(standard_dat) %in% pred_names], 
    type = "prob")[, 2]
  
  list(mod = mod, train_dat = train_dat, test_dat = test_dat, 
       pred_names = pred_names, standard_preds = standard_dat)
}, dat = srwa_bin, pred_names=pred_names_cv01, mtry=mtry, 
standard_dat = standard_dat_binned)

# bind all predictions to test folds together into one df
rf_binCV_01_testData <- bind_rows(
  lapply(rf_binCV_01, function(x) {x$test_dat}))


# choose predictors for null model
pred_names_h0 <- c("observer", "n_min_in_group")
mtry <- floor(sqrt(length(pred_names_h0)))
locs <- as.character(unique(srwa$point_id))
names(locs) <- locs

rf_binCV_h0 <- lapply(locs, function(holdout_loc, dat, pred_names, mtry, 
                                     standard_dat) {
  train_dat <- dat[as.character(dat$point_id) != holdout_loc, ]
  test_dat <- dat[as.character(dat$point_id) == holdout_loc, ]
  mod <- randomForest( 
    x = train_dat[, which(colnames(train_dat) %in% pred_names)],
    y = factor(train_dat$SRWA_in_hour),
    ntree = 1000, 
    mtry = mtry,   
    nodesize = 1, 
    replace = TRUE, classwt = NULL, 
    importance = TRUE, 
    keep.forest = TRUE)
  test_dat$pred_rf_binCVh0 <-  predict(
    mod, newdata = data.frame(test_dat[, colnames(test_dat) %in% pred_names]), 
    type = "prob")[, 2]
  standard_dat$pred_rf_binCVh0 <- predict(
    mod, 
    newdata = data.frame(tibble(standard_dat)[, colnames(standard_dat) %in% 
                                                pred_names]), 
    type = "prob")[, 2]
  
  list(mod = mod, train_dat = train_dat, test_dat = test_dat, 
       pred_names = pred_names, standard_preds = standard_dat)
}, dat = srwa_bin, pred_names=pred_names_h0, mtry=mtry, 
standard_dat = standard_dat_binned)

# bind all predictions to test folds together into one df
rf_binCV_h0_testData <- bind_rows(
  lapply(rf_binCV_h0, function(x) {x$test_dat}))


#### Bootstrapped, location block CV predictions
## Fit chosen model with 100 bootstrapped datasets, each of which get split
## into 11 cross-validation folds

# resample minutes
boot_dats <- lapply(1:100, function(x, orig_dat) {
  orig_dat[sample(1:nrow(orig_dat), size = nrow(orig_dat), replace = TRUE), ]
}, orig_dat = srwa)

# bin data into hours
boot_dats <- lapply(boot_dats, function(x) {
  binned_df <- group_by(x, point_id, observer, doy, date_num, filename, 
                       hour_of_day) %>%
    summarise(prop_srwa_det = prop_detected(SRWA), n_min_in_group = n(), 
              n_det = sum(as.numeric(as.logical(as.character(SRWA)))))
  binned_df$SRWA_in_hour <- binned_df$n_det > 0
  binned_df
})

# set parameters for model
mtry <- floor(sqrt(length(pred_names_cv01)))

rf_boot_bin_cv01 <- lapply(
  boot_dats, 
  function(x, pred_names, mtry, locs, 
           standard_dat_binned) {
    ## fit location block cross-validated models.  Each CV fold has one location
    ## removed, so the number of folds is equal to the number of locations.
    mods <- lapply(locs, function(holdout_loc, dat, pred_names, mtry, 
                                  standard_dat) {
      train_dat <- dat[as.character(dat$point_id) != holdout_loc, ]
      test_dat <- dat[as.character(dat$point_id) == holdout_loc, ]
      mod <- randomForest( 
        x = train_dat[, which(colnames(train_dat) %in% pred_names)],
      y = factor(train_dat$SRWA_in_hour),
      ntree = 1000, 
      mtry = mtry,   
      nodesize = 1, 
      replace = TRUE, classwt = NULL, 
      importance = TRUE, 
      keep.forest = TRUE)
    test_dat$pred_rf_binCV01 <-  predict(
      mod, newdata = test_dat[, colnames(test_dat) %in% pred_names], 
      type = "prob")[, 2]
    standard_dat$pred_rf_binCV01 <- predict(
      mod, 
      newdata = standard_dat[, colnames(standard_dat) %in% pred_names], 
      type = "prob")[, 2]
    
    list(mod = mod, train_dat = train_dat, test_dat = test_dat, 
         pred_names = pred_names, standard_preds = standard_dat)
  }, dat = x, pred_names=pred_names, mtry=mtry, 
  standard_dat = standard_dat_binned)
  
  # bind all predictions to test folds together into one df
  preds_test_data <- bind_rows(
    lapply(mods, function(x) {x$test_dat}))
  
  # get predictions to standardized data into one df
  preds_standard_data <- bind_rows(lapply(mods, function(x) x$standard_preds))
  
  # return
  list(mods = mods, preds_test_data = preds_test_data, 
       preds_standard_data = preds_standard_data)
}, pred_names = pred_names_cv01, mtry = mtry, locs = locs, 
standard_dat_binned = standard_dat_binned)


### bootstrap null model
# set parameters for model
mtry <- floor(sqrt(length(pred_names_h0)))

rf_boot_bin_cvh0 <- lapply(
  boot_dats, 
  function(x, pred_names, mtry, locs, 
           standard_dat_binned) {
    ## fit location block cross-validated models.  Each CV fold has one location
    ## removed, so the number of folds is equal to the number of locations.
    mods <- lapply(locs, function(holdout_loc, dat, pred_names, mtry, 
                                  standard_dat) {
      train_dat <- dat[as.character(dat$point_id) != holdout_loc, ]
      test_dat <- dat[as.character(dat$point_id) == holdout_loc, ]
      mod <- randomForest( 
        x = train_dat[, which(colnames(train_dat) %in% pred_names)],
        y = factor(train_dat$SRWA_in_hour),
        ntree = 1000, 
        mtry = mtry,   
        nodesize = 1, 
        replace = TRUE, classwt = NULL, 
        importance = TRUE, 
        keep.forest = TRUE)
      test_dat$pred_rf_binCV01 <-  predict(
        mod, newdata = test_dat[, colnames(test_dat) %in% pred_names], 
        type = "prob")[, 2]
      standard_dat$pred_rf_binCV01 <- predict(
        mod, 
        newdata = standard_dat[, colnames(standard_dat) %in% pred_names], 
        type = "prob")[, 2]
      
      list(mod = mod, train_dat = train_dat, test_dat = test_dat, 
           pred_names = pred_names, standard_preds = standard_dat)
    }, dat = x, pred_names=pred_names, mtry=mtry, 
    standard_dat = standard_dat_binned)
    
    # bind all predictions to test folds together into one df
    preds_test_data <- bind_rows(
      lapply(mods, function(x) {x$test_dat}))
    
    # get predictions to standardized data into one df
    preds_standard_data <- bind_rows(lapply(mods, function(x) x$standard_preds))
    
    # return
    list(mods = mods, preds_test_data = preds_test_data, 
         preds_standard_data = preds_standard_data)
  }, pred_names = pred_names_h0, mtry = mtry, locs = locs, 
  standard_dat_binned = standard_dat_binned)

#### end bootstrapping ----------------------







## Do not use tmodels below here ------------------
## No advantage over other models.

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
  test_dat$pred_rf_cv01 <-  predict(
    mod, newdata = test_dat[, colnames(test_dat) %in% pred_names], 
    type = "prob")[, 2]
  standard_dat$pred_rf_cv01 <- predict(
    mod, 
    newdata = standard_dat[, colnames(standard_dat) %in% pred_names], 
    type = "prob")[, 2]
  
  list(mod = mod, train_dat = train_dat, test_dat = test_dat, 
       pred_names = pred_names, standard_preds = standard_dat)
}, dat = srwa, pred_names=pred_names, mtry=mtry, standard_dat = standard_dat)

# bind all predictions to test folds together into one df
rf_cv01_testData <- bind_rows(lapply(rf_cv01, function(x) {x$test_dat}))



## No advantage over using T/F model
## RF predicting number of detections per hour
# choose predictors
pred_names <- c("hour_of_day", "observer", "n_min_in_group")
mtry <- floor(sqrt(length(pred_names)))
locs <- as.character(unique(srwa$point_id))
names(locs) <- locs

rf_binCV_02 <- lapply(locs, function(holdout_loc, dat, pred_names, mtry, 
                                     standard_dat) {
  train_dat <- dat[as.character(dat$point_id) != holdout_loc, ]
  test_dat <- dat[as.character(dat$point_id) == holdout_loc, ]
  mod <- randomForest( 
    x = train_dat[, which(colnames(train_dat) %in% pred_names)],
    y = as.numeric(as.character(train_dat$n_det)),
    ntree = 1000, 
    mtry = mtry,   
    nodesize = 1, 
    replace = TRUE, classwt = NULL, 
    importance = TRUE, 
    keep.forest = TRUE)
  test_dat$pred_rf_binCV02 <-  predict(
    mod, newdata = test_dat[, colnames(test_dat) %in% pred_names], 
    type = "response")
  standard_dat$pred_rf_binCV02 <- predict(
    mod, 
    newdata = standard_dat[, colnames(standard_dat) %in% pred_names], 
    type = "response")
  
  list(mod = mod, train_dat = train_dat, test_dat = test_dat, 
       pred_names = pred_names, standard_preds = standard_dat)
}, dat = srwa_bin, pred_names=pred_names, mtry=mtry, 
standard_dat = standard_dat_binned)

# bind all predictions to test folds together into one df
rf_binCV_02_testData <- bind_rows(
  lapply(rf_binCV_02, function(x) {x$test_dat}))




##### Exploratory models with no cross-validation ---------------------------
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


