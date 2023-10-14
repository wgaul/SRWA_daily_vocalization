#####################
## Evaluate models for Saipan Reed Warbler daily vocalization patterns
## 
## 
## author: Willson Gaul  willson.gaul@gmail.com  & Ellie Roark
## created: 1 Oct 2023
## last modified: 14 Oct 2023
######################
library(pROC)
library(psych)

#### Define functions ---------------------------------------------------------
# Cohen's Kappa --------------------------------------
kappa_calc <- function(x, resp, pred) {
  ## Function to calculate Cohen's Kappa using the best threshold
  # make a 2 column df with observed responses (0 or 1) and a T/F whether the 
  # predicted value is above threshold x.  These will be used to compute
  # Cohen's kappa for agreement between categorical response (0, 1) and 
  # categorical predictor (0, 1)
  # ARGS: x - cutoff to test
  #       resp - vector of observed responses (0 or 1)
  #       pred - numeric vector of continuous predictions
  vals <- data.frame(resp = factor(as.numeric(as.logical(resp[!is.na(resp)]))), 
                     pred = factor(as.numeric(pred[!is.na(pred)] > x)))
  psych::cohen.kappa(vals)$kappa
}
kappas <- seq(from = 0, to = 1, by = 0.01) # Thresholds to try
names(kappas) <- kappas

#### End define functions -----------------------------------------------------

## Make list of models to evaluate

# mods <- list(rf_01 = rf_01, rf_02 = rf_02, m_07 = m_07, m_08 = m_08)

eval_df <- data.frame(model = c("rf_01", "rf_02", "rf_03", "rf_04", "rf_cv01",
                                "rf_binCV_01", "rf_binCV_h0", "m_07", "m_08"), 
                      AUC = NA, Kappa = NA, TPR = NA, FPR = NA, TNR = NA, 
                      FNR = NA, Brier = NA)

# performance for rf_01
eval_df$AUC[eval_df$model == "rf_01"] <-  as.numeric(pROC::roc(
  response = srwa$SRWA, 
  predictor = srwa$pred_rf_01)$auc)

k_res <- sapply(kappas, kappa_calc, 
                resp = srwa$SRWA, 
                pred = srwa$pred_rf_01)
# return kappa and threshold that maximised kappa
eval_df$Kappa[eval_df$model == "rf_01"] <- max(k_res)
if(max(k_res) == 0) {thresh <- 0.5} else {
  thresh <- as.numeric(names(k_res[k_res == max(k_res)][1]))}

# TPR = TP / P
eval_df$TPR[eval_df$model == "rf_01"] <- length(
  which(srwa$SRWA == TRUE & (srwa$pred_rf_01 >= thresh))) / 
  length(which(srwa$SRWA == TRUE))
# TNR = TN / N
eval_df$TNR[eval_df$model == "rf_01"] <- length(
  which(srwa$SRWA == FALSE & (srwa$pred_rf_01 < thresh))) / 
  length(which(srwa$SRWA == FALSE))
# FPR = FP / N
eval_df$FPR[eval_df$model == "rf_01"] <- length(
  which(srwa$SRWA == FALSE & (srwa$pred_rf_01 >= thresh))) / 
  length(which(srwa$SRWA == FALSE))
# FNR = FN / P
eval_df$FNR[eval_df$model == "rf_01"] <- length(
  which(srwa$SRWA == TRUE & (srwa$pred_rf_01 < thresh))) / 
  length(which(srwa$SRWA == TRUE))

# Brier score
pr <- srwa$pred_rf_01 # predicted values
# observed values
ob <- as.numeric(as.logical(as.character(srwa$SRWA)))
# squared error
sq_er <- (pr - ob)^2
# mean squared error (Brier score)
eval_df$Brier[eval_df$model == "rf_01"] <- mean(sq_er) 



##### performance for rf_02
eval_df$AUC[eval_df$model == "rf_02"] <-  as.numeric(pROC::roc(
  response = srwa$SRWA, 
  predictor = srwa$pred_rf_02)$auc)

k_res <- sapply(kappas, kappa_calc, 
                resp = srwa$SRWA, 
                pred = srwa$pred_rf_02)
# return kappa and threshold that maximised kappa
eval_df$Kappa[eval_df$model == "rf_02"] <- max(k_res)
if(max(k_res) == 0) {thresh <- 0.5} else {
  thresh <- as.numeric(names(k_res[k_res == max(k_res)][1]))}

# TPR = TP / P
eval_df$TPR[eval_df$model == "rf_02"] <- length(
  which(srwa$SRWA == TRUE & (srwa$pred_rf_02 >= thresh))) / 
  length(which(srwa$SRWA == TRUE))
# TNR = TN / N
eval_df$TNR[eval_df$model == "rf_02"] <- length(
  which(srwa$SRWA == FALSE & (srwa$pred_rf_02 < thresh))) / 
  length(which(srwa$SRWA == FALSE))
# FPR = FP / N
eval_df$FPR[eval_df$model == "rf_02"] <- length(
  which(srwa$SRWA == FALSE & (srwa$pred_rf_02 >= thresh))) / 
  length(which(srwa$SRWA == FALSE))
# FNR = FN / P
eval_df$FNR[eval_df$model == "rf_02"] <- length(
  which(srwa$SRWA == TRUE & (srwa$pred_rf_02 < thresh))) / 
  length(which(srwa$SRWA == TRUE))

# Brier score
pr <- srwa$pred_rf_02 # predicted values
# observed values
ob <- as.numeric(as.logical(as.character(srwa$SRWA)))
# squared error
sq_er <- (pr - ob)^2
# mean squared error (Brier score)
eval_df$Brier[eval_df$model == "rf_02"] <- mean(sq_er) 


#### performance for rf_03
eval_df$AUC[eval_df$model == "rf_03"] <-  as.numeric(pROC::roc(
  response = srwa$SRWA, 
  predictor = srwa$pred_rf_03)$auc)

k_res <- sapply(kappas, kappa_calc, 
                resp = srwa$SRWA, 
                pred = srwa$pred_rf_03)
# return kappa and threshold that maximised kappa
eval_df$Kappa[eval_df$model == "rf_03"] <- max(k_res)
if(max(k_res) == 0) {thresh <- 0.5} else {
  thresh <- as.numeric(names(k_res[k_res == max(k_res)][1]))}

# TPR = TP / P
eval_df$TPR[eval_df$model == "rf_03"] <- length(
  which(srwa$SRWA == TRUE & (srwa$pred_rf_03 >= thresh))) / 
  length(which(srwa$SRWA == TRUE))
# TNR = TN / N
eval_df$TNR[eval_df$model == "rf_03"] <- length(
  which(srwa$SRWA == FALSE & (srwa$pred_rf_03 < thresh))) / 
  length(which(srwa$SRWA == FALSE))
# FPR = FP / N
eval_df$FPR[eval_df$model == "rf_03"] <- length(
  which(srwa$SRWA == FALSE & (srwa$pred_rf_03 >= thresh))) / 
  length(which(srwa$SRWA == FALSE))
# FNR = FN / P
eval_df$FNR[eval_df$model == "rf_03"] <- length(
  which(srwa$SRWA == TRUE & (srwa$pred_rf_03 < thresh))) / 
  length(which(srwa$SRWA == TRUE))

# Brier score
pr <- srwa$pred_rf_03 # predicted values
# observed values
ob <- as.numeric(as.logical(as.character(srwa$SRWA)))
# squared error
sq_er <- (pr - ob)^2
# mean squared error (Brier score)
eval_df$Brier[eval_df$model == "rf_03"] <- mean(sq_er) 



#### performance for rf_04
eval_df$AUC[eval_df$model == "rf_04"] <-  as.numeric(pROC::roc(
  response = srwa$SRWA, 
  predictor = srwa$pred_rf_04)$auc)


k_res <- sapply(kappas, kappa_calc, 
                resp = srwa$SRWA, 
                pred = srwa$pred_rf_04)
# return kappa and threshold that maximised kappa
eval_df$Kappa[eval_df$model == "rf_04"] <- max(k_res)
if(max(k_res) == 0) {thresh <- 0.5} else {
  thresh <- as.numeric(names(k_res[k_res == max(k_res)][1]))}

# TPR = TP / P
eval_df$TPR[eval_df$model == "rf_04"] <- length(
  which(srwa$SRWA == TRUE & (srwa$pred_rf_04 >= thresh))) / 
  length(which(srwa$SRWA == TRUE))
# TNR = TN / N
eval_df$TNR[eval_df$model == "rf_04"] <- length(
  which(srwa$SRWA == FALSE & (srwa$pred_rf_04 < thresh))) / 
  length(which(srwa$SRWA == FALSE))
# FPR = FP / N
eval_df$FPR[eval_df$model == "rf_04"] <- length(
  which(srwa$SRWA == FALSE & (srwa$pred_rf_04 >= thresh))) / 
  length(which(srwa$SRWA == FALSE))
# FNR = FN / P
eval_df$FNR[eval_df$model == "rf_04"] <- length(
  which(srwa$SRWA == TRUE & (srwa$pred_rf_04 < thresh))) / 
  length(which(srwa$SRWA == TRUE))

# Brier score
pr <- srwa$pred_rf_04 # predicted values
# observed values
ob <- as.numeric(as.logical(as.character(srwa$SRWA)))
# squared error
sq_er <- (pr - ob)^2
# mean squared error (Brier score)
eval_df$Brier[eval_df$model == "rf_04"] <- mean(sq_er) 



##### Cross validation measures -----------------------------------------------
#### performance for rf_binCV_01
eval_df$AUC[eval_df$model == "rf_binCV_01"] <-  as.numeric(pROC::roc(
  response = rf_binCV_01_testData$SRWA_in_hour, 
  predictor = rf_binCV_01_testData$pred_rf_binCV01)$auc)

k_res <- sapply(kappas, kappa_calc, 
                resp = rf_binCV_01_testData$SRWA_in_hour, 
                pred = rf_binCV_01_testData$pred_rf_binCV01)
# return kappa and threshold that maximised kappa
eval_df$Kappa[eval_df$model == "rf_binCV_01"] <- max(k_res)
if(max(k_res) == 0) {thresh <- 0.5} else {
  thresh <- as.numeric(names(k_res[k_res == max(k_res)][1]))}

# TPR = TP / P
eval_df$TPR[eval_df$model == "rf_binCV_01"] <- length(
  which(rf_binCV_01_testData$SRWA_in_hour == TRUE & 
          (rf_binCV_01_testData$pred_rf_binCV01 >= thresh))) / 
  length(which(rf_binCV_01_testData$SRWA_in_hour == TRUE))
# TNR = TN / N
eval_df$TNR[eval_df$model == "rf_binCV_01"] <- length(
  which(rf_binCV_01_testData$SRWA_in_hour == FALSE & 
          (rf_binCV_01_testData$pred_rf_binCV01 < thresh))) / 
  length(which(rf_binCV_01_testData$SRWA_in_hour == FALSE))
# FPR = FP / N
eval_df$FPR[eval_df$model == "rf_binCV_01"] <- length(
  which(rf_binCV_01_testData$SRWA_in_hour == FALSE & 
          (rf_binCV_01_testData$pred_rf_binCV01 >= thresh))) / 
  length(which(rf_binCV_01_testData$SRWA_in_hour == FALSE))
# FNR = FN / P
eval_df$FNR[eval_df$model == "rf_binCV_01"] <- length(
  which(rf_binCV_01_testData$SRWA_in_hour == TRUE & 
          (rf_binCV_01_testData$pred_rf_binCV01 < thresh))) / 
  length(which(rf_binCV_01_testData$SRWA_in_hour == TRUE))

# Brier score
pr <- rf_binCV_01_testData$pred_rf_binCV01 # predicted values
# observed values
ob <- as.numeric(as.logical(as.character(rf_binCV_01_testData$SRWA_in_hour)))
# squared error
sq_er <- (pr - ob)^2
# mean squared error (Brier score)
eval_df$Brier[eval_df$model == "rf_binCV_01"] <- mean(sq_er) 
####---------


#### performance for rf_binCV_h0
eval_df$AUC[eval_df$model == "rf_binCV_h0"] <-  as.numeric(pROC::roc(
  response = rf_binCV_h0_testData$SRWA_in_hour, 
  predictor = as.numeric(as.character(
    rf_binCV_h0_testData$pred_rf_binCVh0)))$auc)

k_res <- sapply(kappas, kappa_calc, 
                resp = rf_binCV_h0_testData$SRWA_in_hour, 
                pred = rf_binCV_h0_testData$pred_rf_binCVh0)
# return kappa and threshold that maximised kappa
eval_df$Kappa[eval_df$model == "rf_binCV_h0"] <- max(k_res)
if(max(k_res) == 0) {thresh <- 0.5} else {
  thresh <- as.numeric(names(k_res[k_res == max(k_res)][1]))}

# TPR = TP / P
eval_df$TPR[eval_df$model == "rf_binCV_h0"] <- length(
  which(rf_binCV_h0_testData$SRWA_in_hour == TRUE & 
          (rf_binCV_h0_testData$pred_rf_binCVh0 >= thresh))) / 
  length(which(rf_binCV_h0_testData$SRWA_in_hour == TRUE))
# TNR = TN / N
eval_df$TNR[eval_df$model == "rf_binCV_h0"] <- length(
  which(rf_binCV_h0_testData$SRWA_in_hour == FALSE & 
          (rf_binCV_h0_testData$pred_rf_binCVh0 < thresh))) / 
  length(which(rf_binCV_h0_testData$SRWA_in_hour == FALSE))
# FPR = FP / N
eval_df$FPR[eval_df$model == "rf_binCV_h0"] <- length(
  which(rf_binCV_h0_testData$SRWA_in_hour == FALSE & 
          (rf_binCV_h0_testData$pred_rf_binCVh0 >= thresh))) / 
  length(which(rf_binCV_h0_testData$SRWA_in_hour == FALSE))
# FNR = FN / P
eval_df$FNR[eval_df$model == "rf_binCV_h0"] <- length(
  which(rf_binCV_h0_testData$SRWA_in_hour == TRUE & 
          (rf_binCV_h0_testData$pred_rf_binCVh0 < thresh))) / 
  length(which(rf_binCV_h0_testData$SRWA_in_hour == TRUE))

# Brier score
pr <- rf_binCV_h0_testData$pred_rf_binCVh0 # predicted values
# observed values
ob <- as.numeric(as.logical(as.character(rf_binCV_h0_testData$SRWA_in_hour)))
# squared error
sq_er <- (pr - ob)^2
# mean squared error (Brier score)
eval_df$Brier[eval_df$model == "rf_binCV_h0"] <- mean(sq_er) 
####-------------------


#### performance for rf_cv01
eval_df$AUC[eval_df$model == "rf_cv01"] <-  as.numeric(pROC::roc(
  response = as.logical(as.character(rf_cv01_testData$SRWA)), 
  predictor = rf_cv01_testData$pred_rf_cv01)$auc)

k_res <- sapply(kappas, kappa_calc, 
                resp = rf_cv01_testData$SRWA, 
                pred = rf_cv01_testData$pred_rf_cv01)
# return kappa and threshold that maximised kappa
eval_df$Kappa[eval_df$model == "rf_cv01"] <- max(k_res)
if(max(k_res) == 0) {thresh <- 0.5} else {
  thresh <- as.numeric(names(k_res[k_res == max(k_res)][1]))}

# TPR = TP / P
eval_df$TPR[eval_df$model == "rf_cv01"] <- length(
  which(rf_cv01_testData$SRWA == TRUE & 
          (rf_cv01_testData$pred_rf_cv01 >= thresh))) / 
  length(which(rf_cv01_testData$SRWA == TRUE))
# TNR = TN / N
eval_df$TNR[eval_df$model == "rf_cv01"] <- length(
  which(rf_cv01_testData$SRWA == FALSE & 
          (rf_cv01_testData$pred_rf_cv01 < thresh))) / 
  length(which(rf_cv01_testData$SRWA == FALSE))
# FPR = FP / N
eval_df$FPR[eval_df$model == "rf_cv01"] <- length(
  which(rf_cv01_testData$SRWA == FALSE & 
          (rf_cv01_testData$pred_rf_cv01 >= thresh))) / 
  length(which(rf_cv01_testData$SRWA == FALSE))
# FNR = FN / P
eval_df$FNR[eval_df$model == "rf_cv01"] <- length(
  which(rf_cv01_testData$SRWA == TRUE & 
          (rf_cv01_testData$pred_rf_cv01 < thresh))) / 
  length(which(rf_cv01_testData$SRWA == TRUE))

# Brier score
pr <- rf_cv01_testData$pred_rf_cv01 # predicted values
# observed values
ob <- as.numeric(as.logical(as.character(rf_cv01_testData$SRWA)))
# squared error
sq_er <- (pr - ob)^2
# mean squared error (Brier score)
eval_df$Brier[eval_df$model == "rf_cv01"] <- mean(sq_er) 