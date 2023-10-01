#####################
## Evaluate models for Saipan Reed Warbler daily vocalization patterns
## 
## 
## author: Willson Gaul  willson.gaul@gmail.com  & Ellie Roark
## created: 1 Oct 2023
## last modified: 1 Oct 2023
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
kappas <- seq(from = 0, to = 1, by = 0.1) # Thresholds to try
names(kappas) <- kappas

#### End define functions -----------------------------------------------------

## Make list of models to evaluate

# mods <- list(rf_01 = rf_01, rf_02 = rf_02, m_07 = m_07, m_08 = m_08)

eval_df <- data.frame(model = c("rf_01", "rf_02", "m_07", "m_08"), 
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



## performance for rf_02
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

