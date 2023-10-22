#####################
## Evaluate models for Saipan Reed Warbler daily vocalization patterns
## 
## 
## author: Willson Gaul  willson.gaul@gmail.com  & Ellie Roark
## created: 1 Oct 2023
## last modified: 22 Oct 2023
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
                                "rf_binCV_01", "rf_binCV_h0", 
                                "rf_boot_bin_cv01", "rf_boot_bin_cvh0"), 
                      AUC = NA, Kappa = NA, TPR = NA, FPR = NA, TNR = NA, 
                      FNR = NA, Brier = NA)

# data frame to hold bootstrap metric estimates and confidence intervals
eval_df_ci <- tibble(model = rep(c("rf_boot_bin_cv01", "rf_boot_bin_cvh0"), 7), 
                         metric = rep(c("AUC", "Kappa", "TPR", "FPR", "TNR", 
                                    "FNR", "Brier"), 2), 
                         mean = NA, ci_upper_95 = NA, ci_lower_05 = NA) 
eval_df_ci <- eval_df_ci[order(eval_df_ci$model, eval_df_ci$metric), ]

#### Performance for bootstrap fits, chosen and null models -------------------
#### performance for rf_boot_bin_cv01
## calculate all test metrics within each bootstrap CV fold
rf_boot_bin_cv01_evals <- lapply(
  rf_boot_bin_cv01, function(x) {
    # AUC
    auc_i <- as.numeric(pROC::roc(
      response = x$preds_test_data$SRWA_in_hour,
      predictor = x$preds_test_data$pred_rf_binCV01)$auc)
    # Cohen's Kappa
    k_res <- sapply(kappas, kappa_calc,
                    resp = x$preds_test_data$SRWA_in_hour,
                    pred = x$preds_test_data$pred_rf_binCV01)
    kappa_i <- max(k_res)
    # get threshold that maximised kappa for calculating other metrics
    if(max(k_res) == 0) {thresh <- 0.5} else {
      thresh <- as.numeric(names(k_res[k_res == max(k_res)][1]))}
    # TPR = TP / P
    tpr_i <- length(
      which(x$preds_test_data$SRWA_in_hour == TRUE & 
              (x$preds_test_data$pred_rf_binCV01 >= thresh))) /
      length(which(x$preds_test_data$SRWA_in_hour == TRUE))
    # TNR = TN / N
    tnr_i <- length(
      which(x$preds_test_data$SRWA_in_hour == FALSE & 
              (x$preds_test_data$pred_rf_binCV01 < thresh))) /
      length(which(x$preds_test_data$SRWA_in_hour == FALSE))
    # FPR = FP / N
    fpr_i <- length(
      which(x$preds_test_data$SRWA_in_hour == FALSE & 
              (x$preds_test_data$pred_rf_binCV01 >= thresh))) /
      length(which(x$preds_test_data$SRWA_in_hour == FALSE))
    # FNR = FN / P
    fnr_i <- length(
      which(x$preds_test_data$SRWA_in_hour == TRUE & 
              (x$preds_test_data$pred_rf_binCV01 < thresh))) /
      length(which(x$preds_test_data$SRWA_in_hour == TRUE))
    # Brier score
    pr <- x$preds_test_data$pred_rf_binCV01 # predicted values
    # observed values
    ob <- as.numeric(as.logical(as.character(x$preds_test_data$SRWA_in_hour)))
    # squared error
    sq_er <- (pr - ob)^2
    # mean squared error (Brier score)
    Brier_i <- mean(sq_er)
    
    # return
    data.frame(AUC = auc_i, Kappa = kappa_i, thresh = thresh, TPR = tpr_i, 
         TNR = tnr_i, FPR = fpr_i, FNR = fnr_i, Brier = Brier_i)
  })

# get all metrics from all bootstrap samples into a single df
rf_boot_bin_cv01_eval_df <- bind_rows(rf_boot_bin_cv01_evals)
rf_boot_bin_cv01_eval_df <- rf_boot_bin_cv01_eval_df[, -which(colnames(
  rf_boot_bin_cv01_eval_df) == "thresh")]
rf_boot_bin_cv01_eval_df$model <- "rf_boot_bin_cv01"

rf_boot_bin_cv01_eval_df <- pivot_longer(rf_boot_bin_cv01_eval_df, 
                                         AUC:Brier) %>% 
  group_by(model, name) %>%
  summarise(mean = mean(value), lower_05_ci = quantile(value, probs = 0.05), 
            upper_95_ci = quantile(value, probs = 0.95))
####---------  end performance metrics for chosen model


#### performance for rf_boot_bin_cvh0 - Null model --------------------------
## calculate all test metrics within each bootstrap CV fold
rf_boot_bin_cvh0_evals <- lapply(
  rf_boot_bin_cvh0, function(x) {
    # AUC
    auc_i <- as.numeric(pROC::roc(
      response = x$preds_test_data$SRWA_in_hour,
      predictor = x$preds_test_data$pred_rf_binCVh0)$auc)
    # Cohen's Kappa
    k_res <- sapply(kappas, kappa_calc,
                    resp = x$preds_test_data$SRWA_in_hour,
                    pred = x$preds_test_data$pred_rf_binCVh0)
    kappa_i <- max(k_res)
    # get threshold that maximised kappa for calculating other metrics
    if(max(k_res) == 0) {thresh <- 0.5} else {
      thresh <- as.numeric(names(k_res[k_res == max(k_res)][1]))}
    # TPR = TP / P
    tpr_i <- length(
      which(x$preds_test_data$SRWA_in_hour == TRUE & 
              (x$preds_test_data$pred_rf_binCVh0 >= thresh))) /
      length(which(x$preds_test_data$SRWA_in_hour == TRUE))
    # TNR = TN / N
    tnr_i <- length(
      which(x$preds_test_data$SRWA_in_hour == FALSE & 
              (x$preds_test_data$pred_rf_binCVh0 < thresh))) /
      length(which(x$preds_test_data$SRWA_in_hour == FALSE))
    # FPR = FP / N
    fpr_i <- length(
      which(x$preds_test_data$SRWA_in_hour == FALSE & 
              (x$preds_test_data$pred_rf_binCVh0 >= thresh))) /
      length(which(x$preds_test_data$SRWA_in_hour == FALSE))
    # FNR = FN / P
    fnr_i <- length(
      which(x$preds_test_data$SRWA_in_hour == TRUE & 
              (x$preds_test_data$pred_rf_binCVh0 < thresh))) /
      length(which(x$preds_test_data$SRWA_in_hour == TRUE))
    # Brier score
    pr <- x$preds_test_data$pred_rf_binCVh0 # predicted values
    # observed values
    ob <- as.numeric(as.logical(as.character(x$preds_test_data$SRWA_in_hour)))
    # squared error
    sq_er <- (pr - ob)^2
    # mean squared error (Brier score)
    Brier_i <- mean(sq_er)
    
    # return
    data.frame(AUC = auc_i, Kappa = kappa_i, thresh = thresh, TPR = tpr_i, 
               TNR = tnr_i, FPR = fpr_i, FNR = fnr_i, Brier = Brier_i)
  })

# get all metrics from all bootstrap samples into a single df
rf_boot_bin_cvh0_eval_df <- bind_rows(rf_boot_bin_cvh0_evals)
rf_boot_bin_cvh0_eval_df <- rf_boot_bin_cvh0_eval_df[, -which(colnames(
  rf_boot_bin_cvh0_eval_df) == "thresh")]
rf_boot_bin_cvh0_eval_df$model <- "rf_boot_bin_cvh0"

rf_boot_bin_cvh0_eval_df <- pivot_longer(rf_boot_bin_cvh0_eval_df, 
                                         AUC:Brier) %>% 
  group_by(model, name) %>%
  summarise(mean = mean(value), lower_05_ci = quantile(value, probs = 0.05), 
            upper_95_ci = quantile(value, probs = 0.95))

eval_df_ci <- bind_rows(rf_boot_bin_cv01_eval_df, rf_boot_bin_cvh0_eval_df)
####------  end bootstrap performance for null model --------



# ############ Performance for non-chosen models -----------------------------
# ##### Cross validation measures -----------------------------------------------
# #### performance for rf_binCV_01
# eval_df$AUC[eval_df$model == "rf_binCV_01"] <-  as.numeric(pROC::roc(
#   response = rf_binCV_01_testData$SRWA_in_hour, 
#   predictor = rf_binCV_01_testData$pred_rf_binCV01)$auc)
# 
# k_res <- sapply(kappas, kappa_calc, 
#                 resp = rf_binCV_01_testData$SRWA_in_hour, 
#                 pred = rf_binCV_01_testData$pred_rf_binCV01)
# # return kappa and threshold that maximised kappa
# eval_df$Kappa[eval_df$model == "rf_binCV_01"] <- max(k_res)
# if(max(k_res) == 0) {thresh <- 0.5} else {
#   thresh <- as.numeric(names(k_res[k_res == max(k_res)][1]))}
# 
# # TPR = TP / P
# eval_df$TPR[eval_df$model == "rf_binCV_01"] <- length(
#   which(rf_binCV_01_testData$SRWA_in_hour == TRUE & 
#           (rf_binCV_01_testData$pred_rf_binCV01 >= thresh))) / 
#   length(which(rf_binCV_01_testData$SRWA_in_hour == TRUE))
# # TNR = TN / N
# eval_df$TNR[eval_df$model == "rf_binCV_01"] <- length(
#   which(rf_binCV_01_testData$SRWA_in_hour == FALSE & 
#           (rf_binCV_01_testData$pred_rf_binCV01 < thresh))) / 
#   length(which(rf_binCV_01_testData$SRWA_in_hour == FALSE))
# # FPR = FP / N
# eval_df$FPR[eval_df$model == "rf_binCV_01"] <- length(
#   which(rf_binCV_01_testData$SRWA_in_hour == FALSE & 
#           (rf_binCV_01_testData$pred_rf_binCV01 >= thresh))) / 
#   length(which(rf_binCV_01_testData$SRWA_in_hour == FALSE))
# # FNR = FN / P
# eval_df$FNR[eval_df$model == "rf_binCV_01"] <- length(
#   which(rf_binCV_01_testData$SRWA_in_hour == TRUE & 
#           (rf_binCV_01_testData$pred_rf_binCV01 < thresh))) / 
#   length(which(rf_binCV_01_testData$SRWA_in_hour == TRUE))
# 
# # Brier score
# pr <- rf_binCV_01_testData$pred_rf_binCV01 # predicted values
# # observed values
# ob <- as.numeric(as.logical(as.character(rf_binCV_01_testData$SRWA_in_hour)))
# # squared error
# sq_er <- (pr - ob)^2
# # mean squared error (Brier score)
# eval_df$Brier[eval_df$model == "rf_binCV_01"] <- mean(sq_er) 
# ####---------
# 
# 
# #### performance for rf_binCV_h0
# eval_df$AUC[eval_df$model == "rf_binCV_h0"] <-  as.numeric(pROC::roc(
#   response = rf_binCV_h0_testData$SRWA_in_hour, 
#   predictor = as.numeric(as.character(
#     rf_binCV_h0_testData$pred_rf_binCVh0)))$auc)
# 
# k_res <- sapply(kappas, kappa_calc, 
#                 resp = rf_binCV_h0_testData$SRWA_in_hour, 
#                 pred = rf_binCV_h0_testData$pred_rf_binCVh0)
# # return kappa and threshold that maximised kappa
# eval_df$Kappa[eval_df$model == "rf_binCV_h0"] <- max(k_res)
# if(max(k_res) == 0) {thresh <- 0.5} else {
#   thresh <- as.numeric(names(k_res[k_res == max(k_res)][1]))}
# 
# # TPR = TP / P
# eval_df$TPR[eval_df$model == "rf_binCV_h0"] <- length(
#   which(rf_binCV_h0_testData$SRWA_in_hour == TRUE & 
#           (rf_binCV_h0_testData$pred_rf_binCVh0 >= thresh))) / 
#   length(which(rf_binCV_h0_testData$SRWA_in_hour == TRUE))
# # TNR = TN / N
# eval_df$TNR[eval_df$model == "rf_binCV_h0"] <- length(
#   which(rf_binCV_h0_testData$SRWA_in_hour == FALSE & 
#           (rf_binCV_h0_testData$pred_rf_binCVh0 < thresh))) / 
#   length(which(rf_binCV_h0_testData$SRWA_in_hour == FALSE))
# # FPR = FP / N
# eval_df$FPR[eval_df$model == "rf_binCV_h0"] <- length(
#   which(rf_binCV_h0_testData$SRWA_in_hour == FALSE & 
#           (rf_binCV_h0_testData$pred_rf_binCVh0 >= thresh))) / 
#   length(which(rf_binCV_h0_testData$SRWA_in_hour == FALSE))
# # FNR = FN / P
# eval_df$FNR[eval_df$model == "rf_binCV_h0"] <- length(
#   which(rf_binCV_h0_testData$SRWA_in_hour == TRUE & 
#           (rf_binCV_h0_testData$pred_rf_binCVh0 < thresh))) / 
#   length(which(rf_binCV_h0_testData$SRWA_in_hour == TRUE))
# 
# # Brier score
# pr <- rf_binCV_h0_testData$pred_rf_binCVh0 # predicted values
# # observed values
# ob <- as.numeric(as.logical(as.character(rf_binCV_h0_testData$SRWA_in_hour)))
# # squared error
# sq_er <- (pr - ob)^2
# # mean squared error (Brier score)
# eval_df$Brier[eval_df$model == "rf_binCV_h0"] <- mean(sq_er) 
# ####-------------------
# 
# 
# #### performance for rf_cv01
# eval_df$AUC[eval_df$model == "rf_cv01"] <-  as.numeric(pROC::roc(
#   response = as.logical(as.character(rf_cv01_testData$SRWA)), 
#   predictor = rf_cv01_testData$pred_rf_cv01)$auc)
# 
# k_res <- sapply(kappas, kappa_calc, 
#                 resp = rf_cv01_testData$SRWA, 
#                 pred = rf_cv01_testData$pred_rf_cv01)
# # return kappa and threshold that maximised kappa
# eval_df$Kappa[eval_df$model == "rf_cv01"] <- max(k_res)
# if(max(k_res) == 0) {thresh <- 0.5} else {
#   thresh <- as.numeric(names(k_res[k_res == max(k_res)][1]))}
# 
# # TPR = TP / P
# eval_df$TPR[eval_df$model == "rf_cv01"] <- length(
#   which(rf_cv01_testData$SRWA == TRUE & 
#           (rf_cv01_testData$pred_rf_cv01 >= thresh))) / 
#   length(which(rf_cv01_testData$SRWA == TRUE))
# # TNR = TN / N
# eval_df$TNR[eval_df$model == "rf_cv01"] <- length(
#   which(rf_cv01_testData$SRWA == FALSE & 
#           (rf_cv01_testData$pred_rf_cv01 < thresh))) / 
#   length(which(rf_cv01_testData$SRWA == FALSE))
# # FPR = FP / N
# eval_df$FPR[eval_df$model == "rf_cv01"] <- length(
#   which(rf_cv01_testData$SRWA == FALSE & 
#           (rf_cv01_testData$pred_rf_cv01 >= thresh))) / 
#   length(which(rf_cv01_testData$SRWA == FALSE))
# # FNR = FN / P
# eval_df$FNR[eval_df$model == "rf_cv01"] <- length(
#   which(rf_cv01_testData$SRWA == TRUE & 
#           (rf_cv01_testData$pred_rf_cv01 < thresh))) / 
#   length(which(rf_cv01_testData$SRWA == TRUE))
# 
# # Brier score
# pr <- rf_cv01_testData$pred_rf_cv01 # predicted values
# # observed values
# ob <- as.numeric(as.logical(as.character(rf_cv01_testData$SRWA)))
# # squared error
# sq_er <- (pr - ob)^2
# # mean squared error (Brier score)
# eval_df$Brier[eval_df$model == "rf_cv01"] <- mean(sq_er) 
# 
# 
# 
# #### non-cross-validated modesl ---------------
# # performance for rf_01
# eval_df$AUC[eval_df$model == "rf_01"] <-  as.numeric(pROC::roc(
#   response = srwa$SRWA, 
#   predictor = srwa$pred_rf_01)$auc)
# 
# k_res <- sapply(kappas, kappa_calc, 
#                 resp = srwa$SRWA, 
#                 pred = srwa$pred_rf_01)
# # return kappa and threshold that maximised kappa
# eval_df$Kappa[eval_df$model == "rf_01"] <- max(k_res)
# if(max(k_res) == 0) {thresh <- 0.5} else {
#   thresh <- as.numeric(names(k_res[k_res == max(k_res)][1]))}
# 
# # TPR = TP / P
# eval_df$TPR[eval_df$model == "rf_01"] <- length(
#   which(srwa$SRWA == TRUE & (srwa$pred_rf_01 >= thresh))) / 
#   length(which(srwa$SRWA == TRUE))
# # TNR = TN / N
# eval_df$TNR[eval_df$model == "rf_01"] <- length(
#   which(srwa$SRWA == FALSE & (srwa$pred_rf_01 < thresh))) / 
#   length(which(srwa$SRWA == FALSE))
# # FPR = FP / N
# eval_df$FPR[eval_df$model == "rf_01"] <- length(
#   which(srwa$SRWA == FALSE & (srwa$pred_rf_01 >= thresh))) / 
#   length(which(srwa$SRWA == FALSE))
# # FNR = FN / P
# eval_df$FNR[eval_df$model == "rf_01"] <- length(
#   which(srwa$SRWA == TRUE & (srwa$pred_rf_01 < thresh))) / 
#   length(which(srwa$SRWA == TRUE))
# 
# # Brier score
# pr <- srwa$pred_rf_01 # predicted values
# # observed values
# ob <- as.numeric(as.logical(as.character(srwa$SRWA)))
# # squared error
# sq_er <- (pr - ob)^2
# # mean squared error (Brier score)
# eval_df$Brier[eval_df$model == "rf_01"] <- mean(sq_er) 
# 
# 
# 
# ##### performance for rf_02
# eval_df$AUC[eval_df$model == "rf_02"] <-  as.numeric(pROC::roc(
#   response = srwa$SRWA, 
#   predictor = srwa$pred_rf_02)$auc)
# 
# k_res <- sapply(kappas, kappa_calc, 
#                 resp = srwa$SRWA, 
#                 pred = srwa$pred_rf_02)
# # return kappa and threshold that maximised kappa
# eval_df$Kappa[eval_df$model == "rf_02"] <- max(k_res)
# if(max(k_res) == 0) {thresh <- 0.5} else {
#   thresh <- as.numeric(names(k_res[k_res == max(k_res)][1]))}
# 
# # TPR = TP / P
# eval_df$TPR[eval_df$model == "rf_02"] <- length(
#   which(srwa$SRWA == TRUE & (srwa$pred_rf_02 >= thresh))) / 
#   length(which(srwa$SRWA == TRUE))
# # TNR = TN / N
# eval_df$TNR[eval_df$model == "rf_02"] <- length(
#   which(srwa$SRWA == FALSE & (srwa$pred_rf_02 < thresh))) / 
#   length(which(srwa$SRWA == FALSE))
# # FPR = FP / N
# eval_df$FPR[eval_df$model == "rf_02"] <- length(
#   which(srwa$SRWA == FALSE & (srwa$pred_rf_02 >= thresh))) / 
#   length(which(srwa$SRWA == FALSE))
# # FNR = FN / P
# eval_df$FNR[eval_df$model == "rf_02"] <- length(
#   which(srwa$SRWA == TRUE & (srwa$pred_rf_02 < thresh))) / 
#   length(which(srwa$SRWA == TRUE))
# 
# # Brier score
# pr <- srwa$pred_rf_02 # predicted values
# # observed values
# ob <- as.numeric(as.logical(as.character(srwa$SRWA)))
# # squared error
# sq_er <- (pr - ob)^2
# # mean squared error (Brier score)
# eval_df$Brier[eval_df$model == "rf_02"] <- mean(sq_er) 
# 
# 
# #### performance for rf_03
# eval_df$AUC[eval_df$model == "rf_03"] <-  as.numeric(pROC::roc(
#   response = srwa$SRWA, 
#   predictor = srwa$pred_rf_03)$auc)
# 
# k_res <- sapply(kappas, kappa_calc, 
#                 resp = srwa$SRWA, 
#                 pred = srwa$pred_rf_03)
# # return kappa and threshold that maximised kappa
# eval_df$Kappa[eval_df$model == "rf_03"] <- max(k_res)
# if(max(k_res) == 0) {thresh <- 0.5} else {
#   thresh <- as.numeric(names(k_res[k_res == max(k_res)][1]))}
# 
# # TPR = TP / P
# eval_df$TPR[eval_df$model == "rf_03"] <- length(
#   which(srwa$SRWA == TRUE & (srwa$pred_rf_03 >= thresh))) / 
#   length(which(srwa$SRWA == TRUE))
# # TNR = TN / N
# eval_df$TNR[eval_df$model == "rf_03"] <- length(
#   which(srwa$SRWA == FALSE & (srwa$pred_rf_03 < thresh))) / 
#   length(which(srwa$SRWA == FALSE))
# # FPR = FP / N
# eval_df$FPR[eval_df$model == "rf_03"] <- length(
#   which(srwa$SRWA == FALSE & (srwa$pred_rf_03 >= thresh))) / 
#   length(which(srwa$SRWA == FALSE))
# # FNR = FN / P
# eval_df$FNR[eval_df$model == "rf_03"] <- length(
#   which(srwa$SRWA == TRUE & (srwa$pred_rf_03 < thresh))) / 
#   length(which(srwa$SRWA == TRUE))
# 
# # Brier score
# pr <- srwa$pred_rf_03 # predicted values
# # observed values
# ob <- as.numeric(as.logical(as.character(srwa$SRWA)))
# # squared error
# sq_er <- (pr - ob)^2
# # mean squared error (Brier score)
# eval_df$Brier[eval_df$model == "rf_03"] <- mean(sq_er) 
# 
# 
# 
# #### performance for rf_04
# eval_df$AUC[eval_df$model == "rf_04"] <-  as.numeric(pROC::roc(
#   response = srwa$SRWA, 
#   predictor = srwa$pred_rf_04)$auc)
# 
# 
# k_res <- sapply(kappas, kappa_calc, 
#                 resp = srwa$SRWA, 
#                 pred = srwa$pred_rf_04)
# # return kappa and threshold that maximised kappa
# eval_df$Kappa[eval_df$model == "rf_04"] <- max(k_res)
# if(max(k_res) == 0) {thresh <- 0.5} else {
#   thresh <- as.numeric(names(k_res[k_res == max(k_res)][1]))}
# 
# # TPR = TP / P
# eval_df$TPR[eval_df$model == "rf_04"] <- length(
#   which(srwa$SRWA == TRUE & (srwa$pred_rf_04 >= thresh))) / 
#   length(which(srwa$SRWA == TRUE))
# # TNR = TN / N
# eval_df$TNR[eval_df$model == "rf_04"] <- length(
#   which(srwa$SRWA == FALSE & (srwa$pred_rf_04 < thresh))) / 
#   length(which(srwa$SRWA == FALSE))
# # FPR = FP / N
# eval_df$FPR[eval_df$model == "rf_04"] <- length(
#   which(srwa$SRWA == FALSE & (srwa$pred_rf_04 >= thresh))) / 
#   length(which(srwa$SRWA == FALSE))
# # FNR = FN / P
# eval_df$FNR[eval_df$model == "rf_04"] <- length(
#   which(srwa$SRWA == TRUE & (srwa$pred_rf_04 < thresh))) / 
#   length(which(srwa$SRWA == TRUE))
# 
# # Brier score
# pr <- srwa$pred_rf_04 # predicted values
# # observed values
# ob <- as.numeric(as.logical(as.character(srwa$SRWA)))
# # squared error
# sq_er <- (pr - ob)^2
# # mean squared error (Brier score)
# eval_df$Brier[eval_df$model == "rf_04"] <- mean(sq_er) 
# 
# 
# eval_df
