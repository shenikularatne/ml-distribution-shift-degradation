#Load datasets 
train_data    <- readRDS("train_data.rds")
test_baseline <- readRDS("test_baseline.rds")
test_region   <- readRDS("test_region.rds")
test_age      <- readRDS("test_age.rds")

fix_factors <- function(df) {
  df$native_country <- as.factor(df$native_country)
  df$workclass      <- as.factor(df$workclass)
  df$education      <- as.factor(df$education)
  df$marital_status <- as.factor(df$marital_status)
  df$occupation     <- as.factor(df$occupation)
  df$relationship   <- as.factor(df$relationship)
  df$race           <- as.factor(df$race)
  df$sex            <- as.factor(df$sex)
  df$income         <- as.factor(df$income)
  return(df)
}

train_data    <- fix_factors(train_data)
test_baseline <- fix_factors(test_baseline)
test_region   <- fix_factors(test_region)
test_age      <- fix_factors(test_age)

library(randomForest)
library(pROC)
library(caret)

set.seed(123)

#Train random forest model
rf_model <- randomForest(income ~ .,
                         data       = train_data,
                         ntree      = 100,
                         importance = TRUE,
                         classwt    = c("<=50K" = 0.3, ">50K" = 0.7))


print(rf_model)

#Get feature importance according to the model
importance(rf_model)
varImpPlot(rf_model, main = "Feature Importance - Random Forest")

#///////////////////////////////////////////////
#get prediction on all environments

#Class predictions
pred_class_baseline <- predict(rf_model, test_baseline, type = "class")
pred_class_region   <- predict(rf_model, test_region,   type = "class")
pred_class_age      <- predict(rf_model, test_age,      type = "class")

#Prob predictions
pred_prob_baseline  <- predict(rf_model, test_baseline, type = "prob")[,2]
pred_prob_region    <- predict(rf_model, test_region,   type = "prob")[,2]
pred_prob_age       <- predict(rf_model, test_age,      type = "prob")[,2]

#Calculate AUC
roc_baseline <- roc(test_baseline$income, pred_prob_baseline, quiet = TRUE)
roc_region   <- roc(test_region$income,   pred_prob_region,   quiet = TRUE)
roc_age      <- roc(test_age$income,      pred_prob_age,      quiet = TRUE)

auc_baseline <- auc(roc_baseline)
auc_region   <- auc(roc_region)
auc_age      <- auc(roc_age)

#Calculate confusion matrices
cm_baseline <- confusionMatrix(pred_class_baseline,
                               test_baseline$income,
                               positive = ">50K")

cm_region   <- confusionMatrix(pred_class_region,
                               test_region$income,
                               positive = ">50K")

cm_age      <- confusionMatrix(pred_class_age,
                               test_age$income,
                               positive = ">50K")
print(cm_baseline)
print(cm_region)
print(cm_age)

#Get F1 score
f1_baseline <- cm_baseline$byClass["F1"]
f1_region   <- cm_region$byClass["F1"]
f1_age      <- cm_age$byClass["F1"]

#ROC Plot
plot(roc_baseline,
     col  = "blue",
     main = "ROC Curves Across Environments",
     lwd  = 2)

lines(roc_region, col = "red",   lwd = 2)
lines(roc_age,    col = "green", lwd = 2)

legend("bottomright",
       legend = c(paste("Baseline AUC =",     round(auc_baseline, 4)),
                  paste("Region Shift AUC =", round(auc_region,   4)),
                  paste("Age Shift AUC =",    round(auc_age,      4))),
       col    = c("blue", "red", "green"),
       lwd    = 2)

#Get accuracy from confusion matrix
acc_baseline <- cm_baseline$overall["Accuracy"]
acc_region   <- cm_region$overall["Accuracy"]
acc_age      <- cm_age$overall["Accuracy"]

#Result summary
performance_summary <- data.frame(
  Environment       = c("Baseline Environment",
                        "Region Shift Environment",
                        "Age Shift Environment"),
  Model             = "Random Forest",
  Accuracy          = c(acc_baseline,
                        acc_region,
                        acc_age),
  AUC               = c(as.numeric(auc_baseline),
                        as.numeric(auc_region),
                        as.numeric(auc_age)),
  Recall            = c(cm_baseline$byClass["Sensitivity"],
                        cm_region$byClass["Sensitivity"],
                        cm_age$byClass["Sensitivity"]),
  Precision         = c(cm_baseline$byClass["Pos Pred Value"],
                        cm_region$byClass["Pos Pred Value"],
                        cm_age$byClass["Pos Pred Value"]),
  F1_Score          = c(f1_baseline,
                        f1_region,
                        f1_age),
  Balanced_Accuracy = c(cm_baseline$byClass["Balanced Accuracy"],
                        cm_region$byClass["Balanced Accuracy"],
                        cm_age$byClass["Balanced Accuracy"])
)

performance_summary

#Performance drop summary
drop_acc_region <- as.numeric(acc_baseline) - as.numeric(acc_region)
drop_acc_age    <- as.numeric(acc_baseline) - as.numeric(acc_age)
drop_auc_region <- as.numeric(auc_baseline) - as.numeric(auc_region)
drop_auc_age    <- as.numeric(auc_baseline) - as.numeric(auc_age)
drop_f1_region  <- as.numeric(f1_baseline)  - as.numeric(f1_region)
drop_f1_age     <- as.numeric(f1_baseline)  - as.numeric(f1_age)
drop_bal_region <- as.numeric(cm_baseline$byClass["Balanced Accuracy"]) -
  as.numeric(cm_region$byClass["Balanced Accuracy"])
drop_bal_age    <- as.numeric(cm_baseline$byClass["Balanced Accuracy"]) -
  as.numeric(cm_age$byClass["Balanced Accuracy"])

drops_summary <- data.frame(
  Environment       = c("Region Shift vs Baseline",
                        "Age Shift vs Baseline"),
  Model             = "Random Forest",
  Accuracy_Drop     = c(drop_acc_region, drop_acc_age),
  AUC_Drop          = c(drop_auc_region, drop_auc_age),
  F1_Drop           = c(drop_f1_region,  drop_f1_age),
  Bal_Accuracy_Drop = c(drop_bal_region, drop_bal_age)
)

drops_summary

saveRDS(rf_model, "rf_model.rds")

# Save predictions
saveRDS(pred_class_baseline, "pred_class_baseline.rds")
saveRDS(pred_class_region,   "pred_class_region.rds")
saveRDS(pred_class_age,      "pred_class_age.rds")
saveRDS(pred_prob_baseline,  "pred_prob_baseline.rds")
saveRDS(pred_prob_region,    "pred_prob_region.rds")
saveRDS(pred_prob_age,       "pred_prob_age.rds")

# Save ROC objects
saveRDS(roc_baseline, "roc_baseline.rds")
saveRDS(roc_region,   "roc_region.rds")
saveRDS(roc_age,      "roc_age.rds")

# Save results tables
saveRDS(performance_summary, "performance_summary.rds")
saveRDS(drops_summary,       "drops_summary.rds")
write.csv(performance_summary, "performance_summary.csv", row.names = FALSE)
write.csv(drops_summary,       "drops_summary.csv",       row.names = FALSE)


