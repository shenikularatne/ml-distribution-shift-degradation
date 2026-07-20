#Load datasets
train_data          <- readRDS("train_data.rds")
test_baseline       <- readRDS("test_baseline.rds")
test_region         <- readRDS("test_region.rds")
test_age            <- readRDS("test_age.rds")
performance_summary <- readRDS("performance_summary.rds")
roc_baseline        <- readRDS("roc_baseline.rds")
roc_region          <- readRDS("roc_region.rds")
roc_age             <- readRDS("roc_age.rds")

library(pROC)
library(randomForest)
library(caret)

#combine train and baseline 
baseline_combined <- rbind(train_data, test_baseline)

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

train_data <- fix_factors(train_data)
baseline_combined <- fix_factors(baseline_combined)
test_region       <- fix_factors(test_region)
test_age          <- fix_factors(test_age)

#////////////////////////////////////////////////////////
#Calculate Distribution Shift Metrics(Independant vars)

#Age shift metric
age_shift_region <- abs(mean(baseline_combined$age) - mean(test_region$age))
age_shift_age    <- abs(mean(baseline_combined$age) - mean(test_age$age))

#Capital gain shift metric
cap_shift_region <- abs(mean(baseline_combined$capital_gain) -
                          mean(test_region$capital_gain))
cap_shift_age    <- abs(mean(baseline_combined$capital_gain) -
                          mean(test_age$capital_gain))

#Hours per week shift metric
hrs_shift_region <- abs(mean(baseline_combined$hours_per_week) -
                          mean(test_region$hours_per_week))
hrs_shift_age    <- abs(mean(baseline_combined$hours_per_week) -
                          mean(test_age$hours_per_week))

# Income ratio shift metric
inc_shift_region <- abs(mean(baseline_combined$income == ">50K") -
                          mean(test_region$income == ">50K"))
inc_shift_age    <- abs(mean(baseline_combined$income == ">50K") -
                          mean(test_age$income == ">50K"))

shift_metrics <- data.frame(
  Environment    = c("Region Shift", "Age Shift"),
  Age_Shift      = c(age_shift_region, age_shift_age),
  CapGain_Shift  = c(cap_shift_region, cap_shift_age),
  Hours_Shift    = c(hrs_shift_region, hrs_shift_age),
  Income_Shift   = c(inc_shift_region, inc_shift_age)
)

shift_metrics

#/////////////////////////////////////////////////////
#Calc performance drops(Dependent vars)

# Extract AUC values
auc_baseline <- as.numeric(auc(roc_baseline))
auc_region   <- as.numeric(auc(roc_region))
auc_age      <- as.numeric(auc(roc_age))

# Extract Balanced Accuracy from performance summary
bal_acc_baseline <- performance_summary$Balanced_Accuracy[1]
bal_acc_region   <- performance_summary$Balanced_Accuracy[2]
bal_acc_age      <- performance_summary$Balanced_Accuracy[3]

# Extract F1 scores
f1_baseline <- performance_summary$F1_Score[1]
f1_region   <- performance_summary$F1_Score[2]
f1_age      <- performance_summary$F1_Score[3]

# Calculate drops(+> degrade / ->impr)
auc_drop_region <- auc_baseline - auc_region
auc_drop_age    <- auc_baseline - auc_age
bal_drop_region <- bal_acc_baseline - bal_acc_region
bal_drop_age    <- bal_acc_baseline - bal_acc_age
f1_drop_region  <- f1_baseline - f1_region
f1_drop_age     <- f1_baseline - f1_age


drop_metrics <- data.frame(
  Environment   = c("Region Shift", "Age Shift"),
  AUC_Drop      = c(auc_drop_region, auc_drop_age),
  BalAcc_Drop   = c(bal_drop_region, bal_drop_age),
  F1_Drop       = c(f1_drop_region,  f1_drop_age)
)

drop_metrics

#///////////////////////////////////////////////////////
#Build Regression dataset

set.seed(123)

create_subgroups <- function(df, n_groups, env_name) {
  df$subgroup <- sample(1:n_groups, nrow(df), replace = TRUE)
  return(df)
}

# Use 20 subgroups for more data points and variation
region_sub <- create_subgroups(test_region, 20, "Region")
age_sub    <- create_subgroups(test_age,    20, "Age")

# Create additional sub-environments from baseline data
# Young workers (Age 25-30) — close to baseline
test_young <- baseline_combined[baseline_combined$age >= 25 &
                                  baseline_combined$age <= 30, ]

# Mature workers (Age 40-45) — slightly different from baseline
test_mature <- baseline_combined[baseline_combined$age >= 40 &
                                   baseline_combined$age <= 45, ]

# Workers with zero capital gain — different from average baseline
test_no_cap <- baseline_combined[baseline_combined$capital_gain == 0, ]

# Workers with high capital gain — very different from baseline
test_high_cap <- baseline_combined[baseline_combined$capital_gain >
                                     quantile(baseline_combined$capital_gain,
                                              0.75), ]

#Create subgroups for each additional environment
young_sub    <- create_subgroups(test_young,    10, "Young")
mature_sub   <- create_subgroups(test_mature,   10, "Mature")
no_cap_sub   <- create_subgroups(test_no_cap,   10, "NoCap")
high_cap_sub <- create_subgroups(test_high_cap, 10, "HighCap")



# Function to calculate metrics for each subgroup
calc_subgroup_metrics <- function(df, env_name, baseline_df) {
  
  results <- data.frame()
  
  for(g in unique(df$subgroup)) {
    
    sub <- df[df$subgroup == g, ]
    sub <- fix_factors(sub)
    
    # Align factor levels with training data
    for(col in names(sub)) {
      if(is.factor(sub[[col]]) && is.factor(train_data[[col]])) {
        sub[[col]] <- factor(sub[[col]],
                             levels = levels(train_data[[col]]))
      }
    }
    
    # Calculate distribution shifts from baseline
    age_s <- abs(mean(baseline_df$age) - mean(sub$age))
    cap_s <- abs(mean(baseline_df$capital_gain) -
                   mean(sub$capital_gain))
    hrs_s <- abs(mean(baseline_df$hours_per_week) -
                   mean(sub$hours_per_week))
    inc_s <- abs(mean(baseline_df$income == ">50K") -
                   mean(sub$income == ">50K"))
    
    # Get predictions
    pred_class <- predict(rf_model, sub, type = "class")
    pred_prob  <- predict(rf_model, sub, type = "prob")[,2]
    
    # Calculate AUC
    roc_sub <- roc(sub$income, pred_prob, quiet = TRUE)
    auc_sub <- as.numeric(auc(roc_sub))
    
    # Calculate Balanced Accuracy
    cm_sub <- table(Predicted = pred_class, Actual = sub$income)
    
    if(all(c("<=50K", ">50K") %in% rownames(cm_sub)) &&
       all(c("<=50K", ">50K") %in% colnames(cm_sub))) {
      sens    <- cm_sub[">50K",  ">50K"]  / sum(cm_sub[, ">50K"])
      spec    <- cm_sub["<=50K", "<=50K"] / sum(cm_sub[, "<=50K"])
      bal_acc <- (sens + spec) / 2
    } else {
      bal_acc <- NA
    }
    
    auc_drop     <- auc_baseline - auc_sub
    bal_acc_drop <- bal_acc_baseline - bal_acc
    
    row <- data.frame(
      Environment   = env_name,
      Subgroup      = g,
      Age_Shift     = age_s,
      CapGain_Shift = cap_s,
      Hours_Shift   = hrs_s,
      Income_Shift  = inc_s,
      AUC_Drop      = auc_drop,
      BalAcc_Drop   = bal_acc_drop
    )
    
    results <- rbind(results, row)
  }
  return(results)
}

# Calculate metrics for all environments

region_metrics   <- calc_subgroup_metrics(region_sub,   "Region", baseline_combined)
age_metrics      <- calc_subgroup_metrics(age_sub,      "Age",    baseline_combined)
young_metrics    <- calc_subgroup_metrics(young_sub,    "Young",  baseline_combined)
mature_metrics   <- calc_subgroup_metrics(mature_sub,   "Mature", baseline_combined)
no_cap_metrics   <- calc_subgroup_metrics(no_cap_sub,   "NoCap",  baseline_combined)
high_cap_metrics <- calc_subgroup_metrics(high_cap_sub, "HighCap",baseline_combined)

# Combine all results
regression_data <- rbind(region_metrics,
                         age_metrics,
                         young_metrics,
                         mature_metrics,
                         no_cap_metrics,
                         high_cap_metrics)

print(regression_data)

#/////////////////////////////////////////////////////////////////////////////////
#Build Multiple Linear Regression Models

regression_data_clean <- na.omit(regression_data)

# Model 1: Predicting AUC Drop
lm_auc <- lm(AUC_Drop ~ Age_Shift + CapGain_Shift +
               Hours_Shift + Income_Shift,
             data = regression_data_clean)

summary(lm_auc)

# Model 2: Predicting Balanced Accuracy Drop
lm_bal <- lm(BalAcc_Drop ~ Age_Shift + CapGain_Shift +
               Hours_Shift + Income_Shift,
             data = regression_data_clean)

summary(lm_bal)

#///////////////////////////////////////////////////////////////////////////////
#Regression Diagnostics

par(mfrow = c(2, 2))
plot(lm_auc, main = "Regression Diagnostics - AUC Drop Model")
par(mfrow = c(1, 1))

par(mfrow = c(2, 2))
plot(lm_bal, main = "Regression Diagnostics - Balanced Accuracy Drop Model")
par(mfrow = c(1, 1))

#Regression Results Table

regression_results <- data.frame(
  Model         = c("AUC Drop Model",
                    "Balanced Accuracy Drop Model"),
  R_Squared     = c(summary(lm_auc)$r.squared,
                    summary(lm_bal)$r.squared),
  Adj_R_Squared = c(summary(lm_auc)$adj.r.squared,
                    summary(lm_bal)$adj.r.squared),
  F_Statistic   = c(summary(lm_auc)$fstatistic[1],
                    summary(lm_bal)$fstatistic[1]),
  P_Value       = c(pf(summary(lm_auc)$fstatistic[1],
                       summary(lm_auc)$fstatistic[2],
                       summary(lm_auc)$fstatistic[3],
                       lower.tail = FALSE),
                    pf(summary(lm_bal)$fstatistic[1],
                       summary(lm_bal)$fstatistic[2],
                       summary(lm_bal)$fstatistic[3],
                       lower.tail = FALSE))
)

regression_results

#//////////////////////////////////////////////////////////
#Regression Results visuals
library(ggplot2)

# Plot 1: Income Shift vs AUC Drop
ggplot(regression_data_clean,
       aes(x = Income_Shift, y = AUC_Drop, color = Environment)) +
  geom_point(size = 3) +
  geom_smooth(method = "lm", se = TRUE, color = "black") +
  labs(title = "Income Distribution Shift vs AUC Drop",
       x     = "Income Distribution Shift",
       y     = "AUC Drop from Baseline") +
  theme_minimal()

# Plot 2: Capital Gain Shift vs AUC Drop
ggplot(regression_data_clean,
       aes(x = CapGain_Shift, y = AUC_Drop, color = Environment)) +
  geom_point(size = 3) +
  geom_smooth(method = "lm", se = TRUE, color = "black") +
  labs(title = "Capital Gain Shift vs AUC Drop",
       x     = "Capital Gain Distribution Shift",
       y     = "AUC Drop from Baseline") +
  theme_minimal()

# Plot 3: Age Shift vs Balanced Accuracy Drop
ggplot(regression_data_clean,
       aes(x = Age_Shift, y = BalAcc_Drop, color = Environment)) +
  geom_point(size = 3) +
  geom_smooth(method = "lm", se = TRUE, color = "black") +
  labs(title = "Age Shift vs Balanced Accuracy Drop",
       x     = "Age Distribution Shift",
       y     = "Balanced Accuracy Drop from Baseline") +
  theme_minimal()


saveRDS(regression_data,    "regression_data.rds")
saveRDS(regression_results, "regression_results.rds")
saveRDS(lm_auc,             "lm_auc.rds")
saveRDS(lm_bal,             "lm_bal.rds")
write.csv(regression_data,    "regression_data.csv",    row.names = FALSE)
write.csv(regression_results, "regression_results.csv", row.names = FALSE)


#/////////////////////////////////////////////////////////////////////////////
#coefficiancy
summary(lm_auc)$coefficients
summary(lm_bal)$coefficients

# Coefficient importance plot for AUC model
coef_df <- data.frame(
  Variable    = c("Age Shift", "CapGain Shift",
                  "Hours Shift", "Income Shift"),
  Estimate    = c(coef(lm_auc)[2], coef(lm_auc)[3],
                  coef(lm_auc)[4], coef(lm_auc)[5]),
  Significant = c(TRUE, FALSE, TRUE, TRUE)
)

ggplot(coef_df, aes(x = reorder(Variable, abs(Estimate)),
                    y = Estimate,
                    fill = Significant)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = c("TRUE"  = "#e74c3c",
                               "FALSE" = "#95a5a6")) +
  coord_flip() +
  labs(title = "Regression Coefficients - AUC Drop Model",
       x     = "Variable",
       y     = "Coefficient Estimate",
       fill  = "Significant (p<0.05)") +
  theme_minimal()



