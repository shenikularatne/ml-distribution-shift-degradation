#Load all data
train_data    <- readRDS("train_data.rds")
test_baseline <- readRDS("test_baseline.rds")
test_region   <- readRDS("test_region.rds")
test_age      <- readRDS("test_age.rds")

# Combine train and baseline into one env
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

baseline_combined <- fix_factors(baseline_combined)
test_region       <- fix_factors(test_region)
test_age          <- fix_factors(test_age)


#///////////////////////////////////////////////////////////
#Chi-Square Tests — Income Distribution

#Test 1: Baseline vs Region Shift
income_table_region <- rbind(
  table(baseline_combined$income),
  table(test_region$income)
)
rownames(income_table_region) <- c("Baseline", "Region Shift")

chi_region <- chisq.test(income_table_region)

cat("\n--- Baseline vs Region Shift ---\n")
cat("Income counts:\n")
print(income_table_region)
cat(sprintf("Chi-Square Statistic : %.4f\n", chi_region$statistic))
cat(sprintf("Degrees of Freedom   : %d\n",   chi_region$parameter))
cat(sprintf("P-Value              : %.6f\n",  chi_region$p.value))
if(chi_region$p.value < 0.05) {
  cat("Result: SIGNIFICANT - Income distributions ARE different\n")
} else {
  cat("Result: NOT significant - Income distributions are similar\n")
}

# Test 2: Baseline vs Age Shift
income_table_age <- rbind(
  table(baseline_combined$income),
  table(test_age$income)
)
rownames(income_table_age) <- c("Baseline", "Age Shift")

chi_age <- chisq.test(income_table_age)

cat("\n--- Baseline vs Age Shift ---\n")
cat("Income counts:\n")
print(income_table_age)
cat(sprintf("Chi-Square Statistic : %.4f\n", chi_age$statistic))
cat(sprintf("Degrees of Freedom   : %d\n",   chi_age$parameter))
cat(sprintf("P-Value              : %.6f\n",  chi_age$p.value))
if(chi_age$p.value < 0.05) {
  cat("Result: SIGNIFICANT - Income distributions ARE different\n")
} else {
  cat("Result: NOT significant - Income distributions are similar\n")
}

#/////////////////////////////////////////////////////////////////
#T-Tests — Age

# Test 1: Baseline vs Region Shift
ttest_age_region <- t.test(baseline_combined$age,
                           test_region$age,
                           var.equal = FALSE)

cat("\n--- Baseline vs Region Shift (Age) ---\n")
cat(sprintf("Baseline Mean Age    : %.2f\n", mean(baseline_combined$age)))
cat(sprintf("Region Shift Mean Age: %.2f\n", mean(test_region$age)))
cat(sprintf("T-Statistic          : %.4f\n", ttest_age_region$statistic))
cat(sprintf("P-Value              : %.6f\n", ttest_age_region$p.value))
if(ttest_age_region$p.value < 0.05) {
  cat("Result: SIGNIFICANT - Mean ages ARE different\n")
} else {
  cat("Result: NOT significant - Mean ages are similar\n")
}

# Test 2: Baseline vs Age Shift
ttest_age_ageshift <- t.test(baseline_combined$age,
                             test_age$age,
                             var.equal = FALSE)

cat("\n--- Baseline vs Age Shift (Age) ---\n")
cat(sprintf("Baseline Mean Age    : %.2f\n", mean(baseline_combined$age)))
cat(sprintf("Age Shift Mean Age   : %.2f\n", mean(test_age$age)))
cat(sprintf("T-Statistic          : %.4f\n", ttest_age_ageshift$statistic))
cat(sprintf("P-Value              : %.6f\n", ttest_age_ageshift$p.value))
if(ttest_age_ageshift$p.value < 0.05) {
  cat("Result: SIGNIFICANT - Mean ages ARE different\n")
} else {
  cat("Result: NOT significant - Mean ages are similar\n")
}


#//////////////////////////////////////////////////////
#T-Tests — Capital Gain

# Test 1: Baseline vs Region Shift
ttest_cap_region <- t.test(baseline_combined$capital_gain,
                           test_region$capital_gain,
                           var.equal = FALSE)

cat("\n--- Baseline vs Region Shift (Capital Gain) ---\n")
cat(sprintf("Baseline Mean Cap Gain    : %.2f\n", mean(baseline_combined$capital_gain)))
cat(sprintf("Region Shift Mean Cap Gain: %.2f\n", mean(test_region$capital_gain)))
cat(sprintf("T-Statistic               : %.4f\n", ttest_cap_region$statistic))
cat(sprintf("P-Value                   : %.6f\n", ttest_cap_region$p.value))
if(ttest_cap_region$p.value < 0.05) {
  cat("Result: SIGNIFICANT - Capital gain distributions ARE different\n")
} else {
  cat("Result: NOT significant - Capital gain distributions are similar\n")
}

# Test 2: Baseline vs Age Shift
ttest_cap_age <- t.test(baseline_combined$capital_gain,
                        test_age$capital_gain,
                        var.equal = FALSE)

cat("\n--- Baseline vs Age Shift (Capital Gain) ---\n")
cat(sprintf("Baseline Mean Cap Gain : %.2f\n", mean(baseline_combined$capital_gain)))
cat(sprintf("Age Shift Mean Cap Gain: %.2f\n", mean(test_age$capital_gain)))
cat(sprintf("T-Statistic            : %.4f\n", ttest_cap_age$statistic))
cat(sprintf("P-Value                : %.6f\n", ttest_cap_age$p.value))
if(ttest_cap_age$p.value < 0.05) {
  cat("Result: SIGNIFICANT - Capital gain distributions ARE different\n")
} else {
  cat("Result: NOT significant - Capital gain distributions are similar\n")
}

#///////////////////////////////////////////////////////////
#T-Tests — Hours Per Week

# Test 1: Baseline vs Region Shift
ttest_hrs_region <- t.test(baseline_combined$hours_per_week,
                           test_region$hours_per_week,
                           var.equal = FALSE)

cat("\n--- Baseline vs Region Shift (Hours Per Week) ---\n")
cat(sprintf("Baseline Mean Hours    : %.2f\n", mean(baseline_combined$hours_per_week)))
cat(sprintf("Region Shift Mean Hours: %.2f\n", mean(test_region$hours_per_week)))
cat(sprintf("T-Statistic            : %.4f\n", ttest_hrs_region$statistic))
cat(sprintf("P-Value                : %.6f\n", ttest_hrs_region$p.value))
if(ttest_hrs_region$p.value < 0.05) {
  cat("Result: SIGNIFICANT - Hours per week ARE different\n")
} else {
  cat("Result: NOT significant - Hours per week are similar\n")
}

# Test 2: Baseline vs Age Shift
ttest_hrs_age <- t.test(baseline_combined$hours_per_week,
                        test_age$hours_per_week,
                        var.equal = FALSE)

cat("\n--- Baseline vs Age Shift (Hours Per Week) ---\n")
cat(sprintf("Baseline Mean Hours : %.2f\n", mean(baseline_combined$hours_per_week)))
cat(sprintf("Age Shift Mean Hours: %.2f\n", mean(test_age$hours_per_week)))
cat(sprintf("T-Statistic         : %.4f\n", ttest_hrs_age$statistic))
cat(sprintf("P-Value             : %.6f\n", ttest_hrs_age$p.value))
if(ttest_hrs_age$p.value < 0.05) {
  cat("Result: SIGNIFICANT - Hours per week ARE different\n")
} else {
  cat("Result: NOT significant - Hours per week are similar\n")
}

#/////////////////////////////////////////////////////
#F-Tests — Variance Comparison

# Age variance
ftest_age_region <- var.test(baseline_combined$age, test_region$age)
ftest_age_age    <- var.test(baseline_combined$age, test_age$age)

cat("\n--- Age Variance ---\n")
cat(sprintf("Baseline Age Variance    : %.4f\n", var(baseline_combined$age)))
cat(sprintf("Region Shift Age Variance: %.4f\n", var(test_region$age)))
cat(sprintf("Age Shift Age Variance   : %.4f\n", var(test_age$age)))
cat(sprintf("F-Test Baseline vs Region p-value: %.6f\n", ftest_age_region$p.value))
cat(sprintf("F-Test Baseline vs Age    p-value: %.6f\n", ftest_age_age$p.value))

# Capital gain variance
ftest_cap_region <- var.test(baseline_combined$capital_gain, test_region$capital_gain)
ftest_cap_age    <- var.test(baseline_combined$capital_gain, test_age$capital_gain)

cat("\n--- Capital Gain Variance ---\n")
cat(sprintf("Baseline Cap Gain Variance    : %.2f\n", var(baseline_combined$capital_gain)))
cat(sprintf("Region Shift Cap Gain Variance: %.2f\n", var(test_region$capital_gain)))
cat(sprintf("Age Shift Cap Gain Variance   : %.2f\n", var(test_age$capital_gain)))
cat(sprintf("F-Test Baseline vs Region p-value: %.6f\n", ftest_cap_region$p.value))
cat(sprintf("F-Test Baseline vs Age    p-value: %.6f\n", ftest_cap_age$p.value))

#//////////////////////////////////////////////////////////////
#Model Performance Significance Test

# Load predictions saved from model training script
pred_class_baseline <- readRDS("pred_class_baseline.rds")
pred_class_region   <- readRDS("pred_class_region.rds")
pred_class_age      <- readRDS("pred_class_age.rds")

# Create correct/incorrect vectors
# 1 = correct prediction, 0 = incorrect prediction
correct_baseline <- as.integer(pred_class_baseline == test_baseline$income)
correct_region   <- as.integer(pred_class_region   == test_region$income)
correct_age      <- as.integer(pred_class_age      == test_age$income)

#Two proportion z-test
# Test 1: Baseline vs Region Shift
prop_test_region <- prop.test(
  x = c(sum(correct_baseline), sum(correct_region)),
  n = c(length(correct_baseline), length(correct_region))
)

cat("\n--- Baseline vs Region Shift (Model Performance) ---\n")
cat(sprintf("Baseline Accuracy    : %.4f\n", mean(correct_baseline)))
cat(sprintf("Region Shift Accuracy: %.4f\n", mean(correct_region)))
cat(sprintf("P-Value              : %.6f\n", prop_test_region$p.value))
if(prop_test_region$p.value < 0.05) {
  cat("Result: SIGNIFICANT - Model performance IS different\n")
} else {
  cat("Result: NOT significant - Model performance is similar\n")
}

# Test 2: Baseline vs Age Shift
prop_test_age <- prop.test(
  x = c(sum(correct_baseline), sum(correct_age)),
  n = c(length(correct_baseline), length(correct_age))
)

cat("\n--- Baseline vs Age Shift (Model Performance) ---\n")
cat(sprintf("Baseline Accuracy : %.4f\n", mean(correct_baseline)))
cat(sprintf("Age Shift Accuracy: %.4f\n", mean(correct_age)))
cat(sprintf("P-Value           : %.6f\n", prop_test_age$p.value))
if(prop_test_age$p.value < 0.05) {
  cat("Result: SIGNIFICANT - Model performance IS different\n")
} else {
  cat("Result: NOT significant - Model performance is similar\n")
}

#/////////////////////////////////////////////////
#Clean Summary Table of All Tests

inferential_summary <- data.frame(
  Test              = c("Chi-Square: Income (Baseline vs Region)",
                        "Chi-Square: Income (Baseline vs Age)",
                        "T-Test: Age (Baseline vs Region)",
                        "T-Test: Age (Baseline vs Age)",
                        "T-Test: Capital Gain (Baseline vs Region)",
                        "T-Test: Capital Gain (Baseline vs Age)",
                        "T-Test: Hours/Week (Baseline vs Region)",
                        "T-Test: Hours/Week (Baseline vs Age)",
                        "Prop Test: Performance (Baseline vs Region)",
                        "Prop Test: Performance (Baseline vs Age)"),
  P_Value           = c(chi_region$p.value,
                        chi_age$p.value,
                        ttest_age_region$p.value,
                        ttest_age_ageshift$p.value,
                        ttest_cap_region$p.value,
                        ttest_cap_age$p.value,
                        ttest_hrs_region$p.value,
                        ttest_hrs_age$p.value,
                        prop_test_region$p.value,
                        prop_test_age$p.value),
  Significant       = c(chi_region$p.value    < 0.05,
                        chi_age$p.value        < 0.05,
                        ttest_age_region$p.value  < 0.05,
                        ttest_age_ageshift$p.value < 0.05,
                        ttest_cap_region$p.value  < 0.05,
                        ttest_cap_age$p.value     < 0.05,
                        ttest_hrs_region$p.value  < 0.05,
                        ttest_hrs_age$p.value     < 0.05,
                        prop_test_region$p.value  < 0.05,
                        prop_test_age$p.value     < 0.05)
)

inferential_summary


saveRDS(inferential_summary, "inferential_summary.rds")
write.csv(inferential_summary, "inferential_summary.csv", row.names = FALSE)
cat("\nInferential analytics complete!\n")
cat("Results saved successfully!\n")
