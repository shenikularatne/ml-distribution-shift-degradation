#Load data sets
train_data    <- readRDS("train_data.rds")
test_baseline <- readRDS("test_baseline.rds")
test_region   <- readRDS("test_region.rds")
test_age      <- readRDS("test_age.rds")

#install.packages("ggplot2") 
library(ggplot2)

#Env labels
train_data$environment    <- "Baseline"
test_baseline$environment <- "Baseline"
test_region$environment   <- "Region Shift"
test_age$environment      <- "Age Shift"

#train and baseline comb as it's the same env
baseline_combined <- rbind(train_data, test_baseline)

#comb all envs to one df
all_data <- rbind(baseline_combined, test_region, test_age)

all_data$environment <- factor(all_data$environment,
                               levels = c("Baseline",
                                          "Region Shift",
                                          "Age Shift"))

nrow(all_data)

#//////////////////////////////////////////////////////
#Summary Statistics Table

#sum stat function
env_summary <- function(df, env_name) {
  data.frame(
    Environment   = env_name,
    N             = nrow(df),
    Mean_Age      = round(mean(df$age),            2),
    SD_Age        = round(sd(df$age),              2),
    Mean_Hrs_Week = round(mean(df$hours_per_week), 2),
    SD_Hrs_Week   = round(sd(df$hours_per_week),   2),
    Mean_Cap_Gain = round(mean(df$capital_gain),   2),
    Pct_High_Inc  = round(mean(df$income == ">50K") * 100, 2)
  )
}

#apply sum stat function to all envs
summary_stats <- rbind(
  env_summary(baseline_combined, "Baseline"),
  env_summary(test_region,       "Region Shift"),
  env_summary(test_age,          "Age Shift")
)

summary_stats


#//////////////////////////////////////////////
#Income dist comparison
income_dist <- data.frame(
  Environment = c("Baseline", "Region Shift", "Age Shift"),
  Pct_High    = c(
    mean(baseline_combined$income == ">50K") * 100,
    mean(test_region$income        == ">50K") * 100,
    mean(test_age$income           == ">50K") * 100
  ),
  Pct_Low     = c(
    mean(baseline_combined$income == "<=50K") * 100,
    mean(test_region$income        == "<=50K") * 100,
    mean(test_age$income           == "<=50K") * 100
  )
)


income_dist

#plot income dist
ggplot(all_data, aes(x = environment, fill = income)) +
  geom_bar(position = "fill") +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_manual(values = c("<=50K" = "#3498db", ">50K" = "#e74c3c")) +
  labs(title = "Income Distribution Across Environments",
       x     = "Environment",
       y     = "Proportion",
       fill  = "Income") +
  theme_minimal()


#///////////////////////////////////////////////////////
#Age dist comparison

ggplot(all_data, aes(x = environment, y = age, fill = environment)) +
  geom_boxplot() +
  scale_fill_manual(values = c("Baseline"     = "#3498db",
                               "Region Shift" = "#e74c3c",
                               "Age Shift"    = "#2ecc71")) +
  labs(title = "Age Distribution Across Environments",
       x     = "Environment",
       y     = "Age") +
  theme_minimal() +
  theme(legend.position = "none")

#////////////////////////////////////////////////////////
#Hours Per Week Dist

ggplot(all_data, aes(x = environment, y = hours_per_week, fill = environment)) +
  geom_boxplot() +
  scale_fill_manual(values = c("Baseline"     = "#3498db",
                               "Region Shift" = "#e74c3c",
                               "Age Shift"    = "#2ecc71")) +
  labs(title = "Hours Per Week Distribution Across Environments",
       x     = "Environment",
       y     = "Hours Per Week") +
  theme_minimal() +
  theme(legend.position = "none")

#////////////////////////////////////////////////////////////
#Capital gain dist
cap_gain_nonzero <- all_data[all_data$capital_gain > 0, ]

ggplot(cap_gain_nonzero, aes(x = environment,
                             y = capital_gain,
                             fill = environment)) +
  geom_boxplot() +
  scale_fill_manual(values = c("Baseline"     = "#3498db",
                               "Region Shift" = "#e74c3c",
                               "Age Shift"    = "#2ecc71")) +
  labs(title = "Capital Gain Distribution (Non-Zero Values Only)",
       x     = "Environment",
       y     = "Capital Gain ($)") +
  theme_minimal() +
  theme(legend.position = "none")

#///////////////////////////////////////////
#Occupation Distribution

ggplot(all_data, aes(x = occupation, fill = environment)) +
  geom_bar(position = "dodge") +
  scale_fill_manual(values = c("Baseline"     = "#3498db",
                               "Region Shift" = "#e74c3c",
                               "Age Shift"    = "#2ecc71")) +
  labs(title = "Occupation Distribution Across Environments",
       x     = "Occupation",
       y     = "Count",
       fill  = "Environment") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#/////////////////////////////////////////////////////////////////
#Gender Distribution
ggplot(all_data, aes(x = environment, fill = sex)) +
  geom_bar(position = "fill") +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_manual(values = c("Male" = "#3498db", "Female" = "#e74c3c")) +
  labs(title = "Gender Distribution Across Environments",
       x     = "Environment",
       y     = "Proportion",
       fill  = "Gender") +
  theme_minimal()

#//////////////////////////////////////////////////////
#Education dist
ggplot(all_data, aes(x = education, fill = environment)) +
  geom_bar(position = "dodge") +
  scale_fill_manual(values = c("Baseline"     = "#3498db",
                               "Region Shift" = "#e74c3c",
                               "Age Shift"    = "#2ecc71")) +
  labs(title = "Education Distribution Across Environments",
       x     = "Education Level",
       y     = "Count",
       fill  = "Environment") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


saveRDS(summary_stats, "summary_stats.rds")
write.csv(summary_stats, "summary_stats.csv", row.names = FALSE)
saveRDS(all_data, "all_data.rds")

