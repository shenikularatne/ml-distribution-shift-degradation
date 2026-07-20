# Load the cleaned data 
adult_clean <- readRDS("adult_clean.rds")

nrow(adult_clean)

adult_clean$native_country <- as.factor(adult_clean$native_country)
adult_clean$workclass      <- as.factor(adult_clean$workclass)
adult_clean$education      <- as.factor(adult_clean$education)
adult_clean$marital_status <- as.factor(adult_clean$marital_status)
adult_clean$occupation     <- as.factor(adult_clean$occupation)
adult_clean$relationship   <- as.factor(adult_clean$relationship)
adult_clean$race           <- as.factor(adult_clean$race)
adult_clean$sex            <- as.factor(adult_clean$sex)
adult_clean$income         <- as.factor(adult_clean$income)

print(levels(adult_clean$native_country)[1:5])

#Create the Base Group
base_data <- adult_clean[adult_clean$native_country == "United-States" &
                           adult_clean$age >= 25 &
                           adult_clean$age <= 45, ]

nrow(base_data)

#Same random split
set.seed(123)

#////////////////////////////////////////
#Base splits

#For model trainning from base 
train_index <- sample(1:nrow(base_data), size = 0.7 * nrow(base_data))
train_data <- base_data[train_index, ]

#For model testing from the base
test_baseline <- base_data[-train_index, ]

nrow(train_data)
nrow(test_baseline)

#/////////////////////////////////////
#Test env splits

#Region test split
test_region <- adult_clean[adult_clean$native_country != "United-States" &
                             adult_clean$age >= 25 &
                             adult_clean$age <= 45, ]

nrow(test_region)

#Age test split
test_age <- adult_clean[adult_clean$native_country == "United-States" &
                          adult_clean$age >= 46, ]

nrow(test_age)

#///////////////////////////////////////////
#Check Income Distribution Across All Environments

#Train data (US, Age 25-45)
print(prop.table(table(train_data$income)))

#Baseline test (US, Age 25-45)
print(prop.table(table(test_baseline$income)))

#Region shift (Non-US, Age 25-45)
print(prop.table(table(test_region$income)))

#Age shift (US, Age 46+)
print(prop.table(table(test_age$income)))

summary_table <- data.frame(
  Dataset     = c("train_data",
                  "test_baseline",
                  "test_region",
                  "test_age"),
  Description = c("US Age 25-45 (70%) - Training",
                  "US Age 25-45 (30%) - Baseline Test",
                  "Non-US Age 25-45   - Region Shift",
                  "US Age 46+         - Age Shift"),
  Rows        = c(nrow(train_data),
                  nrow(test_baseline),
                  nrow(test_region),
                  nrow(test_age))
)

summary_table


saveRDS(train_data,    "train_data.rds")
saveRDS(test_baseline, "test_baseline.rds")
saveRDS(test_region,   "test_region.rds")
saveRDS(test_age,      "test_age.rds")




