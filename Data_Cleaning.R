getwd()

#Load dataset
adult_data <- read.csv("adult.data", 
                       header = FALSE, 
                       na.strings = " ?",
                       strip.white = TRUE)

#Add col names
colnames(adult_data) <- c("age", "workclass", "fnlwgt", "education", 
                          "education_num", "marital_status", "occupation", 
                          "relationship", "race", "sex", 
                          "capital_gain", "capital_loss", 
                          "hours_per_week", "native_country", "income")

head(adult_data)

#////////////////////////////////////////////////////////////////////////////////

#Dataset Preprocessing
str(adult_data)

#Check missing values
colSums(is.na(adult_data))


adult_clean <- na.omit(adult_data)

# Check for empty strings hiding as missing values
colSums(adult_clean == "" | adult_clean == " ", na.rm = TRUE)

# Check unique values in columns that commonly have missing data
table(adult_clean$workclass)
table(adult_clean$occupation)
table(adult_clean$native_country)

#fix the"?" problem
adult_clean[adult_clean == "?"] <- NA
adult_clean <- na.omit(adult_clean)
adult_clean <- droplevels(adult_clean)

table(adult_clean$workclass)
table(adult_clean$occupation)
table(adult_clean$native_country)

cat("Rows after removing ? values:", nrow(adult_clean), "\n")

#remove useless columns
adult_clean <- adult_clean[ , !names(adult_clean) %in% c("fnlwgt", "education_num")]

#Remaining columns
print(names(adult_clean))

#convert into factors
adult_clean$native_country <- as.factor(adult_clean$native_country)
adult_clean$workclass      <- as.factor(adult_clean$workclass)
adult_clean$education      <- as.factor(adult_clean$education)
adult_clean$marital_status <- as.factor(adult_clean$marital_status)
adult_clean$occupation     <- as.factor(adult_clean$occupation)
adult_clean$relationship   <- as.factor(adult_clean$relationship)
adult_clean$race           <- as.factor(adult_clean$race)
adult_clean$sex            <- as.factor(adult_clean$sex)
adult_clean$income         <- as.factor(adult_clean$income)

saveRDS(adult_clean, "adult_clean.rds")
write.csv(adult_clean, "adult_clean.csv", row.names = FALSE)


