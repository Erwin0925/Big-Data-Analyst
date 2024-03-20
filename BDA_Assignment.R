## Read the CSV file
hypertension_data <- read.csv("MSFactors.csv")

## Data inspection
head(hypertension_data)

tail(hypertension_data)

dim(hypertension_data)

str(hypertension_data)

summary(hypertension_data)

sum(is.na(hypertension_data))

# Replace the column name to a more readable name
new_column_names <- c("seqn", "age", "sex", "marital_status", "annual_income", 
               "race", "first_name", "last_name", "Waist_Circumference", "BMI", 
               "BloodPressureSys", "BloodPressureDia", "HeartRate", "VitaminD", 
               "Calcium", "Iron", "BloodGlucose", "HDL", "Trigylcerides", 
               "smoking", "Hypertension", "Dyslipidemia_HDL", "Dyslipidemia", 
               "Hyperglycemia", "Obesity", "MetabolicSyndrome")
colnames(hypertension_data) <- new_column_names


## Exploratory data analysis

# Outlier detection 
# Missing Value
# Inconsistent Value
# Noisy Data