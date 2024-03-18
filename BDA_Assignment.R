## Read the CSV file
hypertension_data <- read.csv("hypertension_data.csv")

## Data inspection
head(hypertension_data)

tail(hypertension_data)

dim(hypertension_data)

str(hypertension_data)

summary(hypertension_data)

sum(is.na(hypertension_data))

# Replace the column name to a more readable name
new_column_names <- c('Age', 'Gender', 'ChestPainType', 'RestingBP', 
                      'Cholesterol', 'FastingBS', 'RestingECG', 'MaxHeartRate', 
                      'ExerciseAngina', 'STDepression', 'STSegment', 
                      'NumMajorVessels', 'Thalassemia', 'HeartDisease')
colnames(hypertension_data) <- new_column_names


## Exploratory data analysis

# Outlier detection
# Missing Value
# Inconsistent Value
# Noisy Data