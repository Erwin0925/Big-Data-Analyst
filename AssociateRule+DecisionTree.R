#Import Library
library(arules)
library(rpart)
library(rpart.plot)
library(caret)
library(cluster)
library(ggplot2)
library(factoextra)

## Read the CSV file
data <- read.csv("MSFactors.csv")

## Select the desire column
data_selected <- data[, c("BMI", "WaistCirc", "BloodGlucose", 
                             "HDL", "Trigylcerides", "Hypertension")]

## Replace the column name to a more readable name
names(data_selected) <- c("BMI", "WaistCircumference", "BloodGlucose", 
                          "HighDensityLipoprotein", "Triglycerides", "Hypertension")

###------------------------K Mean Algo------------------------###
data_selected_km <- data_selected

# Perform PCA
pca_result <- prcomp(data_selected_km, scale. = TRUE)

# Visualize the cumulative proportion of variance explained
fviz_eig(pca_result, addlabels = TRUE, ylim = c(0, 100))

# Transform the data using the selected number of components
pca_data <- predict(pca_result, newdata = data_selected_km)


# Elbow Method
elbow_method <- function(data, max_k) {
  wcss <- numeric(max_k)
  for (i in 1:max_k) {
    kmeans_result <- kmeans(data, centers = i)
    wcss[i] <- kmeans_result$tot.withinss
  }
  plot(1:max_k, wcss, type = "b", pch = 19, xlab = "Number of Clusters (k)", ylab = "Within-Cluster Sum of Squares (WCSS)", main = "Elbow Method")
}

# Silhouette Analysis
silhouette_analysis <- function(data, max_k) {
  # Calculate distance matrix only once
  dists <- dist(data)
  
  # Prepare to store average silhouette widths
  silhouette_avg <- numeric(max_k)
  
  # Compute the average silhouette width for each number of clusters from 2 to max_k
  for (i in 2:max_k) {
    kmeans_result <- kmeans(data, centers = i)
    sil_scores <- silhouette(kmeans_result$cluster, dists)
    silhouette_avg[i] <- mean(sil_scores[, "sil_width"])
  }
  
  # Plot the silhouette scores against number of clusters
  plot(2:max_k, silhouette_avg[2:max_k], type = "b", pch = 19, xlab = "Number of Clusters (k)", ylab = "Average Silhouette Width", main = "Silhouette Analysis")
}

elbow_method(pca_data, 10)
silhouette_analysis(pca_data, 10)


kmeans_result <- kmeans(pca_data, centers = 2)

# Visualize the clustering results
fviz_cluster(kmeans_result, data = pca_data, geom = "point", stand = FALSE)

kmeans_result$centers

summary(pca_result)$rotation

aggregate(data_selected_km, by=list(cluster=kmeans_result$cluster), FUN=mean)

###------------------------Association rules Technique------------------------###
data_selected_ar <- data_selected

## Converting numerical columns to categorical 
data_selected_ar$BMI <- cut(data_selected_ar$BMI,
                breaks=c(-Inf, 18.5, 25, 30, Inf),
                labels=c("Underweight", "Normal", "Overweight", "Obese"))

data_selected_ar$WaistCircumference <- cut(data_selected_ar$WaistCircumference,
                               breaks=c(-Inf, 80, 100, Inf),
                               labels=c("Low", "Normal", "High"))

data_selected_ar$BloodGlucose <- cut(data_selected_ar$BloodGlucose,
                         breaks=c(-Inf, 70, 100, 126, Inf),
                         labels=c("Low", "Normal", "Prediabetes", "Diabetes"))

data_selected_ar$HighDensityLipoprotein <- cut(data_selected_ar$HighDensityLipoprotein,
                                   breaks=c(-Inf, 40, 60, Inf),
                                   labels=c("Low", "Normal", "High"))

data_selected_ar$Triglycerides <- cut(data_selected_ar$Triglycerides,
                          breaks=c(-Inf, 150, 200, Inf),
                          labels=c("Normal", "High", "Very High"))

data_selected_ar$Hypertension <- factor(data_selected_ar$Hypertension, levels = c(0, 1), 
                                     labels = c("No Hypertension", "Has Hypertension"))

## Filter the data
data_selected_ar <- subset(data_selected_ar, Hypertension == "Has Hypertension")

## Creating transactions
transactions <- as(data_selected_ar, "transactions")

# Generating rules
rules <- apriori(transactions, parameter = list(supp = 0.05, conf = 0.8))

significant_rules <- subset(rules, lift > 1.2)

inspect(significant_rules[1:10])


###------------------------Decision Tree Algo------------------------###
data_selected_dt <- data_selected

## Converting numerical columns to categorical (2 levels)
data_selected_dt$BMI <- cut(data_selected_dt$BMI,
                            breaks = c(-Inf, 18.5, 24.9, Inf),
                            labels = c("Abnormal", "Normal", "Abnormal"),
                            right = FALSE)


data_selected_dt$WaistCircumference <- cut(data_selected_dt$WaistCircumference,
                                           breaks = c(-Inf, 80, 94, Inf),
                                           labels = c("Normal", "Normal", "Abnormal"),
                                           right = FALSE)

data_selected_dt$BloodGlucose <- cut(data_selected_dt$BloodGlucose,
                                     breaks = c(-Inf, 70, 99, Inf),
                                     labels = c("Abnormal", "Normal", "Abnormal"),
                                     right = FALSE)

data_selected_dt$HighDensityLipoprotein <- cut(data_selected_dt$HighDensityLipoprotein,
                                               breaks = c(-Inf, 40, Inf),
                                               labels = c("Abnormal", "Normal"),
                                               right = FALSE)

data_selected_dt$Triglycerides <- cut(data_selected_dt$Triglycerides,
                                      breaks = c(-Inf, 150, Inf),
                                      labels = c("Normal", "Abnormal"),
                                      right = FALSE)

data_selected_dt$Hypertension <- factor(data_selected_dt$Hypertension, levels = c(0, 1), 
                                     labels = c("No Hypertension", "Has Hypertension"))


## Fitting the decision tree model
tree_model <- rpart(Hypertension ~ .,data = data_selected_dt, method = "class")

# Plotting the tree
rpart.plot(tree_model, main="Decision Tree for Hypertension", extra=102)

##-------Decision Tree Tuning-------##
# Trying a lower cp value
fit <- rpart(Hypertension ~ ., data = data_selected_dt, method = "class", 
             control = rpart.control(cp = 0.001, maxdepth = 5, minsplit = 10, minbucket = 5))

# Perform cross-validation to find the optimal cp
fitControl <- trainControl(method = "cv", number = 10)
dt_cv <- train(Hypertension ~ ., data = data_selected_dt, method = "rpart",
               trControl = fitControl,
               tuneGrid = expand.grid(cp = seq(0.001, 0.01, by = 0.001)))

# Plotting model accuracy across different cp values
plot(dt_cv)

# Choosing the best cp based on cross-validation
bestcp <- dt_cv$bestTune$cp
fit_pruned <- prune(fit, cp = bestcp)

# Plot the pruned tree
rpart.plot(fit_pruned, main="Adjusted Decision Tree for Hypertension", extra=102)


