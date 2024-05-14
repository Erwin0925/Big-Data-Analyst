# Load necessary libraries
library(caret)
library(randomForest)
library(pROC)
library(e1071)

data_selected_lr<- feature_selected

# Convert target variable to factor
data_selected_lr$Hypertension <- as.factor(data_selected_lr$Hypertension)

# Set seed for reproducibility
set.seed(123)

# Split the data
trainIndex <- createDataPartition(data_selected_lr$Hypertension, p = 0.8, list = FALSE)
train_data <- data_selected_lrrf[trainIndex, ]
test_data <- data_selected_lrrf[-trainIndex, ]

###------------------------Logistic Regression Algo------------------------###
logistic_model <- train(Hypertension ~ ., data = train_data, method = "glm", family = "binomial")

# Predict on test data
logistic_pred <- predict(logistic_model, test_data)
logistic_prob <- predict(logistic_model, test_data, type = "prob")[,2]

# Confusion matrix
logistic_confusion <- confusionMatrix(logistic_pred, test_data$Hypertension)
print(logistic_confusion)

# ROC curve and AUC
logistic_roc <- roc(test_data$Hypertension, logistic_prob)
plot(logistic_roc, col = "blue", main = "ROC Curve for Logistic Regression", lwd = 2)
cat("AUC for Logistic Regression:", auc(logistic_roc), "\n")

###------------------------SVM Algo------------------------###
svm_model <- svm(Hypertension ~ ., data = train_data, probability = TRUE)

# Predict on test data
svm_pred <- predict(svm_model, test_data, probability = TRUE)
svm_prob <- attr(predict(svm_model, test_data, probability = TRUE), "probabilities")[,2]

# Confusion matrix
svm_confusion <- confusionMatrix(svm_pred, test_data$Hypertension)
print(svm_confusion)

# ROC curve and AUC
svm_roc <- roc(test_data$Hypertension, svm_prob)
plot(svm_roc, col = "green", main = "ROC Curve for SVM", lwd = 2)
cat("AUC for SVM:", auc(svm_roc), "\n")

###------------------------Random Forest Algo------------------------###
rf_model <- randomForest(Hypertension ~ ., data = train_data, ntree = 100, mtry = 3, importance = TRUE)

# Predict on test data
rf_pred <- predict(rf_model, test_data)
rf_prob <- predict(rf_model, test_data, type = "prob")[,2]

# Confusion matrix
rf_confusion <- confusionMatrix(rf_pred, test_data$Hypertension)
print(rf_confusion)

# ROC curve and AUC
rf_roc <- roc(test_data$Hypertension, rf_prob)
plot(rf_roc, col = "red", add = TRUE, lwd = 2)
cat("AUC for Random Forest:", auc(rf_roc), "\n")

##------------------Plot ROC curves for comparison------------------##
plot(logistic_roc, col = "blue", main = "ROC Curves Comparison", lwd = 2)
plot(rf_roc, col = "red", add = TRUE, lwd = 2)
plot(svm_roc, col = "green", add = TRUE, lwd = 2)
legend("bottomright", legend = c("Logistic Regression", "Random Forest", "SVM"), col = c("blue", "red", "green"), lwd = 2)

