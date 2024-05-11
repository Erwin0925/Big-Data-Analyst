#Import Library
library(arules)
library(rpart)
library(rpart.plot)
library(caret)
library(cluster)
library(ggplot2)
library(factoextra)
library(grid)


## Load the CSV file
data <- read.csv("MSFactors.csv")

## Select the desire column
feature_selected <- data[, c("BMI", "WaistCirc", "BloodGlucose", 
                             "HDL", "Trigylcerides","UrAlbCr","UricAcid","Hypertension")]

## Replace the column name to a more readable name
names(feature_selected) <- c("BMI", "WaistCircumference", "BloodGlucose", 
                             "HighDensityLipoprotein", "Triglycerides","Albuminuria","UricAcid","Hypertension")

## Initial look at the data structure
head(feature_selected)

tail(feature_selected)

dim(feature_selected)

str(feature_selected)

summary(feature_selected)

# Plotting histograms for each variable
par(mfrow=c(4,2))  

hist(feature_selected$BMI, main="Histogram of BMI", xlab="BMI", col="lightblue", border="black")
hist(feature_selected$WaistCircumference, main="Histogram of Waist Circumference", xlab="Waist Circumference", col="lightgreen", border="black")
hist(feature_selected$BloodGlucose, main="Histogram of Blood Glucose", xlab="Blood Glucose", col="lightcoral", border="black")
hist(feature_selected$HighDensityLipoprotein, main="Histogram of HDL", xlab="HDL", col="lightpink", border="black")
hist(feature_selected$Triglycerides, main="Histogram of Triglycerides", xlab="Triglycerides", col="lightyellow", border="black")
hist(feature_selected$Albuminuria, main="Histogram of Albuminuria", xlab="Albuminuria", col="lightcyan", border="black")
hist(feature_selected$UricAcid, main="Histogram of Uric Acid", xlab="Uric Acid", col="orange", border="black")

# Plotting boxplots for each variable
par(mfrow=c(4,2))  # Resets the plotting area into a 3x2 grid

boxplot(feature_selected$BMI, main="Boxplot of BMI", horizontal=TRUE, col="lightblue")
boxplot(feature_selected$WaistCircumference, main="Boxplot of Waist Circumference", horizontal=TRUE, col="lightgreen")
boxplot(feature_selected$BloodGlucose, main="Boxplot of Blood Glucose", horizontal=TRUE, col="lightcoral")
boxplot(feature_selected$HighDensityLipoprotein, main="Boxplot of HDL", horizontal=TRUE, col="lightpink")
boxplot(feature_selected$Triglycerides, main="Boxplot of Triglycerides", horizontal=TRUE, col="lightyellow")
boxplot(feature_selected$Albuminuria, main="Boxplot of Albuminuria", horizontal=TRUE, col="lightcyan")
boxplot(feature_selected$UricAcid, main="Boxplot of Uric Acid", horizontal=TRUE, col="orange")

# Define all combinations of plots
plot_list <- list()
variables <- c("BMI", "WaistCircumference", "BloodGlucose", "HighDensityLipoprotein", "Triglycerides","Albuminuria","UricAcid")

for (i in 1:(length(variables)-1)) {
  for (j in (i+1):length(variables)) {
    p <- ggplot(feature_selected, aes_string(x = variables[i], y = variables[j], color = "factor(Hypertension)")) +
      geom_point(alpha = 0.6) +
      labs(x = variables[i], y = variables[j], color = "Hypertension") +
      theme_minimal() +
      theme(legend.position = "none") +
      scale_color_manual(values = c("0" = "green", "1" = "red"))  # Set custom colors
    plot_list[[length(plot_list) + 1]] <- p
  }
}

grid_plot <- grid.arrange(
  grobs = plot_list,
  ncol = 5,
  nrow = 5,
  top = textGrob("Scatter Plot  by Hypertension Status", gp = gpar(fontsize = 20, fontface = "bold"))
)

print(grid_plot)


## Check for missing values
colSums(is.na(feature_selected))