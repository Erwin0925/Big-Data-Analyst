library(dplyr)
library(MASS)
library(DescTools)

# Replace implausible values with thresholds
feature_selected <- feature_selected %>%
  mutate(
    BMI = ifelse(BMI < 12 | BMI > 80, ifelse(BMI < 12, 12, 80), BMI),
    WaistCircumference = ifelse(WaistCircumference < 40 | WaistCircumference > 200, ifelse(WaistCircumference < 40, 40, 200), WaistCircumference),
    BloodGlucose = ifelse(BloodGlucose < 40 | BloodGlucose > 400, ifelse(BloodGlucose < 40, 40, 400), BloodGlucose),
    HighDensityLipoprotein = ifelse(HighDensityLipoprotein < 10 | HighDensityLipoprotein > 100, ifelse(HighDensityLipoprotein < 10, 10, 100), HighDensityLipoprotein),
    Triglycerides = ifelse(Triglycerides < 10 | Triglycerides > 3000, ifelse(Triglycerides < 10, 10, 3000), Triglycerides),
    Albuminuria = ifelse(Albuminuria < 0.1 | Albuminuria > 5000, ifelse(Albuminuria < 0.1, 0.1, 5000), Albuminuria),
    UricAcid = ifelse(UricAcid < 1 | UricAcid > 20, ifelse(UricAcid < 1, 1, 20), UricAcid)
  )

# Winsorizing method
feature_selected <- feature_selected %>%
  mutate(across(c(BMI, WaistCircumference, HighDensityLipoprotein, UricAcid), ~ Winsorize(., probs = c(0.05, 0.95))))

# Cap and Floor Method
iqr_bloodglucose <- IQR(feature_selected$BloodGlucose, na.rm = TRUE)
lower_bound_bloodglucose <- quantile(feature_selected$BloodGlucose, 0.25, na.rm = TRUE) - 1.5 * iqr_bloodglucose
upper_bound_bloodglucose <- quantile(feature_selected$BloodGlucose, 0.75, na.rm = TRUE) + 1.5 * iqr_bloodglucose
feature_selected$BloodGlucose <- pmin(pmax(feature_selected$BloodGlucose, lower_bound_bloodglucose), upper_bound_bloodglucose)

iqr_triglycerides <- IQR(feature_selected$Triglycerides, na.rm = TRUE)
lower_bound_triglycerides <- quantile(feature_selected$Triglycerides, 0.25, na.rm = TRUE) - 1.5 * iqr_triglycerides
upper_bound_triglycerides <- quantile(feature_selected$Triglycerides, 0.75, na.rm = TRUE) + 1.5 * iqr_triglycerides
feature_selected$Triglycerides <- pmin(pmax(feature_selected$Triglycerides, lower_bound_triglycerides), upper_bound_triglycerides)

# Apply random capping for values above 300
feature_selected$Albuminuria <- ifelse(feature_selected$Albuminuria > 300, 
                               floor(runif(sum(feature_selected$Albuminuria > 300), min=301, max=311)), 
                               feature_selected$Albuminuria)

# Plotting boxplots for each variable
par(mfrow=c(4,2))  # Resets the plotting area into a 3x2 grid

boxplot(feature_selected$BMI, main="Boxplot of BMI", horizontal=TRUE, col="lightblue")
boxplot(feature_selected$WaistCircumference, main="Boxplot of Waist Circumference", horizontal=TRUE, col="lightgreen")
boxplot(feature_selected$BloodGlucose, main="Boxplot of Blood Glucose", horizontal=TRUE, col="lightcoral")
boxplot(feature_selected$HighDensityLipoprotein, main="Boxplot of HDL", horizontal=TRUE, col="lightpink")
boxplot(feature_selected$Triglycerides, main="Boxplot of Triglycerides", horizontal=TRUE, col="lightyellow")
boxplot(feature_selected$Albuminuria, main="Boxplot of Albuminuria", horizontal=TRUE, col="lightcyan")
boxplot(feature_selected$UricAcid, main="Boxplot of Uric Acid", horizontal=TRUE, col="orange")


