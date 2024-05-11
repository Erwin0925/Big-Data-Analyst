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





