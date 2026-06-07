
library(ISwR)
library(survival)

#-----------------------------------------------------------------------#

# Task 1 - graft.vs.host dataset

# In the graft.vs.host data set, estimate the survival function for patients 
# with or without GVHD. Test the hypothesis that the survival is the same in
# both groups. Extend the analysis by including the other explanatory variables.
data(graft.vs.host)
str(graft.vs.host)
head(graft.vs.host)

# estimate the survival function.Test the hypothesis that the survival is the same
# survival function = Kaplan-Meier

# create surv object
#Surv(time, event) 
surv_obj <- Surv(graft.vs.host$time, graft.vs.host$dead)

#survfit() for KM,eEstimate survival function by GVHD status using Kaplan-Meier
km_gvhd <- survfit(surv_obj ~ gvhd, data = graft.vs.host)

#survdiff() estimate survival function by GVHD
km_gvhd <- survfit(surv_obj ~ gvhd, data = graft.vs.host)

summary(km_gvhd)

# Plot Kaplan-Meier curves for GVHD vs no GVHD
png("figures/1/km_gvhd.png")
plot(
  km_gvhd,
  col   = c("blue", "red"),
  lty   = c(1, 2),
  main  = "Survival by GVHD Status",
  xlab  = "Time",
  ylab  = "Survival Probability",
  mark.time = TRUE
)

legend(
  "topright",
  legend = c("No GVHD", "GVHD"),
  col    = c("blue", "red"),
  lty    = c(1, 2)
)
dev.off()


#Test the hypothesis that the survival is the same in
# both groups# Log-rank test = standard test for comparing survival curves
logrank_gvhd <- survdiff(surv_obj ~ gvhd, data = graft.vs.host)

logrank_gvhd

# Extend analysis: include other explanatory variables using Cox model
# Cox proportional hazards model allows multiple predictors
cox_full <- coxph(
  surv_obj ~ gvhd + rcpage + donage + type + preg + index,
  data = graft.vs.host
)

summary(cox_full)

# Save results
sink("outputs/1/gvhd_km_results.txt")
cat("Kaplan-Meier\n\n")
print(summary(km_gvhd))
cat("\nLog-rank test:\n")
print(logrank_gvhd)
sink()

sink("outputs/1/gvhd_cox_results.txt")
cat("Cox Proportional Hazards Model - Full Model\n\n")
print(summary(cox_full))
sink()

#-----------------------------------------------------------------------#

# Task 2 - graft.vs.host dataset
# Fit Cox models to the stroke data with age and sex as predictors and with sex alone. 
# Explain the difference.

data(stroke)
str(stroke)
head(stroke)
# Create survival object
surv_stroke <- Surv(stroke$obsmonths, stroke$dead)

# Plot  Kaplan-Meier curve
png("figures/2/km_stroke.png")
plot(
  survfit(surv_stroke ~ sex, data = stroke),
  col   = c("green", "purple"),
  lty   = c(1, 2),
  main  = "Survival after Stroke by Sex",
  xlab  = "Time (months)",
  ylab  = "Survival Probability",
  mark.time = TRUE
)

legend(
  "topright",
  legend = c("Female", "Male"),
  col    = c("green", "purple"),
  lty    = c(1, 2)
)

dev.off()

# Cox model with sex alone
cox_sex <- coxph(
  surv_stroke ~ sex,
  data = stroke
)

summary(cox_sex)

# Cox model with age and sex
cox_age_sex <- coxph(
  surv_stroke ~ age + sex,
  data = stroke
)

summary(cox_age_sex)

# Save results
sink("outputs/2/stroke_cox_results.txt")

cat("Cox Model - sex  \n\n")
print(summary(cox_sex))

cat("\n\nCox Model - age + sex\n\n")
print(summary(cox_age_sex))
sink()

