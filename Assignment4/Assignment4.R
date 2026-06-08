library(ISwR)

# Task 1 - tb.dilute data

# Perform a two-way analysis of variance on the tb.dilute data. Modify the model
# to have a dose effect that is linear in log dose. Compute a confidence interval 
# for the slope. An alternative approach could be to calculate a slope for each
# animal and perform a test based on them. Compute a confidence interval for 
# the mean slope, and compare it with the preceding result.

data(tb.dilute)
str(tb.dilute)
head(tb.dilute)

# Variables:
# reaction = response variable  
# animal   = blocking factor (6 animals, repeated measures)
# logdose  =factor with 3 levels: -0.5, 0, 0.5

# --- Two-way ANOVA ---
# Perform a two-way analysis of variance on the tb.dilute data.
# animal is a blocking factor (same animals measured at each dose)
# logdose is the treatment factor
model_anova <- lm(
  reaction ~ animal + logdose,
  data = tb.dilute
)

summary(model_anova)
anova(model_anova)

sink("outputs/1/tb_anova_results.txt")
cat("Two-Way ANOVA - tb.dilute\n\n")
cat("ANOVA Table:\n")
print(anova(model_anova))
cat("\nModel Summary:\n")
print(summary(model_anova))
sink()

# Modify the model to have a dose effect that is linear in log dose.
# logdose is a factor — convert to numeric to allow fitting a linear trend
tb.dilute$logdose_num <- as.numeric(as.character(tb.dilute$logdose))

model_linear <- lm(
  reaction ~ animal + logdose_num,
  data = tb.dilute
)

summary(model_linear)
anova(model_linear)

# Compute a confidence interval for the slope.
ci_slope <- confint(model_linear)
ci_slope

sink("outputs/1/tb_linear_results.txt")
cat("Linear Dose Effect Model\n\n")
cat("ANOVA Table:\n")
print(anova(model_linear))
cat("\nModel Summary:\n")
print(summary(model_linear))
cat("\n95% Confidence Interval for the slope:\n")
print(ci_slope)
sink()

# An alternative approach: calculate a slope for each animal
# and perform a test based on them.

# fit a separate regression for each animal and extract the slope
slopes <- sapply(levels(tb.dilute$animal), function(a) {
  animal_data <- subset(tb.dilute, animal == a)
  coef(lm(reaction ~ logdose_num, data = animal_data))[2]
})

slopes

# one-sample t-test / check if is the mean slope significantly different from zero?
t_slopes <- t.test(slopes)
t_slopes

# Compute a confidence interval for the mean slope,
# and compare it with the preceding result.

ci_mean_slope <- t_slopes$conf.int
ci_mean_slope

# compare:
cat("confidence interval from linear model:  "); print(ci_slope["logdose_num", ])
cat("confidence interval from mean slope:    "); print(ci_mean_slope)

sink("outputs/1/tb_slopes_results.txt")
cat("Individual Slopes per Animal:\n\n")
print(slopes)
cat("\n One-sample t-test on slopes:\n")
print(t_slopes)
cat("\n 95% CI for mean slope:\n")
print(ci_mean_slope)
sink()

# Plot - reaction vs logdose  - for each animal to visualize linear trend
png("figures/1/tb_reaction_logdose.png")

plot(
  tb.dilute$logdose_num,
  tb.dilute$reaction,
  col  = as.integer(tb.dilute$animal),
  pch  = 16,
  main = "Reaction vs Log Dose by Animal",
  xlab = "Log Dose",
  ylab = "Reaction"
)

for (i in 1:6) {
  animal_data <- subset(tb.dilute, animal == i)
  abline(lm(reaction ~ logdose_num, data = animal_data), col = i)
}


dev.off()

