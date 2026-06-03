library(ISwR)
#-----------------------------------------------------------------------#

# Task 1 - RMR dataset

# With the rmr data set, plot metabolic rate versus body weight.
# Fit a linear regression model to the relation.
# According to the fitted model, what is the predicted metabolic rate 
# for a body weight of 70 kg?
# Give a 95% confidence interval for the slope of the line.

# Load dataset
data(rmr)

# Look at the structure of the dataset
str(rmr)
names(rmr)

#  #plot metabolic rate versus body weight
# Create a scatterplot - this helps visualize the relationship between them
png("figures/1/rmr_scatterplot.png")

plot(
  rmr$body.weight,
  rmr$metabolic.rate,
  main = "Metabolic Rate vs Body Weight",
  xlab = "Body Weight (kg)",
  ylab = "Metabolic Rate"
)

dev.off()


# Fit a linear regression model
# Metabolic rate is the response variable,  body weight is the predictor variable
model_rmr <- lm(
  metabolic.rate ~ body.weight,
  data = rmr
)

# View model results
summary(model_rmr)

# Save model summary into outpist
sink("outputs/1/rmr_model_rmr.txt")
cat("Linear Regression Model\n\n")
print(summary(model_rmr))
sink()

# According to the fitted model, what is the predicted metabolic rate for a person weighing 70 kg
predict_model_70_rmr <- predict(
  model_rmr,
  newdata = data.frame(body.weight = 70)
)

# Save model summary into outpist
sink("outputs/1/predict_model_70_rmr.txt")
cat("Linear Regression Model\n\n")
print(summary(model_rmr))
sink()

# Calculate a 95% confidence interval for the slope
confint(model_rmr) # this function calculate 95% confidence intervals for all model's coeficients

# Save results
sink("outputs/1/rmr_results.txt")
print(summary(model_rmr))
print(predict(
  model_rmr,
  newdata = data.frame(body.weight = 70)
))
cat("95% confidence interval: \n\n")
print(confint(model_rmr))
sink()


#-----------------------------------------------------------------------#

# Task 2 - Juul dataset

# In the juul data set, fit a linear regression model for the square root of 
# the IGF-I concentration versus age to the group of subjects over 25 years old.

# Load dataset
data(juul)

# Look at the structure of the dataset
str(juul)
names(juul)

# Keep only subjects older than 25 years
juul25 <- subset(
  juul,
  age > 25
)

# Create scatterplot
# This helps me visualize the relationship between age and IGF-I concentration
plot(
  juul25$age,
  sqrt(juul25$igf1),   #the square root of the IGF-I concentration
  main = "Square Root of IGF-I vs Age",
  xlab = "Age",
  ylab = "Square Root of IGF-I"
)

# Save scatterplot
png("figures/2/juul_scatterplot.png")

plot(
  juul25$age,
  sqrt(juul25$igf1),
  main = "Square Root of IGF-I vs Age",
  xlab = "Age",
  ylab = "Square Root of IGF-I"
)

dev.off()

# Fit linear regression model
# Square root of IGF-I is the response variable
# Age is the predictor variable
model_juul <- lm(
  sqrt(igf1) ~ age,
  data = juul25
)

# View model results
summary(model_juul)


# Save results
sink("outputs/2/juul_results.txt")
cat("Linear Regression Model\n\n")
print(summary(model_juul))
sink()


#-----------------------------------------------------------------------#

# Task 3 - Malaria dataset

# In the malaria data set, analyze the log-transformed antibody level versus age.
# Make a plot of the relation.Do you notice anything peculiar?

# Load dataset and inspect its structure
data(malaria)

str(malaria)
names(malaria)

# Create scatterplot
# Age is the predictor variable (X)
# Log-transformed antibody level is the response variable (Y)

plot(
  malaria$age,
  log(malaria$ab),
  main = "Log-transformed Antibody Level vs Age",
  xlab = "Age",
  ylab = "Log(Antibody Level)"
)

# Save figure
png("figures/3/malaria_scatterplot.png")

plot(
  malaria$age,
  log(malaria$ab),
  main = "Log-transformed Antibody Level vs Age",
  xlab = "Age",
  ylab = "Log(Antibody Level)"
)

dev.off()

# Fit linear regression model
model_malaria <- lm(
  log(ab) ~ age,
  data = malaria
)

# Add regression line
abline(model_malaria)

# View results
summary(model_malaria)

# Save results
sink("outputs/3/malaria_results.txt")

cat("Linear Regression Model\n\n")
print(summary(model_malaria))

sink()