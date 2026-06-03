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