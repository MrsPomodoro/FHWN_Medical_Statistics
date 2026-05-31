# 
# Assignment 1
# Barbara Klimek Tomatova
# ___________

library(ISwR) #all datasets are part of ISWR package


# Task 1 - React dataset
# Do the values of the react data set (notice that this is a single vector,
# not a data frame) look reasonably normally distributed? Does the mean differ 
# significantly from zero according to a t test?


data(react)   #load the dataset

#Create a histogram
png("figures/1/hist_react.png",
    width = 1000,
    height = 800)
hist(react)
dev.off()

#create a QQ plot to check if data follows a normal distribution
png("figures/1/qqplot_react.png",
    width = 1000,
    height = 800)

qqnorm(react)
qqline(react)  # add a reference line to the QQ plot

dev.off()

# The null hypothesis is :H0: μ = 0
# The alternative hypothesis was: H1: μ ≠ 0
result <- t.test(react, mu = 0)

#save this resulst of t-test into output txt 
sink("outputs/1/t_test_react.txt")
print(result)
sink()

#-----------------------------------------------------------------------#

# Task 2 - Vitcap dataset
# In the data set vitcap, use a t test to compare the vital capacity for the two groups.
# Calculate a 99% confidence interval for the difference.
# The result of this comparison may be misleading. Why?

data(vitcap)   #load the dataset
#show what the Vitcap dataset contains
names(vitcap)  #names of columns
str(vitcap) 

vital_capacity <- vitcap$vital.capacity #numeric variable
group <-  vitcap$group #grouping variable


#Calculate a 99% confidence interval for the difference.
#compute t-test 
r

#save this resulst of t-test into output txt 
sink("outputs/2/t_test_vitcap.txt")
print(result2)
sink()

# The result of this comparison may be misleading. Why?

# Calculate the mean age in each group. This helps investigate whether the groups differ in age.
mean_age_per_group <-
tapply(vitcap$age,
       vitcap$group,
       mean)
sink("outputs/2/mean_age_per_group.txt")
print(mean_age_per_group)
sink()

# Create a boxplot of age by group to get visual comparison of the age distributions.
png("figures/2/boxplot.png",
    width = 1000,
    height = 800)
boxplot(
  age ~ group,
  data = vitcap
)
dev.off()


#-----------------------------------------------------------------------#

# Task 3 -  Nonparametric techniques
# Perform the analysis of the react and vitcap data using nonparametric techniques.

# This is a nonparametric alternative to the one-sample t-test. It does not require the assumption of normality.
wilcox_react <- wilcox.test(
  react,
  mu = 0,
  exact = FALSE
)

wilcox_vitcap <- wilcox.test(
  vital_capacity ~ group,
  data = vitcap,
  exact = FALSE
)

sink("outputs/3/wilcoxon_results.txt")

print(wilcox_react)
print(wilcox_vitcap)

sink()


#-----------------------------------------------------------------------#

# Task 4 -  Graphical
# Perform graphical checks of the assumptions for a paired t test in the intake data set.

# Load dataset
data(intake)
str(intake) 
names(intake)  #names of columns

# Calculate paired differences - How much did values/measurements change?
diffs <- intake$post - intake$pre

# Histogram of differences
png("figures/4/histogram_differences.png")
hist(diffs)
dev.off()

# Q-Q plot of differences
png("figures/4/qqplot_differences.png")
qqnorm(diffs)
qqline(diffs)
dev.off()

# Boxplot of differences
png("figures/4/boxplot_differences.png")
boxplot(diffs)
dev.off()


#-----------------------------------------------------------------------#

# Task 5 -   Test of Normality
# The function shapiro.test computes a test of normality based on the defree 
# of linearity of the Q-Q plot. Apply it to the react data. Does it help to remove outliers?

# H0: The data are normally distributed.
# H1:  The data are not normally distributed.

shapiro_original <- shapiro.test(react)
# find the outliers - write down the values that R thinks are the outliers
boxplot.stats(react)$out

# exclude the outliers
outliers <- boxplot.stats(react)$out
react_no_outliers <- react[!react %in% outliers]
# test after removing outliers
shapiro_without_outliers <- shapiro.test(react_no_outliers)

# Save all results into one text file
sink("outputs/5/shapiro_results.txt")

print(shapiro_original)
print(outliers)
print(shapiro_without_outliers)

sink()