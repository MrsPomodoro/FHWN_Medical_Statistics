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
result2 <-  t.test(vital_capacity ~ group,
       data = vitcap,
       conf.level = 0.99)


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
