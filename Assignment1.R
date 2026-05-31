# 
# Assignment 1
# Barbara Klimek Tomatova
# ___________

# Task 1 - React dataset
# Do the values of the react data set (notice that this is a single vector,
# not a data frame) look reasonably normally distributed? Does the mean differ 
# significantly from zero according to a t test?


library(ISwR) #react dataset is part of ISWR package
data(react)   #load the dataset

hist(react)  #Create a histogram
qqnorm(react) #create a QQ plot to check if data follows a normal distribution
qqline(react) # add a reference line to the QQ plot


# The null hypothesis is :H0: μ = 0
# The alternative hypothesis was: H1: μ ≠ 0

t.test(react, mu = 0)

# Task 2 - React dataset
