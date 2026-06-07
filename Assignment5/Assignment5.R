
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




data(stroke)
str(stroke)
head(stroke)