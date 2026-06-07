library(ISwR)
#-----------------------------------------------------------------------#

# Task 1 - Zelazo dataset

#The zelazo data are in the form of a list of vectors, one for each of the four groups. 
# Convert the data to a form suitable for the use of lm and calculate the relevant test. 
# Consider t tests comparing selected subgroups or obtained by combining groups.

# Load dataset and check the structure
data(zelazo)
str(zelazo)

# The data is a list of 4 vectors (groups: active, passive, none, ctr8)
# Convert list to a data frame suitable for lm()
zelazo_df <- data.frame(
  score = unlist(zelazo),
  group = factor(rep(names(zelazo), sapply(zelazo, length)))
)

str(zelazo_df)

# scatterplot / boxplot
png("figures/1/zelazo_boxplot.png")

boxplot(
  score ~ group,
  data = zelazo_df,
  main = "Walking Age by Group (Zelazo)",
  xlab = "Group",
  ylab = "Age at First Walking (months)"
)

dev.off()

# test one-way ANOVA using lm
model_zelazo <- lm(
  score ~ group,
  data = zelazo_df
)

summary(model_zelazo)
anova(model_zelazo)

# Save results
sink("outputs/1/zelazo_lm_results.txt")
cat("One-Way ANOVA via lm() - Zelazo Data\n\n")
print(summary(model_zelazo))
cat("\nANOVA Table:\n")
print(anova(model_zelazo))
sink()

# t-tests comparing selected subgroups
# Compare active vs. passive
t_active_passive <- t.test(
  zelazo$active,
  zelazo$passive
)

# Compare active vs. none
t_active_none <- t.test(
  zelazo$active,
  zelazo$none
)

# Compare active vs. ctr8 (8-month control)
t_active_ctr8 <- t.test(
  zelazo$active,
  zelazo$ctr8
)

# Combine passive + none into one group vs active
combined_passive_none <- c(zelazo$passive, zelazo$none)

t_active_combined <- t.test(
  zelazo$active,
  combined_passive_none
)

# Save t-test results
sink("outputs/1/zelazo_ttests.txt")
cat("t-test: active vs passive\n")
print(t_active_passive)

cat("\nt-test: active vs none\n")
print(t_active_none)

cat("\nt-test: active vs ctr8\n")
print(t_active_ctr8)

cat("\nt-test: active vs combined (passive + none)\n")
print(t_active_combined)
sink()

