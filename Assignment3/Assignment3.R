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
cat("One-Way ANOVA \n\n")
print(summary(model_zelazo))
cat("\n ANOVA Table:\n")
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

# save results
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

#---------------------------------------------------------------------------#

# Task 2 - Lung dataset

# In the lung data, do the three measurement methods give systematically different 
# results? If so, which ones appear to be different?

# Load and check the structure 
data(lung)
str(lung)
head(lung)

# - volume  = measurement (response variable)
# - method  = A, B, C  
# - subject = 1-6 ( repeated measures)

# Visualize differences between methods
png("figures/2/lung_boxplot.png")

boxplot(
  volume ~ method,
  data = lung,
  main = "Lung Volume by Measurement Method",
  xlab = "Method",
  ylab = "Volume"
)

dev.off()

# usinf model with subject as blocking factor
# subject must be included because the same 6 people were measured 3 times each
# without it we would treat observations as independent - which is wrong
model_lung <- lm(
  volume ~ method + subject,
  data = lung
)

summary(model_lung)
anova(model_lung)

# Extract values per method for paired t-tests
method_A <- lung$volume[lung$method == "A"]
method_B <- lung$volume[lung$method == "B"]
method_C <- lung$volume[lung$method == "C"]

# Pairwise paired t-tests to see WHICH methods differ
t_A_B <- t.test(method_A, method_B, paired = TRUE)
t_A_C <- t.test(method_A, method_C, paired = TRUE)
t_B_C <- t.test(method_B, method_C, paired = TRUE)

# Save results
sink("outputs/2/lung_results.txt")
cat("ANOVA Table:\n")
print(anova(model_lung))
cat("\nModel Summary:\n")
print(summary(model_lung))
sink()

sink("outputs/2/lung_paired_ttests.txt")
cat("A vs B\n")
print(t_A_B)
cat("\nA vs C\n")
print(t_A_C)
cat("\n B vs C\n")
print(t_B_C)
sink()

#---------------------------------------------------------------------------#
# Task 3 - Nonparametric tests for zelazo and lung
# Repeat the previous exercises using the zelazo and lung data with 
# the relevant nonparametric tests.

#Zelazo dataset

# nonparametric equivalent of one-way ANOVA is Kruskal-Wallis 
# Used because: 3+ independent groups, no assumption of normality
kruskal_zelazo <- kruskal.test(
  score ~ group,
  data = zelazo_df
)

kruskal_zelazo

#  nonparametric equivalent of independent t-test is Wilcoxon rank-sum test
# paired = FALSE because each subject is in only one group
wilcox_active_passive <- wilcox.test(zelazo$active, zelazo$passive)
wilcox_active_none    <- wilcox.test(zelazo$active, zelazo$none)
wilcox_active_ctr8    <- wilcox.test(zelazo$active, zelazo$ctr8)

# Combine passive + none
wilcox_active_combined <- wilcox.test(
  zelazo$active,
  c(zelazo$passive, zelazo$none),
  exact = FALSE 
)

#Lung dataset

# nonparametric equivalent of repeated measures ANOVA is Friedman test
# Used because: same subjects measured 3 times, no assumption of normality
# Requires data in wide format (matrix: rows = subjects, columns = methods)
lung_wide <- matrix(
  lung$volume,
  nrow  = 6,    # 6 subjects
  ncol  = 3,    # 3 methods
  byrow = TRUE  # fill row by row: subject 1 gets A,B,C; subject 2 gets A,B,C ...
)

colnames(lung_wide) <- c("A", "B", "C")
friedman_lung <- friedman.test(lung_wide)
friedman_lung

#  Wilcoxon signed-rank tests , paired = TRUE because same subjects measured by each method
wilcox_A_B <- wilcox.test(method_A, method_B, paired = TRUE)
wilcox_A_C <- wilcox.test(method_A, method_C, paired = TRUE)
wilcox_B_C <- wilcox.test(method_B, method_C, paired = TRUE)

# Save Zelazo nonparametric results
sink("outputs/3/zelazo_nonparametric.txt")
print(kruskal_zelazo)
print(wilcox_active_passive)
cat("\nWilcoxon active vs none\n")
print(wilcox_active_none)
cat("\nWilcoxon active vs ctr8\n")
print(wilcox_active_ctr8)
cat("\nWilcoxon active vs combined (passive + none)\n")
print(wilcox_active_combined)
sink()

# Save Lung nonparametric results
sink("outputs/3/lung_nonparametric.txt")
print(friedman_lung)
cat("\nPaired Wilcoxon A vs B\n")
print(wilcox_A_B)
cat("\nPaired Wilcoxon A vs C\n")
print(wilcox_A_C)
cat("\nPaired Wilcoxon B vs C\n")
print(wilcox_B_C)
sink()

