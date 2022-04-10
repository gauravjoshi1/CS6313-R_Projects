# Part 1: Draw scatter plot with the line

# Read gpa values from gpa.csv
gpa_values <- read.csv("gpa.csv")

# Get the gpa and act from the data
gpa <- gpa_values$gpa
act <- gpa_values$act

# draw a scatter plot of gpa wrt to act
plot(gpa, act, main = "Scatterplot of GPA wrt ACT",  
     xlab = "GPA", ylab = "ACT", pch = 19)

# add a straight line to the scatter plot
abline(lm(act~gpa, data = gpa_values), col = "red")

###################################################

# Part 2: Get the bootstrap estimates for percentile 

# import the boot library to calculate bootstrap statistics
library(boot)

# Get the correlation of gpa and act 
cor(gpa, act)

statistic <- function(gpa_values, i) {
  return (cor(gpa_values$gpa[i], gpa_values$act[i]))
}

# The bootstrap re-sampling takes the following parameters
# data
# statistic (user defined function)
# Number of bootstrap replications
# simulation type which is non-parametric by default
# Character string of type indices
covarience <- boot(gpa_values, statistic = statistic, 
     R = 999, sim = "ordinary", stype = "i")

# Calculate point estimate
mean(covarience$t)

# Calculate confidence interval
boot.ci(covarience, type = c("norm", "basic", "perc", "bca"))

# Verify confidence intervals using quantile
sort(covarience$t)[c(25, 975)]
