# Part 1: Draw QQ Plot and Box Plot

# read the csv file
vapor_data <- read.csv("VAPOR.csv")
par(mfrow = c(1, 2))

# Store the exp and theoretical data
exp_data <- vapor_data$experimental
theoretical_data <- vapor_data$theoretical

# Draw qq plot with line for theoretical data
qqnorm(theoretical_data, main = "Theoretical", pch = 19)
qqline(theoretical_data, col = "blue")

# Draw qq plot with line for exp data
qqnorm(exp_data, main = "Experimental", pch = 19)
qqline(exp_data, col = "red")

# Draw box-plot of theoretical and exp data
boxplot(theoretical_data, exp_data, 
        names = c("Theoretical", "Experimental"),
        main = "Box Plot of Theoretical and Experimental Data")

summary(exp_data)
summary(theoretical_data)

###################################################

# Part 2: Calculate Confidence Interval

# Calculate difference between theoretical and experimental data
diff_theoretical_exp <- theoretical_data - exp_data;

# Calculate Standard Error
standard_err <- sd(diff_theoretical_exp)/sqrt(length(diff_theoretical_exp))

# Calculate CI
output <- mean(diff_theoretical_exp) + c(-1, 1) * qt(0.975, length(diff_theoretical_exp) - 1) * standard_err

t.test(theoretical_data, exp_data, conf.level = 0.95, paired = TRUE)
