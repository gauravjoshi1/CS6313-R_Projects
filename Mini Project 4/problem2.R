# Part 1: Draw a Box Plot

# read csv file
voltage_data <- read.csv("VOLTAGE.csv")

# Store the information about remote and local voltage
remote_voltage <- voltage_data$voltage[which(voltage_data$location == 0)]
local_voltage <- voltage_data$voltage[which(voltage_data$location == 1)]

# Summary of remote and local voltage
summary(remote_voltage)
summary(local_voltage)

# Draw a Box Plot of Local and Remote Voltage Values
boxplot(remote_voltage, local_voltage, names = c("Remote Location", "Local Location"),
        main = "Voltage Values at Remote and Local Locations", range = 1.5)

###################################################
# Part 2: Draw QQPlot
par(mfrow = c(1, 2))

qqnorm(remote_voltage, main = "remote")
qqline(remote_voltage)

qqnorm(local_voltage, main = "local")
qqline(local_voltage)


###################################################

# Part 3: Calculate Confidence Interval

# Step 1: Get the variance of remote and local voltage data
var_remote <- var(remote_voltage)
var_local <- var(local_voltage)

# Step 2: Use the variance to calculate Standard Error
standard_err <- sqrt(var_local/length(local_voltage) + var_remote/length(remote_voltage))

# Step 3: Calculate difference of mean
# Using qnorm, create confidence interval
diff_mean <- mean(remote_voltage) - mean(local_voltage)
output <- diff_mean + c(-1, 1) * qnorm(0.975) * standard_err

t.test(remote_voltage, local_voltage, conf.level = 0.95)