# read temperature from data set
body_temp_rate <- read.csv("bodytemp-heartrate.csv") 

# Separate male and female data from the data sets
males <- subset(body_temp_rate, body_temp_rate$gender == 1)
females <- subset(body_temp_rate, body_temp_rate$gender == 2)

par(mfrow = c(1,2))
# Draw box plot for body temperatures of males and females
boxplot(males$body_temperature, females$body_temperature, main = "Boxplot
        of Body Temperatures", names = c('Males', 'Females'), ylab = "Temparatures")

# Draw qqnorm with qqline for male temperatures
qqnorm(males$body_temperature, main = 'Q-Q Plot for Males')
qqline(males$body_temperature)

# Draw qqnorm with qqline for female temparatures
qqnorm(females$body_temperature, main = 'Q-Q Plot for Females')
qqline(females$body_temperature)


### Confidence Interval
t.test(males$body_temperature, females$body_temperature, alternative = 'two.sided')
boxplot(males$heart_rate, females$heart_rate, main = "Boxplots of heart rates", 
        names = c('Males', 'Females'), ylab = 'Heart Rates')

qqnorm(males$heart_rate, main = "Q-Q Plot for Males")
qqline(males$heart_rate)

qqnorm(females$heart_rate, main = "Q-Q Plot for Females")
qqline(females$heart_rate)
        

male_heart <- males$heart_rate
female_heart <- females$heart_rate
t.test(male_heart, female_heart, alternative = 'two.sided')

# Find correlation values between heart rate and body temperature
cor(males$body_temperature, males$heart_rate)
cor(females$body_temperature, females$heart_rate)

plot(males$heart_rate, males$body_temperature, pch = 19, main = "Scatter Plot for Males")
abline(lm(males$body_temperature~males$heart_rate))

plot(females$heart_rate, females$body_temperature, pch = 19, main = "Scatter Plot for Females")
abline(lm(females$body_temperature~females$heart_rate))



