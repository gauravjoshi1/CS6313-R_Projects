### Question 1a

# Read the data from csv file
road_race_data <- read.csv('roadrace.csv')

# Store the date associated with Maine in a table
road_race_table <- table(road_race_data$Maine)

# Plot the data as bar graph
barplot(road_race_table, main = "Road Race Data for Maine", ylab = "Race Participants", col = c('red', 'blue'))

# More Information about the runners
number_of_Away_runners <- road_race_table[1]
number_of_Maine_runners <- road_race_table[2]


### Question 1b

# Store details of racers from Maine
road_racers_from_maine <- subset(road_race_data, road_race_data$Maine == 'Maine')
maine_racers_time <- road_racers_from_maine$Time..minutes.

# Store details of racers not from Maine
road_racers_from_away <- subset(road_race_data, road_race_data$Maine == 'Away')
away_racers_time <- road_racers_from_Away$Time..minutes.

# Relevant information about Maine Runners
mean(maine_racers_time)
median(maine_racers_time)
range(maine_racers_time)
sd(maine_racers_time)
IQR(maine_racers_time)

# Relevant Information about Away Runners
mean(away_racers_time)
median(away_racers_time)
range(away_racers_time)
sd(away_racers_time)
IQR(away_racers_time)

# General Summary
summary(maine_racers_time)
summary(away_racers_time)

# Plot Histogram of the two groups based on Time in Minutes
hist(maine_racers_time)
hist(away_racers_time)

### Question 1c
boxplot(maine_racers_time, away_racers_time, names = c("Maine Runners", "Away Runners"), ylab = "Time in Minutes")

### Question 1d

# Create a subset of all the male runners from the data set
male_road_runners <- subset(road_race_data, road_race_data$Sex == 'M')

#Filter the male runners based on their age 
male_runners <- as.numeric(as.character(male_road_runners$Age))

# Create a subset of all the female runners from the data set
female_road_runners <- subset(road_race_data, road_race_data$Sex == 'F')

#Filter the female runners based on their age 
female_runners <- as.numeric(as.character(female_road_runners$Age))

# Draw a side-to-side box plot of male and female runners
boxplot(male_runners, female_runners, names = c("Male Runners", "Female Runners"), ylab = "Age in Years")

# Relevant Information about runners
mean(male_runners)
median(male_runners)
range(male_runners)
sd(male_runners)
IQR(male_runners)

mean(female_runners)
median(female_runners)
range(female_runners)
sd(female_runners)
IQR(female_runners)


# General Summary
summary(male_runners)
summary(female_runners)

### Question 2

# Read the data from csv file
motorcycle_fatalities <- read.csv('motorcycle.csv')
number_of_accidents <- motorcycle_fatalities$Fatal.Motorcycle.Accidents

# Draw a boxplot of number of accidents in South Carolina Counties
boxplot(number_of_accidents, ylab = "Number of Motorcycle Accidents")
summary(motorcycle_fatalities$Fatal.Motorcycle.Accidents)


        

