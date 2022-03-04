### Question 1a

# Read the data from csv file
road_race_data <- read.csv('roadrace.csv')

# Store the date associated with Maine in a table
road_race_table <- table(road_race_data$Maine)

# Plot the data as bar graph
barplot(road_race_table, main = "Road Race Data for Maine", ylab = "Race Participants", col = c('red', 'blue'))

# More Information about the runners
number_of_away_runners <- road_race_table[1]
number_of_Maine_runners <- road_race_table[2]


### Question 1b

# Store details of racers from Maine
road_racers_from_maine <- subset(road_race_data, road_race_data$Maine == 'Maine')
maine_racers_time <- road_racers_from_maine$Time..minutes.

# Store details of racers not from Maine
road_racers_from_away <- subset(road_race_data, road_race_data$Maine == 'Away')
away_racers_time <- road_racers_from_away$Time..minutes.

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
