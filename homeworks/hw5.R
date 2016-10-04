
#================================================================================================
#  HEADER
#================================================================================================


#------------------------------------------------------------------------------------------------
#  Packages for this Homework:
#-----------------------------------------------------------------------------------------------

library(readr)
library(stringr)
library(ggplot2)

#------------------------------------------------------------------------------------------------
#  Import Data
#-----------------------------------------------------------------------------------------------

download.file(url = 'http://nflsavant.com/dump/weather_20131231.csv', destfile = 'raw_nflweather.csv')
raw_nflweather <- read_csv('raw_nflweather.csv')

#-----------------------------------------------------------------------------------------------
#  A First Inspection Data
#-----------------------------------------------------------------------------------------------
str(raw_nflweather)
head(raw_nflweather)
tail(raw_nflweather)
nrow(raw_nflweather)
ncol(raw_nflweather)
summary(raw_nflweather)

#-----------------------------------------------------------------------------------------------
#  Regex function
#-----------------------------------------------------------------------------------------------


# remove the percent symbol % from the values in column humidity 
# and convert such values to numeric

format
raw_nflweather$humidity <- gsub("\\%","", raw_nflweather$humidity)
raw_nflweather$humidity <- as.numeric(raw_nflweather$humidity)


# extract the temperature values from column weather; and create a column temperature2 
# with these values (as numeric).

raw_nflweather$temperature2 <- substr(raw_nflweather$weather,1,2)
raw_nflweather$temperature2 <- gsub(" ","", raw_nflweather$temperature2)
raw_nflweather$temperature2 <- as.numeric(raw_nflweather$temperature2)


# extract the humidity values from column weather; and create a column humidity2 
# with these values (as numeric)

raw_nflweather$humidity2 <- str_extract(raw_nflweather$weather, 'humidity [:digit:]+')
raw_nflweather$humidity2 <- gsub("humidity ", "", raw_nflweather$humidity2)
raw_nflweather$humidity2 <- as.numeric(raw_nflweather$humidity2)


# extract the wind speed values from column weather; and create a column wind2 
# with these values (as numeric)

raw_nflweather$wind2 <- str_extract(raw_nflweather$weather, '[:digit:]+ mph')
raw_nflweather$wind2 <- gsub(" mph", "" , raw_nflweather$wind2)
raw_nflweather$wind2 <- as.numeric(raw_nflweather$wind2)


# Make sure that the new columns coincide with the pre-existing ones. 

summary(raw_nflweather)


#-----------------------------------------------------------------------------------------------
#  Data information
#-----------------------------------------------------------------------------------------------
# • Create a column year that contains the number of the year (as numeric).
# • Create a column monthnum that contains the number of the month (as numeric)
# • Create a column month that contains the name of the corresponding month (as factor); 
#   e.g. if the month number is 9 then month will be september.

date_split <- strsplit(raw_nflweather$date, '\\/')
date_frame <- as.data.frame(do.call('rbind',date_split), stringsAsFactors = FALSE)
colnames(date_frame) <- c('month','day','year')

raw_nflweather$year <- as.numeric(date_frame$year)
raw_nflweather$monthnum <- as.numeric(date_frame$month)

month_name <- c('january','february','march','april','may','june','july',
                'august','september','october','november','december')
raw_nflweather$month <- as.factor(month_name[raw_nflweather$monthnum])


# • Create a column decade that indicates the corresponding decade (as factor) of each played game. 
# Use labels: 1960s, 1970s, 1980s, 1990s, 2000s, 2010s. For instance, all games between 1970 and 1979 
# will have the associated decade 1970s.

raw_nflweather$decade <- cut(raw_nflweather$year, breaks = c(1960,1970,1980,1990,2000,2010,2020), 
                             include.lowest = TRUE, right = FALSE,
                             labels = c('1960s', '1970s', '1980s', '1990s', '2000s', '2010s'))
                             



#-----------------------------------------------------------------------------------------------
#  Score information
#-----------------------------------------------------------------------------------------------
# • Create a column total_score that contains the total number of scored points in each game.
# In other words, the sum of home_score and away_score
# • Create a column diff_score that indicates the difference of home_score and away_score.
# In other words, the subtraction of home_score and away_score
# • Create a column home_win that shows whether home_score is greater than away_score. 
# This column will have logical values (TRUE or FALSE)

raw_nflweather$total_score <- raw_nflweather$home_score + raw_nflweather$away_score
raw_nflweather$diff_score <- abs(raw_nflweather$home_score - raw_nflweather$away_score)
raw_nflweather$home_win <- (raw_nflweather$home_score > raw_nflweather$away_score)


#-----------------------------------------------------------------------------------------------
#  Basic Exploration
#-----------------------------------------------------------------------------------------------
# • Inspect variables home_score, away_score, temperature, and wind_mph 
# by getting summary statistics
# (this is just a first inspection making sure there are no “abnormal” values)
summary(raw_nflweather[, c('home_score', 'away_score', 'temperature', 'wind_mph')])

# • Visually inspect variables home_score, away_score, temperature, humidity, and wind_mph 
# (these are “quick and basic” plots)
ggplot(raw_nflweather, aes( x= home_score)) + geom_histogram()
boxplot(raw_nflweather$home_score,horizontal = TRUE, main = 'Home Score')

ggplot(raw_nflweather, aes( x= away_score)) + geom_histogram()
boxplot(raw_nflweather$away_score,horizontal = TRUE, main = 'Away Score')

ggplot(raw_nflweather, aes( x= temperature)) + geom_histogram()
boxplot(raw_nflweather$temperature,horizontal = TRUE, main = 'Temperature')

ggplot(raw_nflweather, aes( x= humidity)) + geom_histogram()
boxplot(raw_nflweather$humidity,horizontal = TRUE, main = 'Humidity')

ggplot(raw_nflweather, aes( x= wind_mph)) + geom_histogram()
boxplot(raw_nflweather$wind_mph,horizontal = TRUE, main = 'Wind mph')


# • What team has the maximum home score?

raw_nflweather$home_team[which.max(raw_nflweather$home_score)]

# • What team has the maximum away score?

raw_nflweather$away_team[which.max(raw_nflweather$away_score)]

# • What is the most common home score?

table_homescr <- table(raw_nflweather$home_score)
names(table_homescr)[which.max(table_homescr)]

# • What is the most common away score?

table_awayscr <- table(raw_nflweather$away_score)
names(table_awayscr)[which.max(table_awayscr)]

# • What has been the maximum temperature in a game?

max(raw_nflweather$temperature)

# • What was the date of the maximum temperature?

raw_nflweather$date[which.max(raw_nflweather$temperature)]

# • What has been the minimum temperature in a game?

min(raw_nflweather$temperature)

# • What was the date of the minimum temperature?

raw_nflweather$date[which.min(raw_nflweather$temperature)]

# • How many games have been played with a temperature of 90 degrees or more?

sum(raw_nflweather$temperature >= 90)

# • How many games have been played with a temperature below 0 degrees (do not include 0)?

sum(raw_nflweather$temperature < 0)

# • What is the most common temperature?

table_temperature <- table(raw_nflweather$temperature)
names(table_temperature)[which.max(table_temperature)]

# • Make a bar chart with the frequency table of temperatures? 


barplot(table_temperature)

# Q. Is there anything that catches your attention?
# A. There has been games done at 72 degree extraordinarily often.



#-----------------------------------------------------------------------------------------------
#  Data Files
#-----------------------------------------------------------------------------------------------

# subset the dataset into different decades. 
# That is, obtain one data.frame with data from the 1960s, another onefrom the 1970s, and so on.
# arrange selcted columns in this following order:
# 1. id          9. date
# 2. home_team   10. year
# 3. home_score  11. month
# 4. away_team   12. decade
# 5. away_score  13. temperature
# 6. total_score 14. humidity
# 7. diff_score  15. wind_mph
# 8. home_win
#
# export a csv file for each decade:
# saves these files into a folder named cleandata
for (i in c('1960','1970','1980','1990','2000','2010')){
   sub_data <- raw_nflweather[ raw_nflweather$decade == paste(i, 's', sep=''), 
                      c( 'id','home_team','home_score','away_team','away_score',
                                 'total_score','diff_score','home_win', 'date', 'year', 
                                 'month', 'decade','temperature', 'humidity', 'wind_mph')]
  write.csv(sub_data, paste('./cleandata/nflweather', i, 's.csv' ,sep = ''),)
}

# export a csv file with all games:
# saves this file into a folder named cleandata
cln_nflweather<- raw_nflweather[ , 
                                 c( 'id','home_team','home_score','away_team','away_score',
                                    'total_score','diff_score','home_win', 'date', 'year', 
                                    'month', 'decade','temperature', 'humidity', 'wind_mph')]

write.csv(cln_nflweather, './cleandata/nflweather.csv')



