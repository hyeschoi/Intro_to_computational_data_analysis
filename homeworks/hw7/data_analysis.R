#-------------------------------------------------------------------
# Header
#-------------------------------------------------------------------

#-------------------------------------------------------------------
# packages
#-------------------------------------------------------------------

library(readr)
library(ggplot2)
library(ggmap)
library(maptools)
library(maps)
#-------------------------------------------------------------------
# Data import : XML parsing
#-------------------------------------------------------------------

storms <- read_csv('./ibtracs_2010.csv', col_names = TRUE)

#-------------------------------------------------------------------
# Data Analysis
#-------------------------------------------------------------------
# Perform an exploratory analysis of the pressure, wind speed, and duration of storms. Get descriptive
# summaries (min, max, quartiles, mean, std deviation, etc), as well as histograms and/or boxplots.
# In addition, answer the following questions (you’ll also have to include them in the report):

# descriptive summaries
summary(storms)
head(storms)
tail(storms)

# summary statistics for pressure
min(na.omit(storms$press))
max(na.omit(storms$press))
sd(na.omit(storms$press))
mean(na.omit(storms$press))
quantile(na.omit(storms$press))

boxplot(na.omit(storms$press), main = 'Box plot of pressure', ylab='Pressure (hPa)', horizontal = TRUE)
hist(storms$press, xlab = 'Pressure (hPa)' , main = 'Histogram of Pressure')

# summary statistics for wind speed

min(na.omit(storms$wind))
max(na.omit(storms$wind))
sd(na.omit(storms$wind))
mean(na.omit(storms$wind))
quantile(na.omit(storms$wind))

boxplot(storms$wind, main = 'Box plot of Wind', ylab='Wind (kt)', horizontal = TRUE)
hist(storms$wind, xlab = 'Wind (kt)' , main = 'Histogram of Wind')

# duration of storms and its summary statistics 

duration <- sapply(unique(storms$name), function(x) {max(storms$date[storms$name == x]) - min(storms$date[storms$name == x])})

min(duration)
max(duration)
sd(duration)
mean(duration)
quantile(duration)

hist(duration)
boxplot(duration, horizontal = TRUE, main= 'Boxplot of duration', xlab = 'Duration (days)')

# Total number of storms in 2010

sum(format(storms$date, '%Y') == '2010')

# Number of storms with winds >= 35 knots (tropical storms)

sum((storms$wind >= 35), na.rm = TRUE)

# Number of storms with winds >= 64 knots (hurricanes)

sum((storms$wind >= 64), na.rm = TRUE)

# Number of storms with winds >= 96 knots (major hurricanes)

sum((storms$wind >= 96), na.rm = TRUE)

# Number of storms per hemisphere (north and south)

# num_north : number of storms on the north hemisphere
# num_south : number of storms on the south hemisphere

num_north <- sum(storms$longitude >= 0, na.rm = TRUE)
num_south <- sum(storms$longitude < 0, na.rm = TRUE)

# Frequency table with data points per month (month in words)

month <- months(storms$date)
freq_month <- table(month)
freq_month <- freq_month[month.name]
freq_month

#  Frequency table with data points per month (month in words) in northern hemisphere

month_north <- months(storms$date[na.omit(storms$longitude >= 0)])
freq_north <- table(month_north)
freq_north <- freq_north[month.name]
freq_north

# Frequency table with data points per month (month in words) in southern hemisphere

freq_south <- freq_month - freq_north
freq_south

# Name of storm that lasted most days
names(which.max(duration))

# Name of storm with maximum wind speed (and speed value)

unique(storms$name[na.omit(storms$wind == max(storms$wind, na.rm = TRUE))])
max(storms$wind, na.rm = TRUE)

# Name of storm with minimum pressure (and pressure value)

unique(storms$name[na.omit(storms$press == min(storms$press, na.rm = TRUE))])
min(storms$press, na.rm = TRUE)

#-------------------------------------------------------------------
# Data Visualization
#-------------------------------------------------------------------

# plotting the storm coordinates (longitude and latitude) with points()
map()
with(storms,
     points(longitude, latitude, pch = '.', col = 'red'))

# Draw storm occurence locations on the world map 
mp <- NULL
mapWorld <- borders("world", colour="#dd855c40", fill="#67b988") # create a layer of borders
mp <- ggplot() + mapWorld
mp <- mp+ geom_point(aes(x=storms$longitude, y=storms$latitude), col = '#1a3a4990', size=3) 
mp <- mp + ggtitle('IBTrACS Storms 2010 Map')
mp <- mp + xlab('Longitude') + ylab('Latitude')
mp <- mp + theme(panel.background = element_rect(fill = "#aae6f6"),
                 panel.grid.major = element_blank(),
                 panel.grid.minor = element_blank())+
      geom_hline(yintercept = 0 , col='#285658a0', linetype = 2)
mp

# Draw the same graph with color that varies by the wind speed;
# the higer the speed was, the darker blue a point is colored
mp1 <- NULL
mapWorld <- borders("world", colour="#dd855c40", fill="#67b988") # create a layer of borders
mp1 <- ggplot(storms, aes(x=longitude, y=latitude, col = wind) ) + mapWorld
mp1 <- mp1+ geom_point(pch= 20, size=3) 
mp1 <- mp1 + ggtitle('IBTrACS Storms 2010 Map')
mp1 <- mp1 + xlab('Longitude') + ylab('Latitude')
mp1 <- mp1 + theme(panel.background = element_rect(fill = "#aae6f6"),
                 panel.grid.major = element_blank(),
                 panel.grid.minor = element_blank())
mp1 <- mp1 + geom_hline(yintercept = 0 , col='#285658a0', linetype = 2)
 
mp1

