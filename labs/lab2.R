# =====================================================
# Stat133: Lab 2
# Author: Gaston Sanchez
# Description: Basics of data frames
# Data: Star Wars characters
# =====================================================

# To read the data, we'll use the package "readr"
# To install "readr" type:
install.packages("readr")

# Remember that you only need to install a package once

# load "readr
library("readr")

# read data using read_csv()
# (read_csv() does not convert strings into factors)
sw <- read_csv("https://raw.githubusercontent.com/gastonstat/stat133/master/datasets/starwarstoy.csv")


# use str() to get information about the data frame structure

str(sw)
# use summary() to get some descriptive statistics
summary(sw)

# convert column 'gender' as a factor
sw$gender <- factor(sw$gender)

# apply summary() on gender
summary(sw)

# apply table() on gender
table(sw$gender)

# what's the largest height value
max(sw$height)

# what's the minimum height value
min(sw$height)

# what's the average height
mean(sw$height)

# who has the smallest height
sw$name[which.min(sw$height)]

# who has the largest height
sw$name[which.max(sw$height)]

# sort height
sort(sw$height)

# sort weight in decrasing order

sort(sw$weight, decreasing = TRUE)
# are there any subjects with weapon "knife"

any(sw$weapon == 'knife')

# are there any subjects with height > 2 meters
any(sw$height > 2)

# are there any subjects with 
# height < 0.5 or weight < 50
any( sw$height <0.5 | sw$weight <50)

# what are the unique weapons
unique(sw$weapon)

# how many different species
length(unique(sw$species))


# =====================================================
# Subsetting operations (without using subset())
# =====================================================

# select the first row
sw[1,]

# select Han Solo's information (i.e. row)

sw[sw$name == 'Han Solo',]
# select subject's info of smallest height
sw[which.min(sw$height),]

# get the data of male subjects
sw[which(sw$gender == 'male'),]

# get the data for those subjects 
# with height less than 1.7 
sw[sw$height <1.7,]

# get the data for subjects with 
# height less than 1.7 and weight less than 50
sw[sw$height < 1.7 & sw$weight <50,]

# select data of droids or humans
sw[sw$species %in% c('human', 'droid'),]

# select data of non-humans
sw[sw$species != 'human',]


# =====================================================
# Subsetting operations using subset()
# Perform the same tasks but now using 'subset()'
# =====================================================

# Han Solo's info

subset(sw, name == 'Han Solo')
# subject's info of smallest height
subset(sw, height == min(height))

# male subjects
subset(sw, gender == 'male')

# height less than 1.7
subset(sw, height <1.7)

# height less than 1.7 and weight less than 50
subset(sw, height < 1.7 , weight <50 )

# data of droids or humans
subset(sw, species %in% c('droid', 'human'))

# data of non-humans
subset(sw, species != 'human')


# =====================================================
# Sorting operations
# =====================================================

# sort data by height
sw[order(sw$height), ]

# sort data by weight in decreasing order
sw[order(sw$weight, decreasing = TRUE),]

# sort data by gender
sw[order(sw$gender),]

# sort data by species
sw[order(sw$species),]


# =====================================================
# Additional questions
# =====================================================

# get data of jedis
sw[sw$jedi == 'jedi',]

# get the names of non-jedis
sw$name[sw$jedi == 'no_jedi']

# data of non humans
sw[sw$species != 'human',]

# get the data of those above the average weight?
sw[sw$weight > mean(sw$weight),]

# how many unarmed humans?
sum(sw$species == 'human' & sw$weapon == 'unarmed')

# standard deviation of weight in non-humans 
sd(subset(sw, species != 'human')$weight)
non_human <- subset(sw, species != 'human')
sd(non_human$weight)
# which humans use a lightsaber
sw$name[sw$species =='human' & sw$weapon =='lightsaber']

# median height of unarmed non-humans
median(subset(sw, weapon == 'unarmed', species != 'human')$height)

# get the square weight of non-jedis


# get the log of height for subjects
# with blaster or bowcaster weapons
log(subset(sw, weapon %in% c('blaster','bowcaster'))$height)


# use plot() to make a scatter plot of height and weight
plot(sw$height, sw$weight,xlab = 'Height' , ylab = 'Weight',pch = 18, cex =2,col = 'orange')

# create a new variable "newvar": height divided by weight
newvar<- sw$height/sw$weight

# add 'newvar' to the data frame sw
sw <- cbind(sw, newvar)
sw
