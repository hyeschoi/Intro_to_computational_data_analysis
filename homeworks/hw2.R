# =====================================================
# Stat133: HW 2
# Description: Basic manipulation of data structures
#              and creation of simple graphics
# Data: Camping Tents
# =====================================================

# Please submit your own R script file to bcourses

# Write your name
# Name: Hye Soo Choi


# =====================================================
# Reading the Data Into R
# =====================================================
# We'll be working with the dataset 'tents1.csv'
# (available in the folder 'datasets' in the 
# github repository of the course)
# =====================================================

# read the file 'tents1.csv' in R and assign it to an object
# called 'tents' ---this will be the main data frame--- 
# Character strings must NOT be converted to factors!
# (use whatever method you like to import the data)
library(readr)
tents <- read_csv('https://raw.githubusercontent.com/gastonstat/stat133/master/datasets/tents1.csv')
# inspect the data structure of 'tents'
str(tents)
# how many rows in the dataset?
nrow(tents)

# how many columns in the dataset?
ncol(tents)

# names of columns
names(tents)

# take a look at the first 6 rows
head(tents)


# =====================================================
# Quantitative Variables:
# =====================================================
# We'll start by exploring the quantiative variables
# 1) price
# 2) weight
# 3) height
# =====================================================

# get numeric summaries of each quantitative variable
summary(tents[c('price','weight','height')])


# weight is given in grams
# add a new variable to 'tents' for weight expressed in pounds
tents$weightpound <- tents$weight/(453.592)

# height is given in centimeters
# add a new variable to 'tents' for height expressed in inches
tents$heightinch <- tents$height/(2.54)

# how many tents have price less than $300
sum(tents$price < 300)

# how many tents have price greater than $400
sum(tents$price > 400)

# what's the name of the tent with maximum price
tents$name[which.max(tents$price)]

# what's the name of the tent with minimum price
tents$name[which.min(tents$price)]

# what's the name of the tent with maximum weight
tents$name[which.max(tents$weight)]

# what's the name of the tent with minimum weight
tents$name[which.min(tents$weight)]

# select the data of tents with 
# price > $400 AND weight < 1500 grams
subset(tents, price>400 & weight < 1500)
tents1<- subset(tents, price>400 & weight < 1500)

# subset those tents with brand 'big-agnes'
subset(tents1, brand == 'big-agnes')


# Create a factor of prices using the cut() function;
cut(tents$price, breaks = c(0,100,200,300,400,500,600,700))
# for the argument 'breaks' give a vector of cutting points
# such that the obtained levels are as follows:
#    (0, 100]
#  (100, 200]
#  (200, 300]
#  (300, 400]
#  (400, 500]
#  (500, 600]
#  (600, 700]




# =====================================================
# Statistical graphics of Quantitative Variables
# =====================================================
# for each quantitative variable, obtain the following plots:
# - histogram
# - boxplot
# - density curve
# (look at each graphic carefully and see what types 
# of distribution patterns show each variable)
# =====================================================

#price
hist(tents$price, xlab = 'price', main = 'Histogram: Price frequency',las=1)
boxplot(tents$price, main ='Box plot: Price',las=1)
d<-par(mar=c(6,6,3,3), mgp=c(4,1,0))
plot(density(tents$price), main='Density curve: Price', las=1)
par(d)
#weight
hist(tents$weight, xlab = 'Histogram: Weight', main = 'Weight frequency',las=1)
boxplot(tents$weight, main ='Box plot: Weight',las=1)
d<-par(mar=c(6,6,3,3), mgp=c(4,1,0))
plot(density(tents$weight), main='Density curve: Weight',las=1)
par(d)
#height
hist(tents$height, xlab = 'height', main = 'Histogram: Height',las=1)
boxplot(tents$height, main ='Box plot: Height',las=1)
plot(density(tents$height), main='Density curve: Height',las=1)


# Obtain scatter plots of:
# price, height
plot(tents$price, tents$height, xlab='price',ylab='height',main='price vs height', pch=20, col= 'pink',las=1)
# price, weight
plot(tents$price, tents$weight, xlab='price',ylab='weight',main='price vs weight', pch=20, col= 'lightgreen',las=1)
# height, weight
plot(tents$height, tents$weight, xlab='height',ylab='weight',main='height vs weight', pch=20, col= 'skyblue',las=1)


# Obtain a scatter plot matrix of price, height, and weight
pairs(tents[,c('price','height','weight')], cex= 0.5, col='blue',las=2)


# =====================================================
# Plot challenge: 
# Here's a small graphic challenge; you'll need to 
# read the documentation of plot() and par()
# =====================================================
# Obtain a scatter plot of height and weight such that:
# - Axis are labeled with the corresponding variable name
# - Points are colored with alpha transparency
#   (choose a color of your preference)
# - The symbol of points are squares
# - x-axis ranges from 80 to 220
# - y-axis ranges from 0 to 10000
# - Include a title
plot(tents[,c('height','weight')], xlab= 'height',ylab='weight',
     xlim=c(80,220),ylim=c(0,10000),main = 'height VS weight',pch= 15,
     col = adjustcolor('pink',alpha.f=0.6), xaxs = "i", yaxs = "i",las=1)




# =====================================================
# Qualitative Variables
# =====================================================
# Now let's focus on the qualitative variables:
# 1) brand
# 2) bestuse
# 3) seasons
# 4) capacity
# =====================================================

# get frequency tables of each qualitative variable
table(tents$brand)

table(tents$bestuse)

table(tents$seasons)

table(tents$capacity)
# what is the brand with less number of tents
names(table(tents$brand))[which.min(table(tents$brand))]

# are there any tents of brand 'rei'?
# and if so, how many?
any(tents$brand == 'rei') #True

#how many
table(tents$brand)['rei']
sum(tents$brand == 'rei')


# are there any tents of brand 'millet'
# and if so, how many?
any(tents$brand == 'millet') # False
sum(tents$brand == 'millet')

# how many 'north-face' tents are intended to be
# used ('bestuse') for Mountaineering
sum(tents$brand == 'north-face' & tents$bestuse == 'Mountaineering')

# what brands have tents intended to be used for 'Mountaineering'
unique(tents$brand[tents$bestuse == 'Mountaineering'])


# =====================================================
# Statistical graphics of Qualitative Variables
# =====================================================
# for 'bestuse', 'seasons', and 'capacity' obtain 
# the following plots:
# - bar plot
# - dot chart
# - pie chart
# (feel free to change colors, add titles, and rank values)
# =====================================================
#bestuse
d <-par(mar=c(3,3,3,3))
barplot(table(tents$bestuse), col = rainbow(3), main='Bar Chart: Best use',las=1)
par(d)
dotchart(as.numeric(table(tents$bestuse)), bg='red',
         labels=levels(as.factor(tents$bestuse)), main= 'Dot Chart: Best use' )
d<-par(mar=c(3,3,5,5))
pie(table(tents$bestuse),col=c('lightgreen','lightpink','skyblue'), main= 'Pie chart: Best use')
par(d)

#seasons
d<-par(mar=c(3,3,3,3))
barplot(table(tents$seasons), col = rgb(223,194,18, maxColorValue = 255,alpha=200), main='Bar chart: Seasons',las=1)
par(d)
dotchart(as.numeric(table(tents$seasons)), labels= levels(as.factor(tents$seasons)), bg = '#451111', main= 'Dot chart: Seasons')
pie(table(tents$seasons),col=c('lightgreen','lightpink','skyblue'), main ='Pie chart: Seasons')



# =====================================================
# Plot challenge: 
# =====================================================
# Make a barplot for 'brand' such that:
# - bars are horizontally oriented
# - bars are arranged in decreasing order
# - bars without a border
# - brand labels are perpendicular to the y-axis
#   (i.e. labels horizontally oriented)
# - modify margins in order to have enough room for labels 
# - x-axis ranges from 0 to 25
# - include a title
d <- par(mar = c(4, 8, 2, 2))
barplot(sort(table(tents$brand),decreasing = TRUE),xlim= c(0,25), xaxs= 'i',horiz = TRUE, main = 'Bar Plot: Brand'
        ,border = NA, las=1, col= 'pink', col.main ='gray')
par(d)



# =====================================================
# More basic manipulations
# =====================================================

# summary of 'price' of big agnes tents
summary(tents$price[tents$brand =='big-agnes'])

# summary of 'price' of rei tents
summary(tents$price[tents$brand == 'rei'])

# cross-table of seasons and bestuse

table(tents[,c('seasons','bestuse')])




# =====================================================
# Bivariate Plots
# =====================================================

# use boxplot() to plot the distribution of 'weight' 
# conditional to:
# - brand
# - bestuse
# - capacity
# - seasons
# (get a boxplot for each categorical variable)
d<-par(mar=c(3,3,3,3))
boxplot( data=tents, weight~brand, main ='Box plot: Weight conditional to brand', las=2, 
        names =levels(factor(tents$brand)),  par( mar=c(7,4,3,1)), col= c('#ffca2a','#7b2828',
                                                                          '#aa4f4f','#f88796',
                                                                          '#f6baba','#f8be78',
                                                                          '#f6a84b','#7d7d87'))

boxplot(data= tents, weight~bestuse, main = ' Box plot: Weight conditional to Best use', las=2, 
        names = levels(factor(tents$bestuse)), par(mar= c(7,4,3,1)),
        col =c('#6fd6a5','#ab89ee','#c5c5c5'))

boxplot(data= tents, weight~capacity, las=2, main='Box plot: Weight conditional to Capacity', 
        par(mar=c(7,4,3,1)), col=c('#b294f3','#589ce2','#f98b8b','#ddacfc','#da5fff'))

library(RColorBrewer)

boxplot(data = tents, weight~seasons, las=2, main = 'Box plot: Weight conditonal to Seasons',
        par(mar=c(7,4,3,1)), col= brewer.pal(3,'Set2'))      

# make a scatter-plot of height and weight, using 
# 'capacity' as a factor for the color argument

palette(adjustcolor(brewer.pal(5,'YlGnBu'),alpha.f=0.6))
plot(tents$height, tents$weight,col=factor(tents$capacity),pch=19,
     xlab='Height',ylab='Weight', main ='Height -vs- Weight colored by Capacity',las=1)

# get the same scatter plot but now pass 'bestuse'
# as a factor for the color argument

palette(adjustcolor(brewer.pal(3,'RdYlGn'),alpha.f=0.9))
plot(tents$height, tents$weight, col = factor(tents$bestuse), xlab='Height', ylab='Weight',
     main ='Height -vs- Weight colored by Best use', pch=19,las=1)
# remember that factors are internally stored as
# integer vectors. To get the integers associated to the 
# levels of a factor you can use unclass(). For instance:
unclass(factor(tents$bestuse))

# make the same scatter plot, using 'bestuse' as factor for colors,
# and using the integers associated to factor 'bestuse' for the
# argument that changes the type of plotted symbol
palette(c('#cad1a8f5','#ead69af5','#eac199f5'))
plot(tents$height, tents$weight, col = factor(tents$bestuse),
     xlab= 'Height', ylab='Weight', main='Height -vs- Weight',
     pch = unclass(factor(tents$bestuse))*2+13,las=1)



# =====================================================
# Plot challenge: 
# =====================================================

# obtain a new data frame by subsetting tents of brand 'rei'
tents_rei <- subset(tents, brand == 'rei')

# create a vector of colors for each rei tent according to 'bestuse':
# 'Carcamping' tents in color 'tomato'
# 'Backpacking' tents in color 'orange'
# 'Mountaineering' tents in color 'blue'
col_rei <- c('orange', 'tomato' , 'blue')
col_rei<- col_rei[unclass(factor(tents_rei$bestuse))]


# Make a scatter plot of 'height' and 'weight' (of 'rei' tents)
# The background of the entire plot must be of color 'gray99'
# Instead of points, show the name of each rei tent
# Use the vector of colors to color tent names
# Include a legend in the top left corner indicating the 
# 'bestuse' types and their corresponding colors
# Add a title
d<-par( bg='grey99')
plot(tents_rei$height, tents_rei$weight,  pch = NA, 
     xlab= 'Height', ylab='Weight', main = 'Height -vs- Weight',las=1)
text(tents_rei$height, tents_rei$weight, labels = tents_rei$name, col = col_rei,cex = 0.7)
legend("topleft",legend=levels(factor(tents_rei$bestuse)),lty=c(1,1,1), lwd(2.5,2.5,2.5),col=c('orange', 'tomato' , 'blue'))
par(d)



