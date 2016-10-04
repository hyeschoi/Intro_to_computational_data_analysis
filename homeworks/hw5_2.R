
#-----------------------------------------------------------------------------------------------
#  Data Analysis
#-----------------------------------------------------------------------------------------------
#
# focus on two main research questions:
# 1. Does playing at home really have an advantage for the home team?
# 2. Has the total number of scored points per game have changed over time?
#
# provide evidence in favor of the claim that 
#“playing at home does give the home team an advantage over the visiting team”.
# For this, aggregate data by year.

#------------------------------------------------------------------------------------------------
#  Packages for this Homework:
#-----------------------------------------------------------------------------------------------

library(readr)
library(stringr)
library(ggplot2)


#-----------------------------------------------------------------------------------------------
# import data 
#-----------------------------------------------------------------------------------------------
# as cln_nflweather : imported data
cln_nflweather <- read_csv('./cleandata/nflweather.csv')


################################################################################
# Question:  Playing at home has an advantage for the home-team?
################################################################################


# function: logical_numsum
#   input : logical vector
#   output: convert true into 1 and false into -1, and return sum of these numeric values
#
logical_numsum <- function (x){
  sum(2* x -1)
}

#
# variable: aggregate_homewin 
# value   : for each year, calculate ( # of homewin - # of awaywin) 
aggregate_homewin <- tapply(cln_nflweather[ ,c('home_win')], cln_nflweather$year, logical_numsum)

#
# graph: barplot of aggregate_homewin
#
barplot(aggregate_homewin, las=2, main="Difference between home wins and home loses by year")
#
# graph: aggregate_homewin with ggplot
#
aggregate_homewin_df <- cbind(names(aggregate_homewin),as.data.frame (aggregate_homewin))
colnames(aggregate_homewin_df) <- c('year', 'homewin_num')

ggplot(data = aggregate_homewin_df, aes(y = homewin_num, x= year) ) +
  geom_bar(stat= 'identity', las=1)+ 
  ggtitle("Difference between home wins and home loses by year")+
  theme(axis.text.x=element_text(angle = 90, hjust = 0)) + ylab("")+
  scale_y_continuous(breaks = c(-20,-10,0,10,20,30,40,50,60,70))

#
# Graph analysis:
# Except for a few years in the 1960s (1962, 1964, 1965, and 1968), 
# the rest of the seasons confirm that more games are won by the home-team rather than the visiting team.
###############################################################################################
###############################################################################################
#
#
# variable: aggregate_score
# value: for each year, calculate the average of homewin score and the average of awayscore
# 
aggregate_score <- aggregate(cln_nflweather[ ,c('home_score','away_score')], list(cln_nflweather$year), mean)
colnames(aggregate_score) <- c('year','home_score','away_score')

#
# In order to draw two graphs of average score in one plot
# devide the result into two parts : avg_homescore, avg_awayscore
# then recombine those two parts by rowbind
# with additional type columns that indicate which type of average it is.
#
aggregate_homescore <- cbind(aggregate_score[,c('year','home_score')],'avg_homescore')
aggregate_awayscore <- cbind(aggregate_score[,c('year','away_score')],'avg_awayscore')
colnames(aggregate_homescore) <- c('year','average_score','type')
colnames(aggregate_awayscore) <- c('year','average_score', 'type')
aggregate_avgscore <- rbind(aggregate_homescore, aggregate_awayscore)

#
# Graph: plot two graphs of average home score and average away score versus year
#
ggplot(aggregate_avgscore, aes(x=year, y= average_score , col= type))+
  geom_line() + geom_point()+theme(legend.position = 'bottom')+
  ylab('score')+
  ggtitle('Average score points per year')

#
# Graph Analysis:
# the average of home_score is almost always above the average of away_score.
#
################################################################################
# Question:  Has the total number of scored points per game changed over time?
################################################################################
#
# 
avg_total_year <- tapply(cln_nflweather$total_score, cln_nflweather$year, mean)
avg_total_df <- cbind(as.numeric(names(avg_total_year)), as.data.frame(avg_total_year))
colnames(avg_total_df) <- c('year','avg_totalscore')

ggplot(avg_total_df, aes( year, avg_totalscore))+ geom_point() + geom_path ()+
  ggtitle('Average total scored points per year') + geom_smooth(method = 'loess')
#
# Graph Analysis:
# the total scored points have indeed changed over time
#
