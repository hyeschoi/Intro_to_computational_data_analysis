#-------------------------------------------------------------------
# Header
#-------------------------------------------------------------------

#-------------------------------------------------------------------
# packages
#-------------------------------------------------------------------

library(XML)
library(stringr)
library(readr)

#-------------------------------------------------------------------
# Data import : XML parsing
#-------------------------------------------------------------------

climate_2010<- xmlParse('./Year.2010.ibtracs_wmo.v03r06.cxml', asText = TRUE)

#-------------------------------------------------------------------
# Data Processing and Cleaning
#-------------------------------------------------------------------
# extract various pieces of data from the xml file.
# Focus on using XPATH expressions and the functions getNodeSet() and xpathSApply().
# The goal is to create a cleaned data.frame with the following variables:
# • name: name of the storm (e.g. ANJA) as character
# • date: date (e.g. 2009-11-13) as Date
# • time: time (e.g. 06:00:00) as character
# • latitude: latitude (e.g. -9.50) as numeric
# • lat_deg: latitude degrees (e.g. “N”) as character
# • longitude: longitude (e.g. 72.50) as numeric
# • lon_deg": longitude degrees (e.g. “E”) as character
# • press: pressure (e.g. 1006.0) as numeric
# • wind: wind speed (e.g. 0.0) as numeric
# Notice that -999 is the value used in pressure and wind speed for missing data. This implies that you have
# convert those values to NA.
# Remove those storms with names MISSING and INVEST.


# extract name

name <- xpathSApply(doc = climate_2010 ,
            path = '//cycloneName',
            xmlValue)
num_rep <- xpathSApply(doc = climate_2010,
                       path = '//disturbance[@ID]',
                       xmlSize)-1

name <- rep(name, times = num_rep)

# extract date and time

date_time <- xpathSApply(doc = climate_2010 ,
            path = '//validTime',
            xmlValue)

date <- str_extract(date_time, pattern = '[0-9]{4}\\-[0-9]{2}\\-[0-9]{2}')
date <- as.Date(date, format = '%Y-%m-%d')

time <- str_extract(date_time, pattern = '[0-9]{2}\\:[0-9]{2}\\:[0-9]{2}')

# extract latitude

latitude <- xpathSApply(doc = climate_2010, path = '//latitude', xmlValue)
latitude <- as.numeric(latitude)

# extract latitude degree

lat_deg <- xpathSApply(doc = climate_2010, path = '//latitude', xmlAttrs) 
lat_deg <- str_replace(lat_deg , pattern = 'deg ', replacement = '')

# extract longitude

longitude <- xpathSApply(doc = climate_2010, path = '//longitude', xmlValue) 
longitude <- as.numeric(longitude)

# extract longitude degree

lon_deg <- xpathSApply(doc = climate_2010, path = '//longitude', xmlAttrs) 
lon_deg <- str_replace(lon_deg, pattern = "deg ", replacement = '')

# extract pressure
press <- xpathSApply(doc = climate_2010, path = '//pressure', xmlValue)
press <- as.numeric(press) 
press[press == -999] <- NA

# extract wind speed
wind <- xpathSApply(doc = climate_2010, path = '//speed', xmlValue)
wind <- as.numeric(wind)
wind[wind == -999] <- NA

# combine all extracted data into a data frame 'cln_clim2010'

cln_clim2010<- data.frame(name = name, date = date, time = time, latitude= latitude, lat_deg=lat_deg,
                          longitude = longitude, lon_deg = lon_deg, press = press, wind = wind)

# remove rows with name: either INVEST or MISSING

cln_clim2010 <- subset(cln_clim2010, name != 'INVEST')
cln_clim2010 <- subset(cln_clim2010, name != 'MISSING')

#-------------------------------------------------------------------
#  Save the cleaned data
#  output: csv file
#-------------------------------------------------------------------
write_csv(cln_clim2010, 'ibtracs_2010.csv')
