# Download the city temperature Data into R
# Data files are present in the data folder under the R Project
# citytemp.csv file contains temperature data 
city_temp <- read.csv(file="data/citytemp.csv",header=T, sep=',')
# View the structure of the file
str(city_temp)
# There are 237200 observations (rows) and 7 varaiables (columns) in the data frame
# count the number of NA's in each column of the data frame
sum(is.na(city_temp$Date))
sum(is.na(city_temp$Monthly.AverageTemp))
sum(is.na(city_temp$Monthly.AverageTemp.Uncertainty))
sum(is.na(city_temp$City))
sum(is.na(city_temp$Country))
sum(is.na(city_temp$Latitude))
sum(is.na(city_temp$Longitude))

# Firstly convert the columns to proper data types
city_temp$Date <- as.Date(city_temp$Date,'%Y-%m-%d')
city_temp$City <- as.character(city_temp$City)
city_temp$Country <- as.character(city_temp$Country)
city_temp$Latitude <- as.character(city_temp$Latitude)
city_temp$Longitude <- as.character(city_temp$Longitude)
# check the counts of NA fields again to see if column formatting introducted any additional NA values
sum(is.na(city_temp$Date))
sum(is.na(city_temp$Monthly.AverageTemp))
sum(is.na(city_temp$Monthly.AverageTemp.Uncertainty))
sum(is.na(city_temp$City))
sum(is.na(city_temp$Country))
sum(is.na(city_temp$Latitude))
sum(is.na(city_temp$Longitude))
# notice that changing the column data type caused 135135 NA values introduced in Date field.
# This means that Date has dates in multiple formats
# So we need to download the file again and look for the various formats
city_temp <- read.csv(file="data/citytemp.csv",header=T, sep=',')

# On analysing the data, the date is found to be present in 2 formats - %Y-%m-%d and %m/%d/%Y.
# Using lubridate package, read all the date formats and convert it into %Y-%m-%d format
library(lubridate)
city_temp$Date <- as.Date(format(parse_date_time(city_temp$Date, c('Ymd','mdY')),'%Y-%m-%d'))
summary(city_temp)
# Convert other columns to proper data types
city_temp$City <- as.character(city_temp$City)
city_temp$Country <- as.character(city_temp$Country)
city_temp$Latitude <- as.character(city_temp$Latitude)
city_temp$Longitude <- as.character(city_temp$Longitude)
# check if there are NA values present. Using summary function to do so.
summary(city_temp)

# Monthly.AverageTemp and Monthly.AverageTemp.Uncertainty have 10802 rows each with NA values
# Check if the 10802 rows for both of Monthly.AverageTemp and Monthly.AverageTemp.Uncertainty point to the same rows or 
#  different. 
sum(is.na(city_temp$Monthly.AverageTemp) & is.na(city_temp$Monthly.AverageTemp.Uncertainty))
# Count of records having both Monthly.AverageTemp and Monthly.AverageTemp.Uncertainty as NA is also 10802.
# So if we remove the rows having NA values of any one of the two columns, both will get rid of records with NA values
# Since for the analysis of temperature, we don't need to records in which the temperature was not recorded, these 
# records can be dropped.
city_temp <- city_temp[which(! is.na(city_temp$Monthly.AverageTemp)),]
# For verification, look into the summary of the data to see if there are still records with null values.
summary(city_temp)
# View some records of the data set to see how the data looks like.
# head(city_temp)
# View the class of each column of the data
sapply(city_temp,class)

# View the structure of the data again
str(city_temp)
# View some records of the data set to see how the data looks like.
head(city_temp)
summary(city_temp)
# Write the tidied City Temperature data file into the data folder with file name as cleaned_City_Temp_Data.txt
write.table(city_temp, 'data/cleaned_City_Temp_Data.txt',
            sep = "\t", row.names = FALSE, quote = FALSE)
