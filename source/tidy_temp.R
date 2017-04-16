# Download the temperature Data into R
# Data files are present in the data folder under the R Project
# temp.csv file contains temperature data 
temp <- read.csv(file="data/temp.csv",header=T, sep=',')
# View the structure of the file
str(temp)
# There are 574223 observations and 4 variables in the data.frame temp

# View the summary of the file to view the count of NA values
sum(is.na(temp$Date))
sum(is.na(temp$Monthly.AverageTemp))
sum(is.na(temp$Monthly.AverageTemp.Uncertainty))
sum(is.na(temp$Country))

# There are some NA values in the Monthly.AverageTemp (32578 records) and Monthly.AverageTemp.Uncertainty(31839 records) columns
# Count of Records with NA as value in both Monthly.AverageTemp and Monthly.AverageTemp.Uncertainty column value
sum(is.na(temp$Monthly.AverageTemp.Uncertainty) & is.na(temp$Monthly.AverageTemp))
# Since Count of Records with NA as Monthly.AverageTemp.Uncertainty column value equals Count of Records with NA 
#  as value in both Monthly.AverageTemp and Monthly.AverageTemp.Uncertainty column value. So if we remove records 
#  having NA values for Monthly.AverageTemp, records with NA as Monthly.AverageTemp.Uncertainty value will also be
#  removed.

# Since for the analysis of temperature, we don't need to records in which the temperature was not recorded, these 
# records can be dropped.
temp <- temp[which(! is.na(temp$Monthly.AverageTemp)),]
# For verification, look into the summary of the data to see if there are still records with null values.
summary(temp)
# View the structure of the file
str(temp)

# Convert the format of Date and Country column to Date and Character respectively
temp$Date <- as.Date(temp$Date, '%Y-%m-%d')
temp$Country <- as.character(temp$Country)
# View the summary of the data frame again
summary(temp)
# Notice that conversion of the Date column to the type Date intorduces 327454 NA's in the data frame.
# Since the count of NA in Date column is very high, examine the input csv file manually suspecting
#  the reason for NA values to be the presence of the date's in multiple formats
# Manual study shows that the date format in the date column is of two types - %Y-%m-%d and %m/%d/%Y.
#  The Date conversion failed to convert all the dates since they were in different formats.
# So the rectification of this would be to repeat all the steps above again and set the date format 
#  as both %m/%d/%Y and %Y-%m-%d

# Read the file again
temp <- read.csv(file="data/temp.csv",header=T, sep=',')
# create new column in data frame named date1 which will have Dates in input format %Y-%m-%d and output format %Y-%m-%d
temp$Date1 <- format(as.Date(temp$Date, '%Y-%m-%d'),'%Y-%m-%d')
# create new column in data frame named date1 which will have Dates in input format %m/%d/%Y and output format %Y-%m-%d
temp$Date2 <- format(as.Date(temp$Date, '%m/%d/%Y'),'%Y-%m-%d')
head(temp)
header_names <- c("Date", "Monthly.AverageTemp", "Monthly.AverageTemp.Uncertainty", "Country")
temp <- rbind(setNames(temp[which(! is.na(temp$Date1)),c(5,2:4)], header_names), setNames(temp[which(! is.na(temp$Date2)),c(6,2:4)], header_names))
summary(temp)
# change the format of the Date column to '%Y-%m-%d'
temp$Date <- as.Date(temp$Date)
summary(temp)
# Since for the analysis of temperature, we don't need to records in which the temperature was not recorded, these 
# records can be dropped.
temp <- temp[which(! is.na(temp$Monthly.AverageTemp)),]

# Write the tidied Temperature data file into the data folder with file name as cleaned_Temp_Data.txt
write.table(temp, 'data/cleaned_Temp_Data.txt',
            sep = "\t", row.names = FALSE, quote = FALSE)
