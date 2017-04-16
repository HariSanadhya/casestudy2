# Load the cleaned temperature data into a data frame named cleaned_temp 
# Source data file: data/cleaned_Temp_Data.txt
cleaned_temp <- read.csv("data/cleaned_Temp_Data.txt",header=T, sep="\t")

# Load the cleaned city temperature data into a data frame named cleaned_city_temp
# Source data file: data/cleaned_City_Temp_Data.txt
cleaned_city_temp <- read.csv("data/cleaned_City_Temp_Data.txt",header=T, sep="\t")

library(dplyr)
head(cleaned_temp)
head(cleaned_city_temp)
group_temp_by_country <- group_by(cleaned_temp[as.Date(cleaned_temp$Date)>=as.Date('1900-01-01'),], Country)
temp_diff <- as.data.frame(summarise(group_temp_by_country, max(Monthly.AverageTemp) - min(Monthly.AverageTemp)))
temp_diff <- setNames(temp_diff, c("Country","Maximum.variation.in.monthly.avg.temp.since.1900"))
temp_diff <- arrange(temp_diff,desc(Maximum.variation.in.monthly.avg.temp.since.1900))
top_20_temp_diff <- head(temp_diff,n=20) 
top_20_temp_diff

# Plot of top 20 countries with different temperatures
plot(top_20_temp_diff)

UStemp <- cleaned_temp[as.Date(cleaned_temp$Date)>=as.Date('1900-01-01') & cleaned_temp$Country == 'United States',]
# since the row numbers are not re-initialized automatically, so re-initialize the row numbers
rownames(UStemp) <- NULL
UStemp$Monthly.AverageTemp.F <- UStemp$Monthly.AverageTemp * 1.8 + 32
group_UStemp_by_year <- group_by(UStemp, as.numeric(format(as.Date(UStemp$Date), '%Y')))
plot(summarize(group_UStemp_by_year,mean(Monthly.AverageTemp.F)))

