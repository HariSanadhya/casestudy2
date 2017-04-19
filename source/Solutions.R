# Install packages dplyr and ggplot2 which will be used in the data analysis process
# To install these packages, uncomment the code line below
# install.packages('dplyr')
# install.packages('ggplot2')
library(dplyr)
library(ggplot2)

# Load the cleaned temperature data into a data frame named cleaned_temp 
# Source data file: data/cleaned_Temp_Data.txt
cleaned_temp <- read.csv("data/cleaned_Temp_Data.txt",header=T, sep="\t")

# Load the cleaned city temperature data into a data frame named cleaned_city_temp
# Source data file: data/cleaned_City_Temp_Data.txt
cleaned_city_temp <- read.csv("data/cleaned_City_Temp_Data.txt",header=T, sep="\t")

# Print the first 6 records of the cleaned files to verify that the data loaded correctly
head(cleaned_temp)
head(cleaned_city_temp)

# To determine difference between the maximum and the minimum monthly average temperatures for each country
#  since 1900, select the records with date starting - '1900-01-01' and group the selected data by country
group_temp_by_country <- group_by(cleaned_temp[as.Date(cleaned_temp$Date)>=as.Date('1900-01-01'),], Country)
# Calculate the difference between the maximum and the minimum monthly average temperatures for each country
temp_diff <- as.data.frame(summarise(group_temp_by_country, max(Monthly.AverageTemp) - min(Monthly.AverageTemp)))
# Set proper column headings
temp_diff <- setNames(temp_diff, c("Country","Maximum.variation.in.monthly.avg.temp.since.1900"))
# Rearrange the data frame in descending order of the difference in temperatures
temp_diff <- arrange(temp_diff,desc(Maximum.variation.in.monthly.avg.temp.since.1900))
# Write this data having maximum monthly average temperature variation by country into the data folder with file 
#  name as max_monthly_temp_varaition_by_country.txt
write.table(temp_diff, 'data/max_varaition_in_monthly_avg_temp_by_country.txt', sep = "\t", row.names = FALSE, quote = FALSE)
# Retrieve top 20 countries with the maximum differences for the period since 1900
top_20_temp_diff <- head(temp_diff,n=20) 
# View the top 20 countries by temp difference
top_20_temp_diff

# Plot of top 20 countries with maximum monthly average temperature varaition
q <- qplot(x=top_20_temp_diff$Country, y=top_20_temp_diff$Maximum.variation.in.monthly.avg.temp.since.1900,
           xlab="Country", ylab="Maximum varaition in monthly average temperature", 
           main="top 20 countries with maximum average temperature varaition", ylim = c(0,51)) 
q + theme(axis.text.x = element_text(angle = 90))
# On direct ploting of the data, the x-axis(country) will be ordered alphabetically. To avoid this automatic ordering,
#  explicitly specify the order of the data and then plot the graph again.
top_20_temp_diff$Country <- factor(top_20_temp_diff$Country,levels=top_20_temp_diff$Country[order(desc(top_20_temp_diff$Maximum.variation.in.monthly.avg.temp.since.1900))])
top_20_temp_diff$Country
q <- qplot(x=top_20_temp_diff$Country,y=top_20_temp_diff$Maximum.variation.in.monthly.avg.temp.since.1900,
      xlab="Country", ylab="Maximum varaition in monthly average temperature", 
      main="top 20 countries with maximum average temperature varaition", ylim = c(0,51)) 
q <- q + theme(axis.text.x = element_text(angle = 90))
# Save the plot
ggsave(filename = "graphs/Maximum Temp Variation.png", device="png", plot=q)
# display the graph
q

# Creation of "UStemp” - US land temperatures data from 01/01/1990
UStemp <- cleaned_temp[as.Date(cleaned_temp$Date)>=as.Date('1900-01-01') & cleaned_temp$Country == 'United States',]
# View the first few records of UStemp dataset
head(UStemp)
# since the row numbers are not re-initialized automatically, so re-initialize the row numbers
rownames(UStemp) <- NULL
# View the first few records of UStemp dataset again
head(UStemp)
# New column having monthly average US land temperatures in Fahrenheit
UStemp$Monthly.AverageTemp.F <- UStemp$Monthly.AverageTemp * 1.8 + 32

# Calculation of average land temperature by year
# group the US land temperature data by year and then calculate the average temp for the year
UStemp_year <- group_by(UStemp, as.numeric(format(as.Date(UStemp$Date), '%Y'))) %>%
  summarize(mean(Monthly.AverageTemp.F))
# Set proper column headings
UStemp_year <- setNames(UStemp_year, c("Year","Yearly.avg.temp.F"))
q <- qplot(x=UStemp_year$Year, y=UStemp_year$Yearly.avg.temp.F, xlab = "Year", ylab="Yearly average Temp(°F)",
           main="Average US land temperature by year")
q <- q + scale_x_continuous(breaks = seq(1900,2017, 13),minor_breaks=NULL)
q <- q + scale_y_continuous(breaks = seq(45,53,0.5),minor_breaks=NULL, limits=c(45,54))
# Save the plot
ggsave(filename = "graphs/Average US land temperature by year.png", device="png", plot=q)
# Display the graph
q

# Calculation of the one year difference of average land temperature by year
# For this, create a data frame having first column as the years whose temperature difference is taken (format of 'YYYY - YYYY') and the 
#  second column is the difference in temperatures
year_difference_of_temp <- (sapply(1:(length(UStemp_year$Year)-1),function(x){paste(as.character(UStemp_year$Year[x+1]), '-', as.character(UStemp_year$Year[x]))})) %>%
   as.data.frame %>% setNames(c("Years"))
year_difference_of_temp$temp_diff <- sapply(1:(length(UStemp_year$Yearly.avg.temp.F)-1),function(x){diff(UStemp_year$Yearly.avg.temp.F[x:(x+1)])})

# maximum difference (value)
index_of_max_value <- which(year_difference_of_temp$temp_diff == max(abs(year_difference_of_temp$temp_diff)))
paste('Maximum difference in average US land temperature for two consecutive years is ' , abs(year_difference_of_temp$temp_diff[index_of_max_value]), 
      '°F for year ' , year_difference_of_temp$Years[index_of_max_value], sep='')

