<b>Introduction</b> <br>
<p>
Given the two datasets, temp and citytemp which contains the monthly average temperature data for various cities and countries around the world. Given these datasets, perform comparitive average monthly temperature analysis of various countries and cities and also for US land temperatures.
</p>
<br> <br>

``` r
# Load dplyr and ggplot2 packages
library(dplyr)
```

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

``` r
library(ggplot2)
# Import the tidied/cleaned temp and citytemp data into R data frame

# Load the cleaned temperature data into a data frame named cleaned_temp 
# Source data file: data/cleaned_Temp_Data.txt
cleaned_temp <- read.csv("data/cleaned_Temp_Data.txt",header=T, sep="\t")

# Load the cleaned city temperature data into a data frame named cleaned_city_temp
# Source data file: data/cleaned_City_Temp_Data.txt
cleaned_city_temp <- read.csv("data/cleaned_City_Temp_Data.txt",header=T, sep="\t")

# Print the first 6 records of the cleaned files to verify that the data loaded correctly
head(cleaned_temp)
```

    ##         Date Monthly.AverageTemp Monthly.AverageTemp.Uncertainty
    ## 1 1838-04-01              13.008                           2.586
    ## 2 1838-06-01              23.950                           2.510
    ## 3 1838-07-01              26.877                           2.883
    ## 4 1838-08-01              24.938                           2.992
    ## 5 1838-09-01              18.981                           2.538
    ## 6 1838-10-01              13.428                           3.039
    ##       Country
    ## 1 Afghanistan
    ## 2 Afghanistan
    ## 3 Afghanistan
    ## 4 Afghanistan
    ## 5 Afghanistan
    ## 6 Afghanistan

``` r
head(cleaned_city_temp)
```

    ##         Date Monthly.AverageTemp Monthly.AverageTemp.Uncertainty
    ## 1 1850-01-01              15.986                           1.537
    ## 2 1850-02-01              18.345                           1.527
    ## 3 1850-03-01              18.632                           2.162
    ## 4 1850-04-01              18.154                           1.693
    ## 5 1850-05-01              17.480                           1.237
    ## 6 1850-06-01              17.183                           1.252
    ##          City  Country Latitude Longitude
    ## 1 Addis Abeba Ethiopia    8.84N    38.11E
    ## 2 Addis Abeba Ethiopia    8.84N    38.11E
    ## 3 Addis Abeba Ethiopia    8.84N    38.11E
    ## 4 Addis Abeba Ethiopia    8.84N    38.11E
    ## 5 Addis Abeba Ethiopia    8.84N    38.11E
    ## 6 Addis Abeba Ethiopia    8.84N    38.11E

<b>Question 1: Find the difference between the maximum and the minimum monthly average temperatures for each country and report/visualize top 20 countries with the maximum differences for the period since 1900.</b>

``` r
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
write.table(temp_diff, 'data/max_variation_in_monthly_avg_temp_by_country.txt', sep = "\t", row.names = FALSE, quote = FALSE)
# Retrieve top 20 countries with the maximum differences for the period since 1900
top_20_temp_diff <- head(temp_diff,n=20) 
# View the top 20 countries by temp difference
top_20_temp_diff
```

    ##         Country Maximum.variation.in.monthly.avg.temp.since.1900
    ## 1    Kazakhstan                                           49.163
    ## 2      Mongolia                                           48.010
    ## 3        Russia                                           46.682
    ## 4        Canada                                           43.532
    ## 5    Uzbekistan                                           42.698
    ## 6  Turkmenistan                                           40.579
    ## 7       Belarus                                           39.338
    ## 8       Finland                                           39.068
    ## 9       Estonia                                           38.815
    ## 10      Ukraine                                           38.660
    ## 11   Kyrgyzstan                                           38.436
    ## 12  North Korea                                           38.342
    ## 13       Latvia                                           38.063
    ## 14      Moldova                                           38.012
    ## 15    Greenland                                           37.516
    ## 16      Denmark                                           37.138
    ## 17    Lithuania                                           36.970
    ## 18   Tajikistan                                           35.829
    ## 19       Poland                                           35.616
    ## 20      Armenia                                           35.273

``` r
# To avoid the automatic alphabetic ordering of the data while plotting the Country, explicitly specify the order of the data and then plot the graph.
top_20_temp_diff$Country <- factor(top_20_temp_diff$Country,levels=top_20_temp_diff$Country[order(desc(top_20_temp_diff$Maximum.variation.in.monthly.avg.temp.since.1900))])
q <- qplot(x=top_20_temp_diff$Country, y=top_20_temp_diff$Maximum.variation.in.monthly.avg.temp.since.1900,
           xlab="Country", ylab="Maximum variation in monthly average temperature(°C)", 
           main="Top 20 Countries with Maximum Average Temperature Varaition", ylim = c(0,51)) + theme(axis.text.x = element_text(angle = 90, hjust=1, vjust=0.5), plot.title = element_text(hjust = 0.5))
# Save the plot
ggsave(filename = "graphs/Maximum Temp Variation.png", device="png", plot=q)
```

    ## Saving 7 x 5 in image

``` r
# Display the plot in the markdown file
```

![](graphs/Maximum%20Temp%20Variation.png) Based on the above result, it can be said that top 20 courtries based on the Maximum variation in monthly average temperature are having the variation in the range of 35 to 49. <br> <br> <b>Question 2: Select a subset of data called “UStemp” where US land temperatures from 01/01/1990 in Temp data. Use UStemp dataset to answer the followings.(2.a, 2.b and 2.c)</b>

``` r
# Creation of "UStemp” - US land temperatures data from 01/01/1990
UStemp <- cleaned_temp[as.Date(cleaned_temp$Date)>=as.Date('1900-01-01') & cleaned_temp$Country == 'United States',]
# View the first few records of UStemp dataset
head(UStemp)
```

    ##              Date Monthly.AverageTemp Monthly.AverageTemp.Uncertainty
    ## 528004 1900-01-01              -2.573                           0.443
    ## 528005 1900-02-01              -2.912                           0.533
    ## 528006 1900-03-01               2.636                           0.358
    ## 528007 1900-04-01               8.091                           0.263
    ## 528008 1900-05-01              14.317                           0.239
    ## 528009 1900-06-01              19.280                           0.379
    ##              Country
    ## 528004 United States
    ## 528005 United States
    ## 528006 United States
    ## 528007 United States
    ## 528008 United States
    ## 528009 United States

``` r
# since the row numbers are not re-initialized automatically, so re-initialize the row numbers
rownames(UStemp) <- NULL
# View the first few records of UStemp dataset again
head(UStemp)
```

    ##         Date Monthly.AverageTemp Monthly.AverageTemp.Uncertainty
    ## 1 1900-01-01              -2.573                           0.443
    ## 2 1900-02-01              -2.912                           0.533
    ## 3 1900-03-01               2.636                           0.358
    ## 4 1900-04-01               8.091                           0.263
    ## 5 1900-05-01              14.317                           0.239
    ## 6 1900-06-01              19.280                           0.379
    ##         Country
    ## 1 United States
    ## 2 United States
    ## 3 United States
    ## 4 United States
    ## 5 United States
    ## 6 United States

<br> <b>Question 2.a: Create a new column to display the monthly average land temperatures in Fahrenheit (°F).</b>

``` r
# New column having monthly average US land temperatures in Fahrenheit
UStemp$Monthly.AverageTemp.F <- UStemp$Monthly.AverageTemp * 1.8 + 32
```

<b>Question 2.b: Calculate average land temperature by year and plot it. The original file has the average land temperature by month.</b>

``` r
# Calculation of average land temperature by year
# Group the US land temperature data by year and then calculate the average temp for the year
UStemp_year <- group_by(UStemp, as.numeric(format(as.Date(UStemp$Date), '%Y'))) %>% summarize(mean(Monthly.AverageTemp.F))
# Set proper column headings
UStemp_year <- setNames(UStemp_year, c("Year","Yearly.avg.temp.F"))
q <- qplot(x=UStemp_year$Year, y=UStemp_year$Yearly.avg.temp.F, xlab = "Year", ylab="Yearly average Temp(°F)", main="Average US land temperature by year") + scale_x_continuous(breaks = seq(1900,2017, 13),minor_breaks=NULL) + scale_y_continuous(breaks = seq(45,53,0.5),minor_breaks=NULL, limits=c(45,54)) + theme(plot.title = element_text(hjust = 0.5))
# Save the plot
ggsave(filename = "graphs/Average US land temperature by year.png", device="png", plot=q)
```

    ## Saving 7 x 5 in image

``` r
# Display the plot in the markdown file
```

![](graphs/Average%20US%20land%20temperature%20by%20year.png) <br> Looking at the graph, it can be said that the average US land temperature does not follow any trend. Its randomly distributed and but the overall picture suggests that there is an increase in the average US land temperatures throughout these years. Also since the last few years, the average temperature is increasing continously indicating the climate change. Since 2008, the average US land temperature is increasing. <br> <br> <b>Question 2.c: Calculate the one year difference of average land temperature by year and provide the maximum difference (value) with corresponding two years.</b>

``` r
# Calculation of the one year difference of average land temperature by year
# For this, create a data frame having first column as the years whose temperature difference is taken (format of 'YYYY - YYYY') and the 
#  second column is the difference in temperatures
year_difference_of_temp <- (sapply(1:(length(UStemp_year$Year)-1),function(x){paste(as.character(UStemp_year$Year[x+1]), '-', as.character(UStemp_year$Year[x]))})) %>% as.data.frame %>% setNames(c("Years"))
year_difference_of_temp$temp_diff <- sapply(1:(length(UStemp_year$Yearly.avg.temp.F)-1), function(x){diff(UStemp_year$Yearly.avg.temp.F[x:(x+1)])})
# Write this data having yearly US average temperature variation by country into the data folder with file 
#  name as difference_of_average_US_land_temperature_by_year.txt
write.table(year_difference_of_temp, 'data/difference_of_average_US_land_temperature_by_year.txt', sep = "\t", row.names = FALSE, quote = FALSE)

# maximum difference (value)
index_of_max_value <- which(year_difference_of_temp$temp_diff == max(abs(year_difference_of_temp$temp_diff)))
paste('Maximum difference in average US land temperature for two consecutive years is ' , abs(year_difference_of_temp$temp_diff[index_of_max_value]), 
      '°F for year ' , year_difference_of_temp$Years[index_of_max_value], sep='')
```

    ## [1] "Maximum difference in average US land temperature for two consecutive years is 2.5401°F for year 1921 - 1920"

<br> Maximum difference in average US land temperature for two consecutive years is 2.5401°F for year 1921 - 1920 <br> <br> <b>Question 3: Download “CityTemp” data set at box.com. Find the difference between the maximum and the minimum temperatures for each major city and report/visualize top 20 cities with maximum differences for the period since 1900.</b>

``` r
# CityTemp data already downloaded and is in the dataset namely cleaned_city_temp
# Find the top 20 cities with maximum difference in maximum and minimum temperatures for the period since 1900
city_temp_since_1900 <- cleaned_city_temp[as.Date(cleaned_city_temp$Date)>=as.Date('1900-01-01'),] 
# Merging City name with Country so as to view Country names as well with the city names.
city_temp_since_1900$City <- paste(city_temp_since_1900$City,' (', city_temp_since_1900$Country, ')' , sep='')
top_20_city_temp_diff_since_1900 <- summarize(group_by(city_temp_since_1900, City,Country),  max(Monthly.AverageTemp) - min(Monthly.AverageTemp)) %>%
  as.data.frame() %>% setNames(c("City","Country","max_and_min_Temp_difference_since_1900")) %>% arrange(desc(max_and_min_Temp_difference_since_1900)) %>% head(n=20)
# View the data of top 20 cities with maximum differences (difference between maximum and the minimum temperatures) for the period since 1900
top_20_city_temp_diff_since_1900
```

    ##                         City       Country
    ## 1             Harbin (China)         China
    ## 2          Changchun (China)         China
    ## 3            Moscow (Russia)        Russia
    ## 4           Shenyang (China)         China
    ## 5          Montreal (Canada)        Canada
    ## 6             Kiev (Ukraine)       Ukraine
    ## 7  Saint Petersburg (Russia)        Russia
    ## 8           Toronto (Canada)        Canada
    ## 9            Taiyuan (China)         China
    ## 10            Peking (China)         China
    ## 11           Tianjin (China)         China
    ## 12       Seoul (South Korea)   South Korea
    ## 13            Mashhad (Iran)          Iran
    ## 14            Dalian (China)         China
    ## 15   Chicago (United States) United States
    ## 16          Tangshan (China)         China
    ## 17  New York (United States) United States
    ## 18            Baghdad (Iraq)          Iraq
    ## 19          Berlin (Germany)       Germany
    ## 20             Jinan (China)         China
    ##    max_and_min_Temp_difference_since_1900
    ## 1                                  53.281
    ## 2                                  49.844
    ## 3                                  43.956
    ## 4                                  43.045
    ## 5                                  41.422
    ## 6                                  40.784
    ## 7                                  40.510
    ## 8                                  38.683
    ## 9                                  37.834
    ## 10                                 36.953
    ## 11                                 36.953
    ## 12                                 35.783
    ## 13                                 35.610
    ## 14                                 35.223
    ## 15                                 34.962
    ## 16                                 34.833
    ## 17                                 34.460
    ## 18                                 34.047
    ## 19                                 33.920
    ## 20                                 33.778

``` r
# To avoid the automatic alphabetic ordering of the data while plotting the City, explicitly specify the order of the data and then plot the graph.
top_20_city_temp_diff_since_1900$City <- factor(top_20_city_temp_diff_since_1900$City,levels=top_20_city_temp_diff_since_1900$City[order(desc(top_20_city_temp_diff_since_1900$max_and_min_Temp_difference_since_1900))])
q <- qplot(x=top_20_city_temp_diff_since_1900$City,y=top_20_city_temp_diff_since_1900$max_and_min_Temp_difference_since_1900, xlab="City (Country)", ylab="Maximum Tempeature variation since 1900 (°C)", main="Top 20 Cities with Maximum Average Temperature Variation", ylim = c(0,55)) + theme(axis.text.x = element_text(angle = 90,hjust=1, vjust=0.5),plot.title = element_text(hjust = 0.5))
# Save the plot
ggsave(filename = "graphs/Maximum Temp Variation-City.png", device="png", plot=q)
```

    ## Saving 7 x 5 in image

``` r
# Display the plot in the markdown file
```

![](graphs/Maximum%20Temp%20Variation-City.png) <br> Based on the above result, it can be said that top 20 cities based on the Maximum variation in monthly average temperature are having the variation in the range of around 35 to 53. <br> <br> <b>Question 4: Compare the two graphs in (i) and (iii) and comment it.</b> On comparison of the two graphs, it can be viewed that the maximum variation in average temperatures for cities are on average higher than the differences in average temperatures for whole cities. Also notice that not all the top 20 countries appear in the top 20 city graph. 9 of the top 20 cities are located in China. Also countries like United states, South Korea, Iran, Germany though not a part of the top 20 countries is having a city in the top 20 city list. This is bacause cities with higher population densities have higher levels of pollution. Also for cities like New York, Chicago, Toronto are there in the list because the cities and countries have the largest difference in average temperatures as they get very very cold in the winter and during summer, though the temperature is not that high, the difference in the two is very high compared to others. <br> <br> <b>Conclusion</b> <br>
<p>
Average yearly US land temperature does not follow any trend in particular with overall indication in rise in temperature. This may be due to the various confounding variables like polulation, pollution, rainfall, urbanization etc. So the relationship between year and average yearly temperature cannot be derived without considering the confounding factors. Amongst the top 20 cities based on the maximum variation in the monthly average temperatures, there are many cities belonging to the same country - 9 belong to China, 2 to Russia, 2 to United States and 2 to Canada. Of these though US is not in the top 20 countries, 2 cities from US are in the top 20 cities. This is likely the mean for countries like US where minimum temperature in one part of country is very different then the other part, the average temperature is settled.
</p>
