
library (lubridate)
library (dplyr)
library (ggplot2)
library (VIM)
library( visdat)
library (corrplot)
library (grid)
library (gridExtra)
library (sm)
library (mice)
library (tidyverse)

#LOAD DATA
df <- read.csv("Location CSVs/df_Brisbane.csv", stringsAsFactors = T)

#CHANGE VARIABLE TYPE
df$Date <- as.Date(df$Date)

#LIST VARIABLES
str(df)

#SUMMARIZE DATA BY COLUMN
summary(df)

#HISTOGRAMS AND CORRELATION PLOTS FOR COMPLETE NUMERIC OBSERVATIONS
nums <- unlist(lapply(df, is.numeric)) 
numrain_data<-df[ , nums]
numrain_data<-subset(numrain_data,complete.cases(numrain_data))
a<-names(numrain_data)
a<-as.list(a)

fun02<-function(i){index=grep(i,names(numrain_data))
bw <- nclass.Sturges(numrain_data[,index]) # Freedman-Diaconis
nm=paste0(i)
assign(paste("g",i,sep=""),
       ggplot(numrain_data, aes(numrain_data[,index])) +  
         geom_histogram(bins = bw,aes(y=..density..), fill="#de2d26") +
         geom_density(alpha=.35, fill="#08519c",color = NA)  +
         geom_vline (aes(xintercept=median(numrain_data[,index])),color="#08519c", size=1) + 
         labs(title=nm, x=NULL, y="UPAS")) +
  theme(plot.title = element_text(size = rel(0.7),face ="bold",hjust = 0.5),
        axis.title.y = element_text(size = rel(0.4)),
        axis.text = element_text(size = rel(0.4)))
}
Histos<-lapply(a[-1],fun02)
do.call(grid.arrange, Histos)

cor_matrix = cor(numrain_data[,c(4:19)]) #starting from column 4: MinTemp
corrplot(cor_matrix, method = 'number') # colorful number

#DETECT MISSING DATA

#Missing Data from WIND DIRECTION already imputed in other .RMd file.

#Detecting missing data by row
mis_ind = rowSums(is.na(df))
m1<-which(mis_ind>0)
df_NAs <- df[m1,]
table(mis_ind)

# Detecting Missing Values by Column as count
mis_col <- colSums(is.na(df))
mis_col


#Detecting Missing Dates from date range
date_range <- seq(min(df$Date), max(df$Date), by = 1) 
missing_dates <- data.frame(date = date_range[!date_range %in% df$Date]) 
###It appears we are missing April 2011, December 2012, and February 2013

# Missing Data Graphs
vis_miss(df)

aggr(df, col=c('grey','#252525'), numbers=TRUE, sortVars=TRUE, labels=names(df), 
     cex.axis=.5, gap=1, ylab=c("Missing data"," "),border=NA)

###Sunshine is missing the most observations, at around 2.5%
###Rainfall is missing 14 observations, less than 1%.

#CHECK NAs FOR SUNSHINE
#create variable to identify rows that are missing Sunshine
na_Sunshine <- which(is.na(df_NAs$Sunshine))
#check rows of observations missing Sunshine
df_NAs[na_Sunshine,]
#check for completeness other than sunshine
which((df_NAs[na_Sunshine,]) == which(mis_ind>1))

###The missing values for Sunshine do not appear random, but don't look related
###to other missing data. It appears to happen on consecutive days at different
###time periods, but almost all of February 2011 is missing. The other variables 
###are complete to be able to come up with consistent multivariate imputations for sunshine.

#CHECK NAs FOR WIND GUST SPEED
#create variable to identify rows that are missing WindGustSpeed
na_GustSpeed <- which(is.na(df_NAs$WindGustSpeed))
#check rows of observations missing WindGustSpeed
df_NAs[na_GustSpeed,]

#Plotting histograms side by side for WindSpeed3pm in full data set vs on days when
#WindGustSpeed is NA
c1 <- rgb(173,216,230, max=255, alpha=80, names = "lt.blue") #create transparent color
c2 <- rgb(255,192,203, max=255, alpha=80, names = "lt.pink") # create transparent color


Speed3pm_full <- df$WindSpeed3pm
Speed3pm_wgNA <- df_NAs[na_GustSpeed,]$WindSpeed3pm
b <- min(c(Speed3pm_full,Speed3pm_wgNA), na.rm=TRUE) # Set the minimum for the breakpoints
e <- max(c(Speed3pm_full,Speed3pm_wgNA), na.rm=TRUE) # Set the maximum for the breakpoints
ax <- pretty(b:e, n=10) # Make a neat vector for the breakpoints
hgSpeed3pm <- hist(Speed3pm_full, breaks=ax, plot = FALSE) # Save first histogram data
hgSpeed3pm_wgNA <- hist(Speed3pm_wgNA,breaks = ax, plot = FALSE) # Save 2nd histogram data
plot(hgSpeed3pm_wgNA, col=c2, freq=FALSE, main="Side by Side Histograms for Wind Speed at 3pm",
     xlab="Wind Speed at 3pm",) # Plot 1st histogram using a transparent color
plot(hgSpeed3pm, col=c1, add=TRUE,freq=FALSE) # Add 2nd histogram using different color
legend("topright", c("All Days", "Days with WindGustSpeed NA"), fill=c(c1,c2))

#Check if the value of WindGustDir is only unknown on days when wind gust speed is missing.
all(which(df$WindGustDir == "unkn") == which(is.na(df$WindGustSpeed)))

### WindGustSpeed is missing more often when the Wind Speed at 3pm is low, so the wind
### gusts may not have been strong enough to record a speed that day, however there are
### also wind gust speeds missing on other days with higher wind which could be random.
### WindGustDir is only missing when WindGustSpeed is missing.

#CHECK NAs FOR RAINFALL
#create variable to identify rows that are missing Rainfall
na_Rainfall <- which(is.na(df_NAs$Rainfall))
#create dataframe to check summary statistics of observations missing Evaporation
df_NA_Rain<- df_NAs[na_Rainfall,]
summary(df_NA_Rain)

#Plotting histograms side by side for sunshine in full data set vs on days when
#Rainfall is NA

sun_full <- df$Sunshine
sun_rain <- df_NA_Rain$Sunshine
b <- min(c(sun_full,sun_rain), na.rm=TRUE) # Set the minimum for the breakpoints
e <- max(c(sun_full,sun_rain), na.rm=TRUE) # Set the maximum for the breakpoints
ax <- pretty(b:e) # Make a neat vector for the breakpoints
hgSun <- hist(sun_full, breaks = ax, plot = FALSE) # Save first histogram data
hgSun_rain <- hist(sun_rain, breaks = ax, plot = FALSE) # Save 2nd histogram data
plot(hgSun_rain, col=c2, freq=FALSE, main="Side by Side Histograms for Sunshine",
     xlab="Sunshine per Day",) # Plot 1st histogram using a transparent color
plot(hgSun, col=c1, add=TRUE,freq=FALSE) # Add 2nd histogram using different color
legend("topleft", c("All Days", "Days with Rainfall NA"), fill=c(c1,c2))

###Rainfall is missing on a variety of dates which appear to be random. Only one instance occurs
###on consecutive days. May 6, 2010 which has many other values missing for that same day.
###When compared to sunshine, the Rainfall is missing on days with a variety of sunshine levels
###but more frequently when sunshine is higher.

#CHECK NAs FOR EVAPORATION
#create variable to identify rows that are missing Evaporation
na_Evaporation <- which(is.na(df_NAs$Evaporation))
#create dataframe to check summary statistics of observations missing Evaporation
df_NA_Evap<- df_NAs[na_Evaporation,]
summary(df_NA_Evap)

#Plotting histograms side by side for rainfall in full data set vs on days when
#Evaporation is NA

rain_full <- df$Rainfall
rain_evap <- df_NA_Evap$Rainfall
b <- min(c(rain_full,rain_evap), na.rm=TRUE) # Set the minimum for the breakpoints
e <- max(c(rain_full,rain_evap), na.rm=TRUE) # Set the maximum for the breakpoints
ax <- pretty(b:e, n=20) # Make a neat vector for the breakpoints
hgRainfall <- hist(rain_full, breaks = ax, plot = FALSE) # Save first histogram data
hgRainfall_Evap <- hist(rain_evap, breaks = ax, plot = FALSE) # Save 2nd histogram data
plot(hgRainfall, col=c1, freq=FALSE, main="Side by Side Histograms for Rainfall",
     xlab="Rainfall per Day (mm)",) # Plot 1st histogram using a transparent color
plot(hgRainfall_Evap, col=c2, add=TRUE,freq=FALSE) # Add 2nd histogram using different color
legend("topright", c("All Days", "Days with Evaporation NA"), fill=c(c1,c2))

#Plotting histograms side by side for Humidity3pm in full data set vs on days when
#Evaporation is NA

humid3_full <- df$Humidity3pm
humid3_evap <- df_NA_Evap$Humidity3pm
b <- min(c(humid3_full,humid3_evap), na.rm=TRUE) # Set the minimum for the breakpoints
e <- max(c(humid3_full,humid3_evap), na.rm=TRUE) # Set the maximum for the breakpoints
ax <- pretty(b:e, n=20) # Make a neat vector for the breakpoints
hgHumid3 <- hist(humid3_full, breaks = ax, plot = FALSE) # Save first histogram data
hgHumid3_Evap <- hist(humid3_evap, breaks = ax, plot = FALSE) # Save 2nd histogram data
plot(hgHumid3_Evap, col=c2, freq=FALSE, main="Side by Side Histograms for Humidity at 3pm",
     xlab="Relative Humidity Percentage at 3pm",) # Plot 1st histogram using a transparent color
plot(hgHumid3, col=c1, add=TRUE,freq=FALSE) # Add 2nd histogram using different color
legend("topleft", c("All Days", "Days with Evaporation NA"), fill=c(c1,c2))

#Plotting histograms side by side for Humidity9am in full data set vs on days when
#Evaporation is NA

humid9_full <- df$Humidity9am
humid9_evap <- df_NA_Evap$Humidity9am
b <- min(c(humid9_full,humid9_evap), na.rm=TRUE) # Set the minimum for the breakpoints
e <- max(c(humid9_full,humid9_evap), na.rm=TRUE) # Set the maximum for the breakpoints
ax <- pretty(b:e, n=20) # Make a neat vector for the breakpoints
hgHumid9 <- hist(humid9_full, breaks = ax, plot = FALSE) # Save first histogram data
hgHumid9_Evap <- hist(humid9_evap, breaks = ax, plot = FALSE) # Save 2nd histogram data
plot(hgHumid9_Evap, col=c2, freq=FALSE, main="Side by Side Histograms for Humidity at 9am",
     xlab="Relative Humidity Percentage at 9am",) # Plot 1st histogram using a transparent color
plot(hgHumid9, col=c1, add=TRUE,freq=FALSE) # Add 2nd histogram using different color
legend("topleft", c("All Days", "Days with Evaporation NA"), fill=c(c1,c2))


###The missing values for Evaporation are clearly not random. They tend to occur on days
###with higher than average rainfall and humidity. In theory, the higher the relative humidity,
###the lower the evaporation should be.

#CHECK NAs FOR HUMIDITY3PM
#create variable to identify rows that are missing Evaporation
na_Humid3 <- which(is.na(df_NAs$Humidity3pm))
#create dataframe to check summary statistics of observations missing Evaporation
df_NA_Humid3<- df_NAs[na_Humid3,]
summary(df_NA_Humid3)

#Plotting histograms side by side for rainfall in full data set vs on days when
#Humidity3pm is NA

rain_full <- df$Rainfall
rain_Humid3 <- df_NA_Humid3$Rainfall
b <- min(c(rain_full,rain_Humid3), na.rm=TRUE) # Set the minimum for the breakpoints
e <- max(c(rain_full,rain_Humid3), na.rm=TRUE) # Set the maximum for the breakpoints
ax <- pretty(b:e, n=20) # Make a neat vector for the breakpoints
hgRainfall <- hist(rain_full, breaks = ax, plot = FALSE) # Save first histogram data
hgRainfall_Humid3 <- hist(rain_Humid3, breaks = ax, plot = FALSE) # Save 2nd histogram data
plot(hgRainfall, col=c1, freq=FALSE, main="Side by Side Histograms for Rainfall",
     xlab="Rainfall per Day (mm)",) # Plot 1st histogram using a transparent color
plot(hgRainfall_Humid3, col=c2, add=TRUE,freq=FALSE) # Add 2nd histogram using different color
legend("topright", c("All Days", "Days with Humidity3pm NA"), fill=c(c1,c2))

### There are multiple cases where the Temperature, Pressure, and Humidity
### measurements at 3pm are simultaneously missing. The dates appear random, 
### but there is a high tendency for thesemeasurements to be missing on the 
### day before a Rainfall measurement is missing. In 8 out of 9 cases the 
### 9am measurements are available, so there is good data available for imputation.
### The missing values for Humidity do not seem to be correlated with the amount of
### rainfall on that given day.

#CHECK NAs FOR MIN & MAX TEMPERATURE

#create variable to identify rows that are missing MinTemp
na_MinTemp <- which(is.na(df_NAs$MinTemp))
#check rows of observations missing MinTemp
df_NAs[na_MinTemp,]

###The 5 instances where minimum temperature is missing do not appear random. They are
###usually missing on the last day of the month and in January. 3 of the 5 instances 
### are also missing rainfall. 2 of the 5 instances are also missing MaxTemp. 
###Temp3pm and Sunshine are available in all cases. Temp9am is available in 4
###out of 5 cases.

#create variable to identify rows that are missing MaxTemp
na_MaxTemp <- which(is.na(df_NAs$MaxTemp))
#check rows of observations missing MaxTemp
df_NAs[na_MaxTemp,]

###In 4 out of 5 cases, Temp3pm is available when MaxTemp is missing. Sunshine is 
###available in all cases. Cloud measurements are also available in all cases.
###In all cases where MaxTemp is missing, WindGustSpeed and WindGustDir are also missing.
###May 6, 2010 is one of the days which has a lot of missing values.

#CHECK NAs for Temp9am

#create variable to identify rows that are missing MaxTemp
na_Temp9 <- which(is.na(df_NAs$Temp9am))
#check rows of observations missing MaxTemp
df_NAs[na_Temp9,]

###One of the days where Temp9am is missing is May 6,2010 which has many missing values.
###The other day is January 31, 2012 which has some missing values, but Temp3pm, Sunshine
### and Cloud measurements are avaialable.

#CHECK NAs FOR CLOUD

#create variable to identify rows that are missing Cloud3pm
na_Cloud3 <- which(is.na(df_NAs$Cloud3pm))
#check rows of observations missing Cloud3pm
df_NAs[na_Cloud3,]

###There are only two missing values for Cloud3pm and it appears to be random.

#create variable to identify rows that are missing Cloud9am
na_Cloud9 <- which(is.na(df_NAs$Cloud9am))
#check rows of observations missing Cloud9am
df_NAs[na_Cloud9,]

###There are no missing values for Cloud9am.


#IMPUTE MISSING VALUES

###'calm' added to wind levels in other .csv

##create variable to identify rows that are missing wind direction at 9am
#na_WindDir9am <- which(is.na(df$WindDir9am))
##check summary of wind speed at 9am for rows that are missing wind direction at 9am
#summary(df[na_WindDir9am,]$WindSpeed9am)
#### All values of WindDir9am can be imputed to 'Calm'
##imputation using row identifier
#df$WindDir9am[na_WindDir9am] <- 'Calm'

##create variable to identify rows that are missing wind direction at 3pm
#na_WindDir3pm <- which(is.na(df$WindDir3pm))
##check summary of wind speed at 3pm for rows that are missing wind direction at 3pm
#summary(df[na_WindDir3pm,]$WindSpeed3pm)
#### All values of WindDir3pm can be imputed to 'Calm'
##imputation using row identifier
#df$WindDir3pm[na_WindDir3pm] <- 'Calm'

#Copy original data before completing data with MICE imputations.
df_original <- df

# IMPUTATION BY MICE

md.pattern(df)

tempData <- mice(df[,c("Month","MinTemp","MaxTemp","Rainfall","Evaporation",
                       "Sunshine","WindGustDir","WindGustSpeed","WindDir9am",
                       "WindDir3pm","WindSpeed9am","WindSpeed3pm","Humidity9am",
                       "Humidity3pm","Pressure9am","Pressure3pm","Cloud9am",
                       "Cloud3pm","Temp9am","Temp3pm")],m=3,set.seed(2))
tempData$meth
summary(tempData)
completedData <- complete(tempData,1)
xyplot(tempData,Sunshine ~ Temp3pm+MaxTemp+Cloud3pm,pch=18,cex=1)
xyplot(tempData,WindGustSpeed ~ WindSpeed9am+WindSpeed3pm,pch=18,cex=1)
xyplot(tempData,Rainfall ~ Sunshine+Cloud9am+Cloud3pm,pch=18,cex=1)
xyplot(tempData,Evaporation ~ Humidity9am+Humidity3pm,pch=18,cex=1)
xyplot(tempData,Evaporation ~ Rainfall,pch=18,cex=1)
densityplot(tempData)

summary(completedData)


#Compare density plots of original data with completed data
plot(density(df$Sunshine, na.rm=TRUE), col='red')
lines(density(completedData$Sunshine), col='blue')

plot(density(df$WindGustSpeed, na.rm=TRUE), col='red')
lines(density(completedData$WindGustSpeed), col='blue')

plot(density(df$Rainfall, na.rm=TRUE), col='red')
lines(density(completedData$Rainfall), col='blue')

plot(density(df$Humidity3pm, na.rm=TRUE), col='red')
lines(density(completedData$Humidity3pm), col='blue')

plot(density(df$Temp3pm, na.rm=TRUE), col='red')
lines(density(completedData$Temp3pm), col='blue')

plot(density(df$Pressure3pm, na.rm=TRUE), col='red')
lines(density(completedData$Pressure3pm), col='blue')

plot(density(df$Evaporation, na.rm=TRUE), col='red')
lines(density(completedData$Evaporation), col='blue')

plot(density(df$MinTemp, na.rm=TRUE), col='red')
lines(density(completedData$MinTemp), col='blue')

plot(density(df$MaxTemp, na.rm=TRUE), col='red')
lines(density(completedData$MaxTemp), col='blue')

plot(density(df$Humidity9am, na.rm=TRUE), col='red')
lines(density(completedData$Humidity9am), col='blue')

plot(density(df$Cloud3pm, na.rm=TRUE), col='red')
lines(density(completedData$Cloud3pm), col='blue')

plot(density(df$Temp9am, na.rm=TRUE), col='red')
lines(density(completedData$Temp9am), col='blue')

plot(density(df$Pressure9am, na.rm=TRUE), col='red')
lines(density(completedData$Pressure9am), col='blue')



##Cloud9am was not missing values
#plot(density(df$Cloud9am, na.rm=TRUE), col='red')
#lines(density(completedData$Cloud9am), col='blue')





###Density plots of imputed variables match density plots of original variables
###with NA's removed.


#Replace original data with completed data from MICE
df[,c("Month","MinTemp","MaxTemp","Rainfall","Evaporation",
      "Sunshine","WindGustDir","WindGustSpeed","WindDir9am",
      "WindDir3pm","WindSpeed9am","WindSpeed3pm","Humidity9am",
      "Humidity3pm","Pressure9am","Pressure3pm","Cloud9am",
      "Cloud3pm","Temp9am","Temp3pm")] <- completedData

#IMPUTE RAINTODAY & RAINTOMORROW

#Fill RainToday & RainTomorrow NAs with appropriate values based on Rainfall.
#identify rows which are missing RainToday & Rainfall > 1mm and impute "Yes"
na_RainYes <- which(is.na(df$RainToday) & (df$Rainfall >= 1))
df[na_RainYes,]$RainToday <- "Yes"
#identify dates from imputed values
dates_RainYes <- df[na_RainYes,]$Date
#identify dates before imputed values
dates_RainTomYes <- df[na_RainYes,]$Date - 1 
#identify rows for day before imputed Rain and impute "Yes"
na_RainTomYes <- which(df$Date == dates_RainTomYes)
df[na_RainTomYes,]$RainTomorrow <- "Yes"

#identify rows which are missing RainToday & Rainfall < 1mm and impute "No"
na_RainNo <- which(is.na(df$RainToday) & (df$Rainfall < 1))
df[na_RainNo,]$RainToday <- "No"
#identify dates from imputed "No" values
dates_RainNo <- df[na_RainNo,]$Date
#identify dates before imputed "No" values
dates_RainTomNo <- df[na_RainNo,]$Date - 1 
#identify rows for for which RainTomorrow is still missing
na_RainTom <- which(is.na(df$RainTomorrow))
#Check that all dates of missing are equivalent to the dates where Rain Tomorrow
#should be "No" then impute "No"
if (all(dates_RainTomNo == df[na_RainTom,c("Date")])) {
  df[na_RainTom,]$RainTomorrow <- "No"
}

summary(df)

#All NAs are filled, write new .csv to use for imputing time series.
#write.csv(df,'Location CSVs/df_Brisbane_MICE.csv', row.names = FALSE)

#https://www.earthdatascience.org/courses/earth-analytics/time-series-data/summarize-time-series-by-month-in-r/
# plot rainfall as time series
df %>%
  ggplot(aes(x = Date, y = Rainfall)) +
  geom_point(color = "darkorchid4") +
  labs(title = "Rainfall",
       y = "Daily rainfall (mm)",
       x = "Date") + theme_bw(base_size = 15)


###Importing data set after time series imputations.
df_Brisbane <- read.csv("Completed_Location_CSVs/df_Brisbane_completed.csv", stringsAsFactors = T)
df_Brisbane$Date <- as.Date(df_Brisbane$Date)

#Create function to plot density for original and imputed data
#accepts the attribute name as string,city as string, and legend location as string.
compare_densities <- function(data1, data2, attribute, city, legloc) {
  plot(density(data1[,attribute], na.rm=TRUE), col='darkblue', lwd=3,
       main=attribute, sub=city)
  lines(density(data2[,attribute]), col='orange')
  legend(x= legloc,legend=c("original", "imputed"),
         col = c('darkblue','orange'), lty=c(1,1), 
         lwd=c(3,1), cex=0.7)
}


city <- "Brisbane"
compare_densities(df_original, df_Brisbane, "Evaporation", city, "topright")
compare_densities(df_original, df_Brisbane, "WindGustSpeed", city, "topright")
compare_densities(df_original, df_Brisbane, "Sunshine", city, "topleft")
compare_densities(df_original, df_Brisbane, "Cloud9am", city, "topleft")
compare_densities(df_original, df_Brisbane, "Cloud3pm", city, "topleft")
compare_densities(df_original, df_Brisbane, "Rainfall", city, "topright")
compare_densities(df_original, df_Brisbane, "MinTemp", city, "topright")
compare_densities(df_original, df_Brisbane, "MaxTemp", city, "topright")
compare_densities(df_original, df_Brisbane, "Humidity9am", city, "topright")
compare_densities(df_original, df_Brisbane, "Humidity3pm", city, "topright")
compare_densities(df_original, df_Brisbane, "Temp9am", city, "topright")
compare_densities(df_original, df_Brisbane, "Temp3pm", city, "topright")
compare_densities(df_original, df_Brisbane, "Pressure9am", city, "topleft")
compare_densities(df_original, df_Brisbane, "Pressure3pm", city, "topleft")
compare_densities(df_original, df_Brisbane, "WindSpeed9am", city, "topright")
compare_densities(df_original, df_Brisbane, "WindSpeed3pm", city, "topright")

