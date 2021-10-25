
library (lubridate)
library (dplyr)
library (ggplot2)
library (VIM)
library (visdat)
library (corrplot)
library (grid)
library (gridExtra)
library (sm)
library (mice)

#LOAD DATA
df <- read.csv("df_Cairns.csv", stringsAsFactors = T)
<<<<<<< HEAD

=======
>>>>>>> 9a417b94c9ae547d75d4c8deeb8a77160d988e6d

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

#Missing Data from WIND already imputed in other .RMd file.

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

###Very few numerical data are missing, Evaporation is not strongly correlated
###with any other numeric variable. Cloud3pm is negatively correlated with Sunshine
###and positively correlated with Cloud9am and Humidity3pm. Sunshine is negatively
###correlated with the Humidity and Cloud measurements.


#CHECK NAs FOR EVAPORATION
#create variable to identify rows that are missing Evaporation
na_Evaporation <- which(is.na(df_NAs$Evaporation))
#create dataframe to check summary statistics of observations missing Evaporation
df_NA_Evap<- df_NAs[na_Evaporation,]
summary(df_NA_Evap)

#Plotting histograms side by side for rainfall in full data set vs on days when
#Evaporation is NA
c1 <- rgb(173,216,230, max=255, alpha=80, names = "lt.blue") #create transparent color
c2 <- rgb(255,192,203, max=255, alpha=80, names = "lt.pink") # create transparent color

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

#CHECK NAs FOR SUNSHINE
#create variable to identify rows that are missing Sunshine
na_Sunshine <- which(is.na(df_NAs$Sunshine))
#check rows of observations missing Sunshine
df_NAs[na_Sunshine,]

###The missing values for Sunshine do not appear random. All days are in Summer, 
###and on days with rainfall, high cloud cover, or both.

#CHECK NAs FOR WIND GUST SPEED
#create variable to identify rows that are missing WindGustSpeed
na_GustSpeed <- which(is.na(df_NAs$WindGustSpeed))
#check rows of observations missing WindGustSpeed
df_NAs[na_GustSpeed,]

#Plotting histograms side by side for WindSpeed3pm in full data set vs on days when
#WindGustSpeed is NA

Speed3pm_full <- df$WindSpeed3pm
Speed3pm_wgNA <- df_NAs[na_GustSpeed,]$WindSpeed3pm
b <- min(c(Speed3pm_full,Speed3pm_wgNA), na.rm=TRUE) # Set the minimum for the breakpoints
e <- max(c(Speed3pm_full,Speed3pm_wgNA), na.rm=TRUE) # Set the maximum for the breakpoints
ax <- pretty(b:e, n=20) # Make a neat vector for the breakpoints
hgSpeed3pm <- hist(Speed3pm_full, breaks = ax, plot = FALSE) # Save first histogram data
hgSpeed3pm_wgNA <- hist(Speed3pm_wgNA, breaks = ax, plot = FALSE) # Save 2nd histogram data
plot(hgSpeed3pm_wgNA, col=c2, freq=FALSE, main="Side by Side Histograms for Wind Speed at 3pm",
     xlab="Wind Speed at 3pm",) # Plot 1st histogram using a transparent color
plot(hgSpeed3pm, col=c1, add=TRUE,freq=FALSE) # Add 2nd histogram using different color
legend("topleft", c("All Days", "Days with WindGustSpeed NA"), fill=c(c1,c2))

### WindGustSpeed is potentially missing randomly, however WindGustDir is only missing
### when WindGustSpeed is missing.

#CHECK NAs FOR CLOUD
#create variable to identify rows that are missing Cloud9am
na_Cloud9 <- which(is.na(df_NAs$Cloud9am))
#check rows of observations missing Cloud9am
df_NAs[na_Cloud9,]

#create variable to identify rows that are missing Cloud3pm
na_Cloud3 <- which(is.na(df_NAs$Cloud3pm))
#check rows of observations missing Cloud3pm
df_NAs[na_Cloud3,]

###Cloud data is potentially missing randomly. The days that have data missing at 9am
###have data available at 3pm and vice versa.

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
xyplot(tempData,Evaporation ~ Humidity9am+Humidity3pm,pch=18,cex=1)
xyplot(tempData,Evaporation ~ Rainfall,pch=18,cex=1)
densityplot(tempData)

summary(completedData)


#Compare density plots of original data with completed data
plot(density(df$Evaporation, na.rm=TRUE), col='red')
lines(density(completedData$Evaporation), col='blue')

plot(density(df$WindGustSpeed, na.rm=TRUE), col='red')
lines(density(completedData$WindGustSpeed), col='blue')

plot(density(df$Sunshine, na.rm=TRUE), col='red')
lines(density(completedData$Sunshine), col='blue')

plot(density(df$Cloud9am, na.rm=TRUE), col='red')
lines(density(completedData$Cloud9am), col='blue')

plot(density(df$Cloud3pm, na.rm=TRUE), col='red')
lines(density(completedData$Cloud3pm), col='blue')

plot(density(df$Rainfall, na.rm=TRUE), col='red')
lines(density(completedData$Rainfall), col='blue')

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
#identify rows which are missing RainToday & Rainfall > .01 and impute "Yes"
na_RainYes <- which(is.na(df$RainToday) & (df$Rainfall > .01))
df[na_RainYes,]$RainToday <- "Yes"
#identify dates from imputed values
dates_RainYes <- df[na_RainYes,]$Date
#identify dates before imputed values
dates_RainTomYes <- df[na_RainYes,]$Date - 1 
#identify rows for day before imputed Rain and impute "Yes"
na_RainTomYes <- which(df$Date == dates_RainTomYes)
df[na_RainTomYes,]$RainTomorrow <- "Yes"

summary(df)

#All NAs are filled, write new .csv to use for imputing time series.
#write.csv(df,'df_Cairns_MICE.csv', row.names = FALSE)

#https://www.earthdatascience.org/courses/earth-analytics/time-series-data/summarize-time-series-by-month-in-r/
# plot rainfall as time series
df %>%
  ggplot(aes(x = Date, y = Rainfall)) +
  geom_point(color = "darkorchid4") +
  labs(title = "Rainfall",
       y = "Daily rainfall (mm)",
       x = "Date") + theme_bw(base_size = 15)


