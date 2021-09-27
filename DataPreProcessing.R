# CLEARING ENVIRONMENT #

rm(list=ls(all=TRUE))

# IMPORTS #

library(rstudioapi)
library(tidyverse)

# SETTING WORKSPACE #

current_path <- getActiveDocumentContext()$path 
setwd(dirname(current_path ))

# LOADING DATASET #

rain_data <- read_csv("weatherAUSOriginal.csv")

# DATE FORMATTING AND SUBSETTING #

rain_data$Date <- as.Date(rain_data$Date)

rain_data <- filter(rain_data, Date < "2013-06-02" & Location %in% c("Sydney","AliceSprings","Brisbane","Cairns","Perth","Moree"))

# MISSING DATA #

missing_stats <- colSums(is.na(rain_data))*100/nrow(rain_data)




# ADDING ID COLUMN AND SAVING NEW DATASET #

# ID = c(1:nrow(rain_data))
# rain_data <- add_column(rain_data, ID, .before=1)
# write.table(rain_data, file = "weatherAUS.csv", sep = ",", na = "NA", dec = ".", row.names = FALSE, col.names = TRUE)




