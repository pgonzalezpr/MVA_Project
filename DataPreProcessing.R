# CLEARING ENVIRONMENT #

rm(list=ls(all=TRUE))

# IMPORTS #

library(rstudioapi)
library(tidyverse)

# SETTING WORKSPACE #

current_path <- getActiveDocumentContext()$path 
setwd(dirname(current_path ))

# LOADING DATASET AND ADDING ID COLUMN #

rain_data <- read.csv("weatherAUSOriginal.csv")
n <- nrow(rain_data)
ID = c(1:n)
new_rain_data <- add_column(rain_data, ID, .before=1)

# SAVING NEW DATASET #

write.table(new_rain_data, file = "weatherAUS.csv", sep = ",", na = "NA", dec = ".", row.names = FALSE, col.names = TRUE)



