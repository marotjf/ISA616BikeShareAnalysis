setwd("M:/BikeShare")

df <- read.csv("Copy of Bike_sharing Project Data.csv", stringsAsFactors = TRUE)

class(df$season)


library(DataExplorer)


str(df)

df$season <- factor(df$season)
df$holiday <- factor(df$holiday)
df$workingday <- factor(df$workingday)
df$weather <- factor(df$weather)


create_report(df)
