## Manage Packages
if(require(pacman)==FALSE) install.packages("pacman")
pacman::p_load(readxl,fpp2,ggplot2,scales,dplyr, skimr, DataExplorer)


#Reading in the data
setwd("M:/ISA 616/ISA616BikeShareAnalysis/BikeShare Workflow")
bikeshare <- read.csv("Bike Share Data.csv", stringsAsFactors = TRUE)


#Data Description Code

str(bikeshare)

#Splitting date and time column into a separate date column and numeric time column

bikeshare$Date <- sapply(strsplit(as.character(bikeshare$datetime), " "), "[", 1)
bikeshare$Time <- sapply(strsplit(as.character(bikeshare$datetime), " "), "[", 2)
bikeshare$Date <- as.Date(bikeshare$datetime)
bikeshare$Time <- as.factor(bikeshare$Time)
bikeshare$Time <- as.numeric(bikeshare$Time)

#recoding other variables as factors 
bikeshare$season <- factor(bikeshare$season)
bikeshare$holiday <- factor(bikeshare$holiday)
bikeshare$workingday <- factor(bikeshare$workingday)
bikeshare$weather <- factor(bikeshare$weather)

#Removing original datetime column
library(dplyr)
bikeshare<-select(bikeshare, -datetime)


str(bikeshare)

library(skimr)
skim(bikeshare)

source('data summary.R')
data.summary(bikeshare)
)
