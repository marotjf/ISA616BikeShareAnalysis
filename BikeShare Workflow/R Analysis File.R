## Manage Packages
if(require(pacman)==FALSE) install.packages("pacman")
pacman::p_load(readxl,fpp2,ggplot2,scales,dplyr, skimr, DataExplorer,corrplot)


#Reading in the data
setwd("M:/ISA 616/ISA616BikeShareAnalysis/BikeShare Workflow")
bikeshare <- read.csv("Bike Share Data.csv", stringsAsFactors = TRUE)


#Data Description Code

str(bikeshare)

#Splitting date and time column into a separate date column and numeric time column

bikeshare$Date <- sapply(strsplit(as.character(bikeshare$datetime), " "), "[", 1)
bikeshare$Time <- sapply(strsplit(as.character(bikeshare$datetime), " "), "[", 2)
bikeshare$Date <- as.Date(bikeshare$datetime,format='%m/%d/%Y')
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


#str(bikeshare)



#source('data summary.R')
#data.summary(bikeshare)



#PREPROCESSING STEPS

#Creating Dummies for Factors

dum<-as.data.frame(model.matrix(~0+bikeshare$season))
colnames(dum)<-c("spring", "summer", "fall", "winter")
bikeshare<-cbind(bikeshare, dum[,-1])
#Possibly remove seasonal attribute


dum4<-as.data.frame(model.matrix(~0+bikeshare$weather))
colnames(dum4)<-c("Clear", "Mist", "Light Snow/Rain", "Heavy Rain/Snow")
bikeshare<-cbind(bikeshare, dum4[,-1])

bikeshare<-select(bikeshare, -season)
bikeshare<-select(bikeshare, -weather)
str(bikeshare)

#Outlier Detection

boxplot(bikeshare$count)
Outliers = boxplot(bikeshare$count)$out
Outliers
which(bikeshare$count %in% Outliers)


#Plots to look at data
plot(as.Date(bikeshare$Date), bikeshare$count)

library(corrplot)
nums<-unlist(lapply(bikeshare, is.numeric))
M<-cor(bikeshare[,nums], use="complete.obs")
corrplot(M, method="circle")


#BEGINNNING OF Analysis
