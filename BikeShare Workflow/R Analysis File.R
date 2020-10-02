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
colnames(dum4)<-c("Clear", "Mist", "Light_SnowRain", "Heavy_SnowRain")
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


#BEGINNNING OF Analysis - Splitting into Training and Validation -Predicting Full Count

#split into testing and validation
set.seed(13)
trainIndex = sample(1:nrow(bikeshare), size = round(0.7*nrow(bikeshare)), replace=FALSE)
head(trainIndex, 10)

bikeshare.train<-bikeshare[trainIndex, ]
bikeshare.valid<-bikeshare[-trainIndex, ]
nrow(bikeshare.train)


#For this analysis we are looking at overall count. Therefore we need to take casual and registered out of the data frame for the analysis because count is the sum of these variables
bikeshare.train<-select(bikeshare.train, -registered)
bikeshare.train<-select(bikeshare.train, -casual)

bikeshare.valid<-select(bikeshare.valid, -registered)
bikeshare.valid<-select(bikeshare.valid, -casual)

#???Deseasonalizing DatA???



#first fitting model with all variables entered. Needed to write it out long ways because cannot include registered and casual when predicting count
full<-lm(count~.,data=bikeshare.train)

summary(full)


#Next using stepwise function to create multiple regression for total Count

null<-lm(bikeshare.train$count~1, data=bikeshare.train)
lm.step<-step(null, scope=list(lower=null, upper=full), direction="both", trace=0)#trace=0 suppresses output.  Change trace=1 or 2 to get more
summary(lm.step)

#Evaluate Model

library(forecast)
p.full<-predict(full, newdata=bikeshare.valid)
p.step<-predict(lm.step, newdata=bikeshare.valid)
head(p.full)


accuracy(p.step, bikeshare.valid$count)

########################################################

#Analysis for to predict # of registered riders

set.seed(13)
trainIndex = sample(1:nrow(bikeshare), size = round(0.7*nrow(bikeshare)), replace=FALSE)
head(trainIndex, 10)

bikeshare.train<-bikeshare[trainIndex, ]
bikeshare.valid<-bikeshare[-trainIndex, ]
nrow(bikeshare.train)

bikeshare.train<-select(bikeshare.train, -count)
bikeshare.valid<-select(bikeshare.valid, -count)

full<-lm(registered~.,data=bikeshare.train)

summary(full)

null<-lm(bikeshare.train$registered~1, data=bikeshare.train)
lm.step<-step(null, scope=list(lower=null, upper=full), direction="both", trace=0)#trace=0 suppresses output.  Change trace=1 or 2 to get more
summary(lm.step)

#Evaluate Model

library(forecast)
p.full<-predict(full, newdata=bikeshare.valid)
p.step<-predict(lm.step, newdata=bikeshare.valid)
head(p.full)


accuracy(p.step, bikeshare.valid$registered)

########################################################

#Analysis for to predict # of casual riders

set.seed(13)
trainIndex = sample(1:nrow(bikeshare), size = round(0.7*nrow(bikeshare)), replace=FALSE)
head(trainIndex, 10)

bikeshare.train<-bikeshare[trainIndex, ]
bikeshare.valid<-bikeshare[-trainIndex, ]
nrow(bikeshare.train)

bikeshare.train<-select(bikeshare.train, -count)
bikeshare.valid<-select(bikeshare.valid, -count)

full<-lm(casual~.,data=bikeshare.train)

summary(full)

null<-lm(bikeshare.train$casual~1, data=bikeshare.train)
lm.step<-step(null, scope=list(lower=null, upper=full), direction="both", trace=0)#trace=0 suppresses output.  Change trace=1 or 2 to get more
summary(lm.step)

#Evaluate Model

library(forecast)
p.full<-predict(full, newdata=bikeshare.valid)
p.step<-predict(lm.step, newdata=bikeshare.valid)
head(p.full)


accuracy(p.step, bikeshare.valid$casual)

###############################################################

