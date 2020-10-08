## Manage Packages using pacman librabry
if(require(pacman)==FALSE) install.packages("pacman")
pacman::p_load(readxl,fpp2,ggplot2,scales,dplyr, forecast, rsample, rpart, rpart.plot, ipred, caret, DT, deseasonalize )

#Reading in the data
setwd("M:/ISA 616/ISA616BikeShareAnalysis/BikeShare Workflow")
bikeshare <- read.csv("Bike Share Data.csv", stringsAsFactors = TRUE)


###############################################################################################

#Data Description Code

str(bikeshare)
#calling data summary function that is within github
source('data summary.R')
data.summary(bikeshare)

#printing head and tail of data in table
DT::datatable(head(bikeshare, 10))
DT::datatable(tail(bikeshare, 10))

#################################################################################

#PREPROCESSING STEPS

#Splitting date and time column into a separate numeric date column and numeric time column
bikeshare$Date <- sapply(strsplit(as.character(bikeshare$datetime), " "), "[", 1)
bikeshare$Time <- sapply(strsplit(as.character(bikeshare$datetime), " "), "[", 2)
bikeshare$Date <- as.Date(bikeshare$datetime,format='%m/%d/%Y')
bikeshare$Time <- as.factor(bikeshare$Time)
bikeshare$Time <- as.numeric(bikeshare$Time)

str(bikeshare)

#recoding other variables as factors 
bikeshare$quarter <- factor(bikeshare$season) #quarter is a more appropriate name for this variable
bikeshare$holiday <- factor(bikeshare$holiday)
bikeshare$workingday <- factor(bikeshare$workingday)
bikeshare$weather <- factor(bikeshare$weather)

#Removing original datetime column using select function in dplyr
bikeshare<-select(bikeshare, -datetime)
bikeshare<-select(bikeshare, -season)


#Creating Dummy variables for multilevel Factors

dum<-as.data.frame(model.matrix(~0+bikeshare$quarter))
colnames(dum)<-c("1Q", "2Q", "3Q", "4Q")
bikeshare<-cbind(bikeshare, dum[,-1])

dum1<-as.data.frame(model.matrix(~0+bikeshare$weather))
colnames(dum1)<-c("Clear", "Mist", "Light_SnowRain", "Heavy_SnowRain")
bikeshare<-cbind(bikeshare, dum1[,-1])

bikeshare<-select(bikeshare, -quarter)
bikeshare<-select(bikeshare, -weather)


#Plotting Count over Days

bikeshare$Date <- as.POSIXct.Date(bikeshare$Date) #using this variable just for the plot
dfplot =data.frame(select(bikeshare,Date, count))

PLOT<-function(x){
  theme_set(theme_gray(base_size = 14))
  label=colnames(x)[2]
  g=ggplot(x, aes(Date, x[,2])) 
  g=g+ geom_line()
  g = g + scale_x_datetime(name = "Date", 
                           breaks = date_breaks("70 days"),
                           labels = date_format(format = "%b %d"),
                           expand = c(0,0))
  g=g+xlab("") + ylab(paste0("Count of Rentals",label))
  print(g)
}

PLOT(dfplot)

bikeshare<-select(bikeshare, -Time)
bikeshare<-select(bikeshare, -Date)

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

sstr(bikeshare.train)
#For this analysis we are looking at overall count. Therefore we need to take casual and registered out of the data frame for the analysis because count is the sum of these variables
bikeshare.train<-select(bikeshare.train, -registered)
bikeshare.train<-select(bikeshare.train, -casual)

bikeshare.valid<-select(bikeshare.valid, -registered)
bikeshare.valid<-select(bikeshare.valid, -casual)



#first fitting model with all variables entered. Needed to write it out long ways because cannot include registered and casual when predicting count
full<-lm(count~ holiday+ workingday+ temp +atemp + humidity+ windspeed+Clear+Mist+Light_SnowRain+Heavy_SnowRain,data=bikeshare.train)

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

#Regression Tree Analysis

#split into testing and validation
set.seed(13)
trainIndex = sample(1:nrow(bikeshare), size = round(0.7*nrow(bikeshare)), replace=FALSE)
head(trainIndex, 10)

bikeshare.train<-bikeshare[trainIndex, ]
bikeshare.valid<-bikeshare[-trainIndex, ]
nrow(bikeshare.train)


bikeshare.train<-select(bikeshare.train, -count)
bikeshare.valid<-select(bikeshare.valid, -count)


#best days for casual riders to be out, so that you can send them promos to become registered member
library(rsample)     # data splitting 
library(dplyr)       # data wrangling
library(rpart)       # performing regression trees
library(rpart.plot)  # plotting regression trees
library(ipred)       # bagging
library(caret)       # bagging

m1 <- rpart(
  formula = casual ~ holiday + workingday + temp + humidity + windspeed + summer + fall + winter, 
  data    = bikeshare.train,
  method  = "anova"
)
rpart.plot(m1)

# Specify 10-fold cross validation
ctrl <- trainControl(method = "cv",  number = 10) 

# CV bagged model
bagged_cv <- train(
  casual ~ holiday + workingday + temp + humidity + windspeed + summer + fall + winter,
  data = bikeshare.train,
  method = "treebag",
  trControl = ctrl,
  importance = TRUE
)

# assess results
bagged_cv


# plot most important variables
plot(varImp(bagged_cv), 20)  

pred <- predict(bagged_cv, bikeshare.valid)
RMSE(pred, bikeshare.valid$casual)
#34.28095

#if temp >23, it is a workingday, and humidity is less than or equal to 56
#even if humidity is less than 71 still have avg of 91 riders