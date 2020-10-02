## Manage Packages
if(require(pacman)==FALSE) install.packages("pacman")
pacman::p_load(readxl,fpp2,ggplot2,scales,dplyr, skimr, DataExplorer)





#Reading in the data
setwd("M:/ISA 616/ISA616BikeShareAnalysis/BikeShare Workflow")
bikeshare <- read.csv("Bike Share Data.csv", stringsAsFactors = TRUE)


#Data Description Code
library(skimr)
skim(bikeshare)



str(bikeshare)

bikeshare$season <- factor(ebay.data$Competitive, levels=c(0,1), labels=c("Not Competitive", "Competitive"))
bikeshare$holiday <- factor(bikeshare$holiday)
bikeshare$workingday <- factor(bikeshare$workingday)
bikeshare$weather <- factor(bikeshare$weather)
str(bikeshare)

ebay.data$Competitive <- factor(ebay.data$Competitive, levels=c(0,1), labels=c("Not Competitive", "Competitive"))

create_report(df)
