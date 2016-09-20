#Exploratory analysis of EIDR dataset 

library(foreign)
library(tidyverse)
library(ggplot2)

#setting working directory to find file 
wd <- "~/Users/Watson/Documents/EIDR/"
setwd(wd)

#reading in EIDR database and doing some exploratory analysis 
eidr <- read.csv(file = "eidr.csv", header = TRUE, stringsAsFactors = FALSE)
dim(eidr)
names(eidr)

#selecting variables of interest
eidr2 <- dplyr::select(eidr, Event.Name:Start.Date, Number.Infected, Number.of.Deaths,
                       Zoonotic..Not.Event.Specific.,
        Average.Age.of.Infected:Life.Expectancy.in.Country.in.the.first.Year.of.Event)

#converting factors to integers
for (i in c(9,10,12)){
  eidr2[,i] <- gsub("years", "", eidr2[,i], perl=TRUE)
  eidr2[,i] <- 
}

#adding NAs  
for (i in 1:12){
  eidr2[,i] <- gsub("Not Applicable", "NA", eidr2[,i], perl=TRUE)
  eidr2[,i] <- gsub("Not Found", "NA", eidr2[,i], perl=TRUE)
}


#making infection and death rates numeric 
for (i in c(6:7, 12)){
  eidr2[,i] <- as.numeric(as.character(eidr2[,i]))
} 
 
#creating a "case fatality rate" variable
eidr2 <- mutate(eidr2, Fatality.Rate = Number.of.Deaths/Number.Infected)

#summing missing values 
missing.infected <- sum(is.na(eidr2[,6])) # 44 missing values for number of infected 
missing.deaths <- sum(is.na(eidr2[,7])) # 107 missing values for number of fatalities 
missing.fatality.rate <- sum(is.na(eidr2[,13])) #113 

