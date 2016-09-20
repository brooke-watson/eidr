#Exploratory analysis of EIDR dataset 

library(foreign)
library(tidyverse)
library(ggplot2)
library(gridExtra)

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

##central tendancy values for number of infected and number of deaths 
meaninfxd <- sum(eidr2$Number.Infected, na.rm = TRUE)/nrow(eidr2) #3
meandeath <- sum(eidr2$Number.of.Deaths, na.rm = TRUE)/nrow(eidr2) #0
medinfxd <- median(eidr2$Number.Infected, na.rm = TRUE) #10295.2 
meddeath <- median(eidr2$Number.of.Deaths, na.rm = TRUE) # 62.4

#arranged, ordered sequence variables for plotting one-dimensional histograms  
eidr.i <- arrange(eidr2, Number.Infected)
I.Count <- seq_along(eidr.i$Number.Infected)  

eidr.d <- arrange(eidr2, Number.of.Deaths)
D.Count <- seq_along(eidr.d$Number.of.Deaths) #another sequence variable 
 

 
#plotting 
gi <- ggplot(eidr.i, aes(I.Count, Number.Infected))+geom_point()
gd <- ggplot(eidr.d, aes(D.Count, Number.of.Deaths))+geom_point()
grid.arrange(gi, gd, nrow=2, top = "histograms of infection and death frequencies in EIDs")

#just deaths 
gd <- ggplot(eidr.d, aes(D.Count, Number.of.Deaths))
gd+geom_point()
