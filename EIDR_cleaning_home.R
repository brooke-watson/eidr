#EIDR database - master spreadsheet 

# Importing database and loading libraries.
library(knitr)
library(readxl)
library(dplyr)
library(xtable)
library(stringr)
library(devtools)
library(xda)
library(plotly)
library(janitor)
library(stargazer)
library(stringi)
library(plyr)
       

# load data
rm(list=ls())
setwd("..")
path <- paste0(getwd(), "/EIDR Spreadsheet.xlsx")
list = excel_sheets(path)

#source & run the function that will assign multiple objects to the global environment at once 
source('https://gist.githubusercontent.com/brooke-watson/e03c8a785fe69cbef42a7ebfe9e24613/raw/5782d93be6b49307988a211231ee5f309298cf63/multassign.R')
g(event, pathogen, host, location, col_names) %=%
  lapply(excel_sheets(path)[c(1:4, 6)], read_excel, path=path)   # loading excel files 

# filter out non-EIDs 
event = filter(event, eidVal == 1)


# merge data
all = merge(event, host, by='eventName') 
all = merge(all, pathogen, by='eventName')
all = merge(all, location, by='eventName')
all = filter(all, eidVal == 1)
  

# select variables
all2 = all %>%
  dplyr::select(eventName, eidCategoryVal, eidVal, numberInfectedVal, eidCategoryPrecis,
                refAbstract, driverVal,
                diseaseVal, startDateISOVal, numberDeathsVal, zoonoticVal, testingMethodVal,
                pathogenTypeVal, pathogenFamilyVal, pathogenDrugResistanceVal,
                pathogenOrderVal, DomesticationStatusVal, hostVal,
                hostClassVal, hostFamilyVal, hostSpeciesVal, hostTaxOrderVal,
                locationNationVal, locationContinentVal ) %>% 
                unique()  
all = all2

  
#character munging 
all = lapply(all, trimws) %>% # trim white space
    lapply(tolower) %>% #standardize capitalization 
    as.data.frame(stringsAsFactors = FALSE) # turn back into a data frame 


# get number of unique values per column and set it as an attribute 
# variables with a ton of unique values I'm not going to bother with for the time being - 
# the frequency tables just aren't going to be interesting.

un = lapply(all, unique) %>% sapply(length)     # int vector, n of unique values per variable
attr(all, "uniques") = un                       # setting uniques as an attribute varaiable 
summary(un)                                     # range of unique values 
names(all[attr(all, "uniques") >  109] )        # seeing at a glance which ones are huge (top quartile)
# all = all[attr(all, "uniques") <  109 | attr(all, "names") == 'eventName']          # subsetting just the reasonable ones + uniqueID. 


# get numeric vals for the ones for which that makes sense 
all$year = lapply(all$startDateISOVal, substr, start = 1, stop = 4) 
all$year = as.numeric(all$year)
all = dplyr::select(all, -startDateISOVal)

# create binary dummy variables from lists 
source(dummyv.R)
all = dummyv(all, 'hostVal', 'human')
unique(all$driverVal)


#fixing spelling of host class values  
unique(all2$hostClassVal)
# [1] "mammalia"       "aves"           "Mammalia"       NA               "Aves"          
# [6] "Mammalia "      "Insecta"        "mammalia "      "Actinopteri;"   "Arachnida"     
# [11] "Mamalia"        "Gastropoda"     "Actinopterygii"
 
mam = agrep(pattern = "mammalia", x = all$hostClassVal, ignore.case = TRUE, value = FALSE, max.distance = 3)
act = agrep(pattern = "actinopterygii", x = all$hostClassVal, ignore.case = TRUE, value = FALSE, max.distance = 5)

all[mam, "hostClassVal"] = "mammalia"
all[act, "hostClassVal"] = "actinopterygii"
 
  


# get number of unique values per variable
uniques = lapply(all, unique) %>% lapply(length) %>% as.data.frame()


# get counts of the NAs in a column
nas = function(col, db) {
  sum(is.na(db[col]))
}

test = nas(all, 1)

# summary stats for the numeric variables
nums = lapply(all, as.numeric)
lapply(nums, summary) # so most things are character vectors 

test= lapply((1:ncol(all)), nas, db = all)
test= lapply((1:ncol(nums)), nas, db = nums)
nums = nums[ , (sum(is.na(nums)) < 200)]
nums = nums %>% as.data.frame()

 
 
 
# Plots 
    
    
    
    qplot(as.numeric(as.factor(all$pathogenTypeVal)), xlab = names(as.factor(all$pathogenTypeVal))
          
          
