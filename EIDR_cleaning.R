#AMR_data_bare

# Step 1: Importing database and loading libraries.

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

# load data
rm(list=ls())
setwd("~/Dropbox (EHA)")
path <- paste0(getwd(), "/AMR/data/EIDR Spreadsheet.xlsx")
list = excel_sheets(path)

source('https://gist.githubusercontent.com/brooke-watson/e03c8a785fe69cbef42a7ebfe9e24613/raw/5782d93be6b49307988a211231ee5f309298cf63/multassign.R')

g(event, pathogen, host, location, col_names) %=%
  lapply(excel_sheets(path)[c(1:4, 6)], read_excel, path=path)

event = filter(event, eidVal == 1)
# merge data
all = merge(event, host, by='eventName')
  all = merge(all, pathogen, by='eventName')
  all = merge(all, location, by='eventName')
  all[] <- lapply(all, factor)

# select variables
all = all %>%
  dplyr::select(eventName, eidCategoryVal, eidVal, numberInfectedVal, eidCategoryPrecis,
                refAbstract, driverVal,
                diseaseVal, startDateISOVal, numberDeathsVal, zoonoticVal, testingMethodVal,
                DomesticationStatusVal, pathogenTypeVal, pathogenDrugResistanceVal,
                pathogenDrugResistancePrecis, pathogenSpeciesVal, pathogenFamilyVal,
                pathogenOrderVal, DomesticationStatusVal, hostVal,
                hostClassVal, hostFamilyVal, hostSpeciesVal, hostTaxOrderVal,
                locationNationVal, locationContinentVal )

all = unique(all) 

lapply(oldeidr, summary) 

tab = tabyl(all$pathogenDrugResistanceVal)

# get number of unique values per variable
uniques = lapply(all, unique) %>% lapply(length) %>% as.data.frame()


# get counts of the NAs in a column
nas = function(col, db) {
  sum(is.na(db[col]))
}

test = nas(all, 1)

# summary stats for the numeric variables
nums = lapply(all, as.numeric)

oldclass = lapply(oldeidr, as.numeric)  

table(oldclass)

test= lapply((1:ncol(all)), nas, db = all)
test= lapply((1:ncol(nums)), nas, db = nums)
nums = nums[ , (sum(is.na(nums)) < 200)]
nums = nums %>% as.data.frame()

 
# get numeric vals for the ones for which that makes sense 
all$year = lapply(all$startDateISOVal, substr, start = 1, stop = 4) 
all$year = as.numeric(all$year)
sum(is.na(all$year)) #72 
table(all$year)
qplot(all$year, binwidth = 5)
summary(all$year)
   # Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   # 1940    1970    1980    1980    2000    2010      72 

EIDR: 

# ## exploring data with datacomb
# devtools::install_github('cmpolis/datacomb', subdir='pkg', ref='1.1.2')
# library(datacomb)
# Datacomb(all)

