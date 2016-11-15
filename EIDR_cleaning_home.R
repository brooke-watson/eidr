## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)

## ----Attaching Necessary Packages, warning=FALSE, message=FALSE, results='hide'----

pkgs = list('knitr', 'readxl', 'dplyr', 'xtable', 'stringr', 'devtools', 'plotly', 'janitor', 'stringi', 'xlsx', 'bookdown')
lapply(pkgs, library, character.only = TRUE)
 

## ----Setup Working Directory---------------------------------------------
rm(list=ls()) 
wd = "~/Dropbox (EHA)/GVP/EIDR/"
wd2 = '~/Desktop/git/eidr' # for when using a different computer 
setwd(wd)
path = "~/Dropbox (EHA)/AMR/data/EIDR Spreadsheet.xlsx"
path2 = paste0(getwd(), "~/Desktop/EIDR Spreadsheet.xlsx")
list = excel_sheets(path) # see names of excel sheets 

## ----Source Github function, warning=FALSE, message=FALSE----------------

source('https://gist.githubusercontent.com/brooke-watson/e03c8a785fe69cbef42a7ebfe9e24613/raw/5782d93be6b49307988a211231ee5f309298cf63/multassign.R') # find the function 
g(event, pathogen, host, location, col_names) %=%
  lapply(excel_sheets(path)[c(1:4, 6)], read_excel, path=path)   # loading specific excel sheets 

## ----Merge and Clean Datasets--------------------------------------------

event = filter(event, eidVal == 1)

# merge data
all = merge(event, host, by='eventName') 
all = merge(all, pathogen, by='eventName')
all = merge(all, location, by='eventName')

## ------------------------------------------------------------------------

all = all %>%
  dplyr::select(eventName, eidCategoryVal, eidVal, numberInfectedVal, driverVal,
                diseaseVal, startDateISOVal, numberDeathsVal, zoonoticVal, testingMethodVal,
                pathogenTypeVal, pathogenFamilyVal, pathogenDrugResistanceVal,
                pathogenOrderVal, DomesticationStatusVal, hostVal, refAbstract, 
                hostClassVal, hostFamilyVal, hostSpeciesVal, hostTaxOrderVal,
                locationNationVal, locationContinentVal,
                refEventName, refEidCategory, refAbstract, refDriver, refDisease, 
                refStartDateISO) %>% unique()
# unique(all$pathogenTypeVal)  


## ----Character Munging---------------------------------------------------

all = lapply(all, trimws) %>% # trim white space
    lapply(tolower) %>% #standardize capitalization 
    as.data.frame(stringsAsFactors = FALSE) %>% # turn back into a data frame 
    unique() # drop duplicates 
all2 = all #making a backup just in case 


## ----Ref fix-------------------------------------------------------------

refdb = all %>% 
    dplyr::select(refEventName, refEidCategory, refAbstract, refDriver, refDisease, 
                refStartDateISO) # creating a reference-specific database to compare                                         # differences. 

# get number of NAs per reference column to see which is most complete 
nums = lapply(refdb, is.na) %>% 
    lapply(sum) %>% rbind() %>% print() 

## ------------------------------------------------------------------------
 NAdb = filter(refdb, is.na(refEidCategory)) # checking those with no references 
 # View(NAdb)

## ---- message=FALSE, warning=FALSE---------------------------------------
if (is.na(refdb$refEidCategory) & !is.na(refdb$refAbstract)){
    refdb$refEidCategory = refdb$refAbstract
} else {refdb$refEidCategory = refdb$refEidCategory}

## ------------------------------------------------------------------------
all = dplyr::select(all, -c(refEidCategory, refAbstract, refDriver, refDisease, refStartDateISO)) # remove reference columns 

all$ref = gsub( ",.*$", "", refdb$refEidCategory) %>% as.integer() 
# add single numeric reference column 
rm(NAdb)


## ---- warning=FALSE, message=FALSE---------------------------------------

refs = read_excel(paste0(getwd(), "/EIDR References.xlsx"), sheet = 1) %>% 
    janitor::clean_names()
# install_github('brooke-watson/thesis') 
# library)
refs = select(refs, c(rights, author, title, publication_title, doi, issn, url, access_date)) %>% 
    dplyr::rename(ref = rights) %>% 
    lapply(trimws) %>% # trim white space
    lapply(tolower) %>% #standardize capitalization 
    as.data.frame(stringsAsFactors = FALSE)     

all = merge(all, refs, by="ref")


## ----Checking Duplicates-------------------------------------------------
dupes = get_dupes(all, eventName)  
# View(dupes) 

## ----Creating Attributes-------------------------------------------------
un = lapply(all, unique) %>% sapply(length)     # int vector, n of unique values per variable
attr(all, "uniques") = un                       # setting uniques as an attribute varaiable 
summary(un)                                     # range of unique values 
names(all[attr(all, "uniques") >  109] )        # seeing at a glance which ones are huge (top quartile)
# all = all[attr(all, "uniques") <  109 | attr(all, "names") == 'eventName']          
# subsetting just the reasonable ones + uniqueID. 

## ----As.Numeric(), warning=FALSE, message=FALSE, error=FALSE-------------
# creating a year variable and removing the startdateiso variable from the data frame. 
all$year = lapply(all$startDateISOVal, substr, start = 1, stop = 4) 
all$year = as.numeric(all$year)
all = dplyr::select(all, -startDateISOVal)

## ----Head Host Val-------------------------------------------------------
head(all$hostVal, 6)

## ----Encoding the Data---------------------------------------------------
# create binary dummy variables from lists 
# wd is set in the "Setup Working Directory" chunk - flip between the two as necessary 
source(paste0(getwd(), '/dummyv.R'))  # change the path file 
source(paste0(getwd(), '/spellcheck.R'))  # change the path file 

       # (((( don't forget to make these a function later  ))))
       
       
all = dummyv('human', all, 'hostVal') #testing a human binary variable 
# all = dummyvh('human') # custom dummyv variable for host 


head(all$hostVal, 10)

# all = all2
#tail(all$hostVal)
# all = spellcheckh('pig', 'swine', max.dist = 1)
# tail(all$hostVal)

## ------------------------------------------------------------------------
# hostlist = list('human', 'primate', 'pig', 'rodent', 'bat', 'cat', 'dog', 'sheep', 'goat')
# 
#  fix this later  

## ----Fix Spelling--------------------------------------------------------
mam = agrep(pattern = "mammalia", x = all$hostClassVal, ignore.case = TRUE, value = FALSE, max.distance = 3) 
act = agrep(pattern = "actinopterygii", x = all$hostClassVal, ignore.case = TRUE, value = FALSE, max.distance = 5) 
  
all[mam, "hostClassVal"] = "mammalia" 
all[act, "hostClassVal"] = "actinopterygii"  

## ---- eval=FALSE---------------------------------------------------------
## unique(all$hostClassVal)

