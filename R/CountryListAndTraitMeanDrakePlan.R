# Load libraries
#library("devtools")
#install_github("ropensci/drake")
library("drake")
library("tidyverse")
library("readxl")
library("lubridate")
library("rdrop2")
library("e1071")
#devtools::install_github("richardjtelford/traitstrap")
library("traitstrap")
#remotes::install_github("centerforopenscience/osfr")
#devtools::install_github("Between-the-Fjords/dataDownloader")
library("dataDownloader")


pn <- . %>% print(n = Inf)

# drake configurations
pkgconfig::set_config("drake::strings_in_dots" = "literals")

#import scripts
source("R/ImportAndClean_China.R")
source("R/ImportAndClean_Peru.R")
source("R/ImportAndClean_Svalbard.R")
source("R/ImportAndClean_Norway.R")
source("R/ImportAndClean_Colorado.R")
source("R/ImportAndClean_Database.R")
source("R/CommunityWeightedTraitMeans.R")
source("R/MakePrettyFigures.R")

# Functions to download and load data from dropbox and google drive
#drop_acc()

# Import codes from data base do not work!
# some of the flux data is wrong


### NEEDS DOING!!!

### RAW DATA
# CH
# Check taxon between comm and trait: Aud
# Later: China comm meta data: Lichen2 and Lichen have different values...?
# same for moss and bryophytes.

# NO
# comm: I removed rows where Taxon was NA -> Ragnhild
# comm: I removed 3 duplicate species at SKJ PlotID Skj3XC ->


# CO
# Check plot and block and site id, if that matches in comm and trait

# SV
### Check turfs with low cover of traits:
# communitySV %>% filter(PlotID %in% c("3BC", "6CD", "5BB")) %>% arrange(PlotID) %>% pn
### Check 3BC ranunculus nivalis 43% Cover
# -> Aud


# GENERAL
# Put in thresholds for traits: SLA > xxx etc.
# Small leaves and light leaves: set a threshold?
# graphical checks and with literature


# scale before CWM, before log, before calculate mean?
# Talk to Richard


### Data checks that need to be done
# do sp names match? comm and trait
# are there zeros in the comm data that need to be removed
# do site, block, plot, etc. names match in all the different data sets?
# is.na(Trait): trait name
# Check if there are 0 or NAs in trait. NA should be removed, 0 should be NA. Then no NaN in log transformation.


# CHECK CODE
# Check if remove NA after CWM is necessary.
# Calc CWM for some species to check if code is working

# NEW THINGS
# Bootstraping
# 80% of commnity




### Import Data
ImportDrakePlan <- drake_plan(

  Data_CH = ImportClean_China(),
  Data_PE = ImportClean_Peru(),
  Data_SV = ImportClean_Svalbard(),
  Data_NO = ImportClean_Norway(),
  Data_CO = ImportClean_Colorado(),
  Database0 = ImportClean_Database()
  
)

# make an analysis drake plan
CountryListAndTraitMeanDrakePlan <- drake_plan(
  
  # make a list with all data sets
  CountryList = list(China = Data_CH,
                     Peru = Data_PE,
                     Svalbard = Data_SV,
                     Norway = Data_NO,
                     Colorado = Data_CO,
                     Database = Database0) %>%
    map(LogTransformation),  #Log transforming trait data (height, mass and area)
  
  # make a list with all data sets but without Database
  CountryList_WD = list(China = Data_CH,
                     Peru = Data_PE,
                     Svalbard = Data_SV,
                     Norway = Data_NO,
                     Colorado = Data_CO) %>%
    map(LogTransformation),  #Log transforming trait data (height, mass and area)

  
  #Calculating trait means
  TraitMeans = map_df(CountryList, CalculateTraitMeans)
) 

