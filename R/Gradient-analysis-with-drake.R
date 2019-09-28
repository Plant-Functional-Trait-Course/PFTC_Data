# Load libraries
#library("devtools")
#install_github("ropensci/drake")
library("drake")
library("tidyverse")
library("readxl")
library("lubridate")
library("rdrop2")
library("e1071")
library("readr")

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
# CH remove high nitrogen values: CountryList$China$trait %>% filter(Trait == "N_percent", Value > 7) %>% pn: Aud
# China data: Lichen2 and Lichen have different values...?
# same for moss and bryophytes.

# NO
# NO: duplicate cover values -> group_by and distinct: Ragnhild, make comment to remove when we get new comm data.

# CO
# Check species name in comm and trait if they are the same. Some sp1, need to get real names.
# Check plot and block and site id, if that matches in comm and trait
# Lorah CO species names
# -> Ragnhild

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
AnalysesDrakePlan <- drake_plan(
  
  # make a list with all data sets
  CountryList = list(China = Data_CH,
                     Peru = Data_PE,
                     Svalbard = Data_SV,
                     Norway = Data_NO,
                     Colorado = Data_CO,
                     Database = Database0) %>%
    map(LogTransformation),  #Log transforming trait data (height, mass and area)

  
  #### CALCULATIONS, ANALYSES, FIGURES
  #Calculating trait means
  # Countries
  TraitMeans = map_df(CountryList, CalculateTraitMeans),
  
  
  #Calculating community weighted trait means
  Full_CWTraitMeans = CommunityW_TraitMeans(CountryList, TraitMeans),
  CWTMeans = CommunityW_Means(Full_CWTraitMeans)
)
  
  
  
  # Bootstrapped CWM
  #BootstrapMoments_All = CountryList_trans %>% 
   # map_df(CWM_Bootstrapping),
    
  # Summarize Bootstrap Moments
  #BootstrapMoments = SummarizeBootMoments(BootstrapMoments_All),

  
  #BootstrapMoments_Bio = BootstrapMoments %>% 
   # left_join(metaBioclim, by = c("Country", "Site")),

  #GradientPlot = MakeFigure(BootstrapMoments),
  #GradientMeanPlot = MakeMeanFigure(CW_Means_Bootstrapped_Bio),
  #GradientVarPlot = MakeVarFigure(CW_Means_Bootstrapped_Bio),
  #GradientSkewPlot = MakeSkewFigure(CW_Means_Bootstrapped_Bio),
  #GradientKurtPlot = MakeKurtFigure(CW_Means_Bootstrapped_Bio)

#)


# combine import and analysis to master drake plan
MasterDrakePlan <- ImportDrakePlan %>% 
  bind_rows(AnalysesDrakePlan)

#configure and make drake plan
config <- drake_config(MasterDrakePlan)

config