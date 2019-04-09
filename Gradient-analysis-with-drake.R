# Load libraries
#library("devtools")
#install_github("ropensci/drake")
library("drake")
library("tidyverse")
library("readxl")
library("lubridate")
library("rdrop2")
library("e1071")

pn <- . %>% print(n = Inf)

# drake configurations
pkgconfig::set_config("drake::strings_in_dots" = "literals")

#import scripts
source("R/ImportAndClean_China.R")
source("R/ImportAndClean_Peru.R")
source("R/ImportAndClean_Svalbard.R")
source("R/ImportAndClean_Norway.R")
source("R/ImportAndClean_Colorado.R")
source("R/CommunityWeightedTraitMeans.R")
source("R/MakePrettyFigures.R")

# Functions to download and load data from dropbox and google drive
#drop_acc()

# Import codes from data base do not work!
# some of the flux data is wrong


### NEEDS DOING!!!
# CH
# China gradient comm
# CH remove high nitrogen values
# China data: Lichen2 and Lichen have different values...?
# same for moss and bryophytes.

# NO
# NO: duplicate cover values -> Ragnhild har kontroll.

# CO
# Lorah CO species names

# SV
### Check turfs with low cover of traits:
# communitySV %>% filter(PlotID %in% c("3BC", "6CD", "5BB")) %>% arrange(PlotID) %>% pn
### Check 3BC ranunculus nivalis 43% Cover


# GENERAL
# scale before CWM
# Small leaves and light leaves: set a threshold?

### Data checks that need to be done
# do sp names match? comm and trait
# check high trait values, and remove outliers
# are there zeros in the comm data that need to be removed
# do site, block, plot, etc. names match in all the different data sets?
# is.na(Trait): trait name
# NAs in trait data should be removed
# Why are NaN after log transformation?




### Import Data
ImportDrakePlan <- drake_plan(

  Data_CH = ImportClean_China(),
  Data_PE = ImportClean_Peru(),
  Data_SV = ImportClean_Svalbard(),
  Data_NO = ImportClean_Norway(),
  Data_CO = ImportClean_Colorado()
)

# make an analysis drake plan
AnalysesDrakePlan <- drake_plan(
  
  # make a list with all data sets
  CountryList = list(China = Data_CH,
                     Peru = Data_PE,
                     Svalbard = Data_SV,
                     Norway = Data_NO,
                     Colorado = Data_CO) %>% 
    map(LogTranformation)  #Log transforming trait data (height, mass and area)
  
  
  #### CALCULATIONS, ANALYSES, FIGURES
  
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

)


# combine import and analysis to master drake plan
MasterDrakePlan <- ImportDrakePlan %>% 
  bind_rows(AnalysesDrakePlan)

#configure and make drake plan
config <- drake_config(MasterDrakePlan)
# outdated(config)        # Which targets need to be (re)built?
make(MasterDrakePlan)          # Build the right things.
loadd()
readd(GradientMeanPlot)
readd(GradientVarPlot)
readd(GradientSkewPlot)
readd(GradientKurtPlot)

#view dependency graph
vis_drake_graph(config, targets_only = TRUE)
