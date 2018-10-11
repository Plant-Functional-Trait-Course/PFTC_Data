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

#import scripts
source("R/CommunityWeightedTraitMeans.R")
source("R/MakePrettyFigures.R")
source("DataImportDrakePlan.R")

# Functions to download and load data from dropbox and google drive
drop_acc()



### NEEDS DOING!!!
# Colorado: trait and height
#China gradient comm

### Check turfs with low cover of traits:
# communitySV %>% filter(PlotID %in% c("3BC", "6CD", "5BB")) %>% arrange(PlotID) %>% pn
### Check 3BC ranunculus nivalis 43% Cover
# CH remove high nitrogen values
# scale before CWM

# check PE sp names sp2 sp_2


# make an analysis drake plan
analyses_plan <- drake_plan(
  strings_in_dots = "literals",
  
  
  #### CALCULATIONS, ANALYSES, FIGURES
  # Simple CWM
  #TraitMeans_All = CountryList %>% 
    #map_df(CommunityW_GlobalAndLocalMeans) %>% 
    #left_join(metaBioclim),
  #CWTraitMeans = CommunityW_Means(TraitMeans_All),
  
  # Transformation
  CountryList_trans = CountryList %>% 
    map(LogTranformation),
  
  # Bootstrapped CWM
  CW_Moments_Bootstrapped = CountryList_trans %>% 
    map_df(CWM_Bootstrapping)
    

  
  #CW_Means_Bootstrapped_Bio = CW_Means_Bootstrapped %>% 
    #left_join(metaAll, by = c("Country", "PlotID")) %>% 
    #left_join(metaBioclim, by = c("Country", "Site")),

  #GradientPlot = MakeFigure(TraitMeans),
  #GradientMeanPlot = MakeMeanFigure(CW_Means_Bootstrapped_Bio),
  #GradientVarPlot = MakeVarFigure(CW_Means_Bootstrapped_Bio),
  #GradientSkewPlot = MakeSkewFigure(CW_Means_Bootstrapped_Bio),
  #GradientKurtPlot = MakeKurtFigure(CW_Means_Bootstrapped_Bio)

)


# combine import and analysis to master drake plan
MasterDrakePlan <- dataImport_plan %>% 
  bind_rows(analyses_plan)

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
