#######################
### Auds Drake Plan ###
#######################

### LOAD LIBRARIES
library("drake")
library("tidyverse")
library("readxl")
library("lubridate")
library("rdrop2")
library("e1071")
#devtools::install_github("richardjtelford/traitstrap")
library("traitstrap")
#devtools::install_github("Between-the-Fjords/dataDownloader")
library("dataDownloader")

# tricks
pn <- . %>% print(n = Inf)

### DRAKE CONFIGURATIONS
pkgconfig::set_config("drake::strings_in_dots" = "literals")


### IMPORT FUNCTION FILES
source("R/ImportAndClean_China.R")
source("R/ImportAndClean_Peru.R")
source("R/ImportAndClean_Svalbard.R")
source("R/ImportAndClean_Norway.R")
source("R/ImportAndClean_Colorado.R")
source("R/ImportAndClean_Database.R")

source("R/CommunityWeightedTraitMeans.R")

source("R/MakePrettyFigures.R")

#source("R/CWTM_Bootstrapping.R")


### IMPORT DRAKE PLANS
source("R/PrettyImportPlan.R")
source("R/DataProcessingPlan.R")
source("R/MakePrettyFiguresPlan.R")





### COMBINING THE DRAKE PLANS 
MasterDrakePlan <-  bind_rows(ImportDrakePlan, 
                              CountryListAndTraitMeanDrakePlan) 
#CWTraitMeanDrakePlan

#configure and make drake plan
config <- drake_config(MasterDrakePlan)

config