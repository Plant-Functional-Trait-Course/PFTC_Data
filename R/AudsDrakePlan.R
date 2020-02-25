#######################
### Auds Drake Plan ###
#######################

### LOAD LIBRARIES
library("drake")
library("tidyverse")
library("readxl")
library("lubridate")
library("broom")
library("rdrop2")
library("e1071")
#devtools::install_github("richardjtelford/traitstrap")
library("traitstrap")
#devtools::install_github("Between-the-Fjords/dataDownloader")
library("dataDownloader")
library("vegan")
library("lme4")
library("ggvegan")
library("grid")

# tricks
pn <- . %>% print(n = Inf)

### DRAKE CONFIGURATIONS
pkgconfig::set_config("drake::strings_in_dots" = "literals")


### IMPORT FUNCTION FILES
source("R/Functions/ImportAndClean_China.R")
source("R/Functions/ImportAndClean_Peru.R")
source("R/Functions/ImportAndClean_Svalbard.R")
source("R/Functions/ImportAndClean_Norway.R")
source("R/Functions/ImportAndClean_Colorado.R")
source("R/Functions/ImportAndClean_Database.R")

source("R/Functions/DataProcessing.R")
source("R/Functions/CalculateDiversityIndices.R")
source("R/Functions/CWTM_Bootstrapping.R")

source("R/Functions/MultivatiateAnalysis.R")

source("R/Functions/MakeMap.R")
source("R/Functions/MakePrettyFigures.R")



### IMPORT DRAKE PLANS
source("R/ImportPrettyDataPlan.R")
source("R/DataProcessingDrakePlan.R")
source("R/DataAnalyisDrakePlan.R")
source("R/MakePrettyFiguresPlan.R")





### COMBINING THE DRAKE PLANS 
MasterDrakePlan <-  bind_rows(ImportDrakePlan, 
                              CountryListAndTraitMeanDrakePlan,
                              DataProcessingDrakePlan,
                              DataAnalysisDrakePlan,
                              MakePrettyFiguresPlan) 

#DataAnalysisDrakePlan

#configure and make drake plan
config <- drake_config(MasterDrakePlan)

config