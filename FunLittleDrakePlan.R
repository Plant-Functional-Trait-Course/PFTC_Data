# Load libraries
#library("devtools")
#install_github("ropensci/drake")
library("drake")
library("tidyverse")
library("readxl")
library("lubridate")
library("rdrop2")

#import scripts
source("R/CommunityWeightedTraitMeans.R")

analyses2 <- drake_plan(
  strings_in_dots = "literals",
  
  #### IMPORT & CLEAN DATA
  TraitMeans = get(load(file = "CWTraitMeans_Gradient.Rdata")),
  
  #### CLEAN DATA SETS
  
  #### CALCULATIONS, ANALYSES, FIGURES
  GradientPlot = MakeFigure(TraitMeans)
  
)


#configure and make drake plan
config <- drake_config(analyses2)
outdated(config)        # Which targets need to be (re)built?
make(analyses2)          # Build the right things.
loadd()
readd(GradientPlot)

#voew dependency graph
vis_drake_graph(config, targets_only = TRUE)
