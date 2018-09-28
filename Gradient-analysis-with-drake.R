# Load libraries
#library("devtools")
#install_github("ropensci/drake")
library("drake")
library("tidyverse")
library("readxl")
library("lubridate")
library("rdrop2")

pn <- . %>% print(n = Inf)

#import scripts
source("R/MergingCountriesToList.R")
source("R/CleanChinaData.R")

# Functions to download and load data from dropbox and google drive
drop_acc()

drop_and_load <- function(myfile, localpath){
  drop_download(path = myfile, local_path = localpath, overwrite = TRUE)
  dat <- load(file = localpath)
}

#construct drake plan
analyses <- drake_plan(
  strings_in_dots = "literals",
  #import & clean data
  
  # China
  traitCH = target(
    drop_and_load(myfile = "transplant/USE THIS DATA/traits.Rdata",
                          localpath = "data/traits_2015_2016_China.Rdata"),
                   trigger = trigger(change = drop_get_metadata(path = "transplant/USE THIS DATA/traits.Rdata")$content_hash)
    ),
  communityCH = target(
    drop_and_load(myfile = "transplant/USE THIS DATA/cover_thin_CH_2012_2016.Rdata",
                  localpath = "data/cover_thin_CH_2012_2016.Rdata"),
    trigger = trigger(change = drop_get_metadata(path = "transplant/USE THIS DATA/cover_thin_CH_2012_2016.Rdata")$content_hash)
  ),
  
  # Peru
  traitPE = target(
    drop_and_load(myfile = "transplant/USE THIS DATA/PFTC3_Peru/traits_2018_Peru_cleaned.Rdata",
                  localpath = "data/traits_2018_Peru_cleaned.Rdata"),
    trigger = trigger(change = drop_get_metadata(path = "transplant/USE THIS DATA/PFTC3_Peru/traits_2018_Peru_cleaned.Rdata")$content_hash)
  ),
  communityPE = target(
    drop_and_load(myfile = "transplant/USE THIS DATA/PFTC3_Peru/CommunityCover_2018_Peru.Rdata",
                  localpath = "data/CommunityCover_2018_Peru.Rdata"),
    trigger = trigger(change = drop_get_metadata(path = "transplant/USE THIS DATA/PFTC3_Peru/CommunityCover_2018_Peru.Rdata")$content_hash)
  ),
  
  # Svalbard
  traitSV = target(
    drop_and_load(myfile = "transplant/USE THIS DATA/PFTC4_Svalbard/traitsGradients_SV_2018.Rdata",
                  localpath = "data/traitsGradients_SV_2018.Rdata"),
    trigger = trigger(change = drop_get_metadata(path = "transplant/USE THIS DATA/PFTC4_Svalbaru/traitsGradients_SV_2018.Rdata")$content_hash)
  ),
  communitySV = target(
    drop_and_load(myfile = "transplant/USE THIS DATA/PFTC4_Svalbard/communitySV_2018.Rdata",
                  localpath = "data/communitySV_2018.Rdata"),
    trigger = trigger(change = drop_get_metadata(path = "transplant/USE THIS DATA/PFTC4_Svalbard/communitySV_2018.Rdata")$content_hash)
  )
  
  # Norway
  
  
  # Colorado
  
  
  
  # make a list with all data sets
  
  
  # make plots and analysis
  
)


  


  

#configure and make drake plan
config <- drake_config(analyses)
outdated(config)        # Which targets need to be (re)built?
make(analyses)          # Build the right things.

#voew dependency graph
vis_drake_graph(config, targets_only = TRUE)
