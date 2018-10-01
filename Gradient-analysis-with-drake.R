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
source("R/CleanData.R")
source("R/LoadingFiles.R")
source("R/sync_from_dropbox.R")

# Functions to download and load data from dropbox and google drive
drop_acc()



### NEEDS DOING!!!
# SV coords for site 1 from gradient
#load("data/traitsGradients_SV_2018", verbose = TRUE)
#metaSV <- traitsGradients_SV_2018 %>% filter(Project == "T") %>% distinct(Country, Gradient, Site, Elevation_m, Latitude_N, Longitude_E) %>% rename(Elevation = Elevation_m, Latitude = Latitude_N, Longitude = Longitude_E)

# Colorado: everything



#construct drake plan
analyses <- drake_plan(
  strings_in_dots = "literals",
  
  #### IMPORT & CLEAN DATA
  
  ## CHINA
  metaCH = get(load(file = file_in("data/metaCH.Rdata"))),
  metaCommunityCH = get(load(file = file_in("data/metaCommunity_CH_2012_2016.Rdata"))),
  traitCH_raw = target(
    drop_and_load(myfile = "transplant/USE THIS DATA/traits.Rdata",
                          localpath = "data/traits_2015_2016_China.Rdata"),
                   trigger = trigger(change = drop_get_metadata(path = "transplant/USE THIS DATA/traits.Rdata")$content_hash)
    ),
  communityCH_raw = target(
    drop_and_load(myfile = "transplant/USE THIS DATA/cover_thin_CH_2012_2016.Rdata",
                  localpath = "data/cover_thin_CH_2012_2016.Rdata"),
    trigger = trigger(change = drop_get_metadata(path = "transplant/USE THIS DATA/cover_thin_CH_2012_2016.Rdata")$content_hash)
  ),
  
  ## PERU
  metaPE = get(load(file = "data/metaPE.Rdata")),
  metaCommunityPE = get(load(file = "data/metaCommunity_PE_2018.Rdata")),
  traitPE = target(
    drop_and_load(myfile = "transplant/USE THIS DATA/PFTC3_Peru/traits_2018_Peru_cleaned.Rdata",
                  localpath = "data/traits_2018_Peru_cleaned.Rdata"),
    trigger = trigger(change = drop_get_metadata(path = "transplant/USE THIS DATA/PFTC3_Peru/traits_2018_Peru_cleaned.Rdata")$content_hash)
  ),
  communityPE_raw = target(
    drop_and_load(myfile = "transplant/USE THIS DATA/PFTC3_Peru/CommunityCover_2018_Peru.Rdata",
                  localpath = "data/CommunityCover_2018_Peru.Rdata"),
    trigger = trigger(change = drop_get_metadata(path = "transplant/USE THIS DATA/PFTC3_Peru/CommunityCover_2018_Peru.Rdata")$content_hash)
  ),
  
  ## SVALBARD
  metaCommunitySV = get(load(file = "data/metaCommunitySV_2018.Rdata")),
  traitSV = target(
    drop_and_load(myfile = "transplant/USE THIS DATA/PFTC4_Svalbard/traitsGradients_SV_2018.Rdata",
                  localpath = "data/traitsGradients_SV_2018.Rdata"),
    trigger = trigger(change = drop_get_metadata(path = "transplant/USE THIS DATA/PFTC4_Svalbard/traitsGradients_SV_2018.Rdata")$content_hash)
  ),
  communitySV_raw = target(
    drop_and_load(myfile = "transplant/USE THIS DATA/PFTC4_Svalbard/communitySV_2018.Rdata",
                  localpath = "data/communitySV_2018.Rdata"),
    trigger = trigger(change = drop_get_metadata(path = "transplant/USE THIS DATA/PFTC4_Svalbard/communitySV_2018.Rdata")$content_hash)
  ),
  
  ## NORWAY
  metaNO = get(load(file = "data/metaNO.Rdata")),
  metaCommunityNO.raw = get(load(file = "data/metaCommunityNO_2016.Rdata")),
  traitNO = target(
    drop_and_load.csv(myfile = "transplant/USE THIS DATA/Norway/traitdata_NO.csv",
                  localpath = "data/traitdata_NO.csv"),
    trigger = trigger(change = drop_get_metadata(path = "transplant/USE THIS DATA/Norway/traitdata_NO.csv")$content_hash)
  ),
  communityNO_raw = target(
    drop_and_load.xlsx(myfile = "transplant/USE THIS DATA/Norway/funcab_composition_2016.xlsx",
                      localpath = "data/funcab_composition_2016.xlsx"),
    trigger = trigger(change = drop_get_metadata(path = "transplant/USE THIS DATA/Norway/funcab_composition_2016.xlsx")$content_hash)
  ),
  
  ## COLORADO
  #metaCO = load(file = "data/metaNO.Rdata"),
  #traitCO = 
  #communityCO
  
  
  #### CLEAN DATA SETS
  traitCH = CleanChinaTrait(traitCH_raw),
  communityCH = CleanChinaCommunity(communityCH_raw),
  communityPE = CleanPeruCommunity(communityPE_raw),
  communitySV = CleanSvalbardCommunity(communitySV_raw),
  metaCommunityNO = CleanNorwayMetaCommunity(metaCommunityNO_raw),
  communityNO = CleanNorwayCommunity(communityNO_raw),
  
  # make a list with all data sets
  CountryList = MakeCountryList(metaCH, metaCommunityCH, communityCH, traitCH, 
                                metaPE, metaCommunityPE, communityPE, traitPE, 
                                metaCommunitySV, communitySV, traitSV, 
                                metaNO, metaCommunityNO, communityNO, traitNO)
  
  
  
  #### CALCULATIONS, ANALYSES, FIGURES
  #DivIndex <- CountryList %>% map(CalculateDiversityIndices())
  
)


#configure and make drake plan
config <- drake_config(analyses)
outdated(config)        # Which targets need to be (re)built?
make(analyses)          # Build the right things.

loadd(metaCH, metaCommunityCH, traitCH, communityCH, 
      metaPE, metaCommunityPE, traitPE, communityPE, 
      metaCommunitySV, traitSV, communitySV,
      metaNO, metaCommunityNO, traitNO, communityNO,
      CountryList)
#voew dependency graph
vis_drake_graph(config, targets_only = TRUE)
