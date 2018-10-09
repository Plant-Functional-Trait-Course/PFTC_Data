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
source("R/CommunityWeightedTraitMeans.R")

# Functions to download and load data from dropbox and google drive
drop_acc()



### NEEDS DOING!!!
# Colorado: trait and height
#DryMass_g Svalbard!!!
#CHina gradient comm
# PE site and plot
# CO site_plot (only a number)


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
  fluxCH = target(
    drop_and_load(myfile = "transplant/USE THIS DATA/standardControlFluxCH_2016.Rdata",
                  localpath = "data/standardControlFluxCH_2016.Rdata"),
    trigger = trigger(change = drop_get_metadata(path = "transplant/USE THIS DATA/standardControlFluxCH_2016.Rdata")$content_hash)
  ),
  
  ## PERU
  metaPE = get(load(file = "data/metaPE.Rdata")),
  metaCommunityPE_raw = get(load(file = "data/metaCommunity_PE_2018.Rdata")),
  traitPE_raw = target(
    drop_and_load(myfile = "transplant/USE THIS DATA/PFTC3_Peru/traits_2018_Peru_cleaned.Rdata",
                  localpath = "data/traits_2018_Peru_cleaned.Rdata"),
    trigger = trigger(change = drop_get_metadata(path = "transplant/USE THIS DATA/PFTC3_Peru/traits_2018_Peru_cleaned.Rdata")$content_hash)
  ),
  communityPE_raw = target(
    drop_and_load(myfile = "transplant/USE THIS DATA/PFTC3_Peru/CommunityCover_2018_Peru.Rdata",
                  localpath = "data/CommunityCover_2018_Peru.Rdata"),
    trigger = trigger(change = drop_get_metadata(path = "transplant/USE THIS DATA/PFTC3_Peru/CommunityCover_2018_Peru.Rdata")$content_hash)
  ),
  fluxPE = target(
    drop_and_load(myfile = "transplant/USE THIS DATA/PFTC3_Peru/standardControlFluxPE_2016.Rdata",
                  localpath = "data/standardControlFluxPE_2016.Rdata"),
    trigger = trigger(change = drop_get_metadata(path = "transplant/USE THIS DATA/PFTC3_Peru/standardControlFluxPE_2016.Rdata")$content_hash)
  ),
  
  
  ## SVALBARD
  metaSV_raw = get(load(file = "data/metaSV.Rdata")),
  metaCommunitySV = get(load(file = "data/metaCommunitySV_2018.Rdata")),
  traitSV_raw = target(
    drop_and_load(myfile = "transplant/USE THIS DATA/PFTC4_Svalbard/traitsGradients_SV_2018.Rdata",
                  localpath = "data/traitsGradients_SV_2018.Rdata"),
    trigger = trigger(change = drop_get_metadata(path = "transplant/USE THIS DATA/PFTC4_Svalbard/traitsGradients_SV_2018.Rdata")$content_hash)
  ),
  communitySV_raw = target(
    drop_and_load(myfile = "transplant/USE THIS DATA/PFTC4_Svalbard/communitySV_2018.Rdata",
                  localpath = "data/communitySV_2018.Rdata"),
    trigger = trigger(change = drop_get_metadata(path = "transplant/USE THIS DATA/PFTC4_Svalbard/communitySV_2018.Rdata")$content_hash)
  ),
  fluxSV = target(
    drop_and_load(myfile = "transplant/USE THIS DATA/PFTC4_Svalbard/standardControlFluxSV_2016.Rdata",
                  localpath = "data/standardControlFluxSV_2016.Rdata"),
    trigger = trigger(change = drop_get_metadata(path = "transplant/USE THIS DATA/PFTC4_Svalbard/standardControlFluxSV_2016.Rdata")$content_hash)
  ),
  
  
  ## NORWAY
  metaNO = get(load(file = "data/metaNO.Rdata")),
  metaCommunityNO_raw = get(load(file = "data/metaCommunityNO_2016.Rdata")),
  traitNO_raw = target(
    drop_and_load.csv(myfile = "transplant/USE THIS DATA/Norway/traitdata_NO.csv",
                  localpath = "data/traitdata_NO.csv"),
    trigger = trigger(change = drop_get_metadata(path = "transplant/USE THIS DATA/Norway/traitdata_NO.csv")$content_hash)
  ),
  communityNO_raw = target(
    drop_and_load.xlsx(myfile = "transplant/USE THIS DATA/Norway/funcab_composition_2016.xlsx",
                      localpath = "data/funcab_composition_2016.xlsx"),
    trigger = trigger(change = drop_get_metadata(path = "transplant/USE THIS DATA/Norway/funcab_composition_2016.xlsx")$content_hash)
  ),
  spNO = target(
    drop_and_load.xlsx(myfile = "transplant/USE THIS DATA/Norway/systematics_species.xlsx",
                       localpath = "data/fsystematics_species.xlsx"),
    trigger = trigger(change = drop_get_metadata(path = "transplant/USE THIS DATA/Norway/systematics_species.xlsx")$content_hash)
  ),
  fluxNO_raw = target(
    drop_and_load(myfile = "transplant/USE THIS DATA/Norway/standardControlFluxNO_2016.Rdata",
                  localpath = "data/standardControlFluxNO_2016.Rdata"),
    trigger = trigger(change = drop_get_metadata(path = "transplant/USE THIS DATA/Norway/standardControlFluxNO_2016.Rdata")$content_hash)
  ),
  
  
  ## COLORADO
  metaCO_raw = get(load(file = "data/metaCO.Rdata")),
  metaCommunityCO_raw = get(load(file = "data/metaCommunityCO_2016.Rdata")),
  communityCO_raw = target(
    drop_and_load.csv(myfile = "transplant/USE THIS DATA/Colorado/CO_gradient_2016_Species_Cover.csv",
                      localpath = "data/CO_gradient_2016_Species_Cover.csv"),
    trigger = trigger(change = drop_get_metadata(path = "transplant/USE THIS DATA/Colorado/CO_gradient_2016_Species_Cover.csv")$content_hash)
),
  traitCO_raw = target(
    drop_and_load.csv(myfile = "transplant/USE THIS DATA/Colorado/rmbl_trait_data_master.csv",
                      localpath = "data/rmbl_trait_data_master.csv"),
    trigger = trigger(change = drop_get_metadata(path = "transplant/USE THIS DATA/Colorado/rmbl_trait_data_master.csv")$content_hash)
),
  fluxCO_raw = target(
    drop_and_load(myfile = "transplant/USE THIS DATA/Colorado/standardControlFluxCO_2016.Rdata",
                  localpath = "data/standardControlFluxCO_2016.Rdata"),
    trigger = trigger(change = drop_get_metadata(path = "transplant/USE THIS DATA/Colorado/standardControlFluxCO_2016.Rdata")$content_hash)
),

  ### META BIOCLIM ###
  metaBioclim_raw = target(
  drop_and_load(myfile = "transplant/USE THIS DATA/PFTC/MetaBioclimAllCountries.Rdata",
                localpath = "data/MetaBioclimAllCountries.Rdata"),
  trigger = trigger(change = drop_get_metadata(path = "transplant/USE THIS DATA/PFTC/MetaBioclimAllCountries.Rdata")$content_hash)
),


  
  #### CLEAN DATA SETS
  traitCH = CleanChinaTrait(traitCH_raw),
  communityCH = CleanChinaCommunity(communityCH_raw),
  
  metaCommunityPE = CleanPeruMetaCommunity(metaCommunityPE_raw),
  traitPE = CleanPeruTrait(traitPE_raw),
  communityPE = CleanPeruCommunity(communityPE_raw),

  metaSV = CleanSvalbardMeta(metaSV_raw),
  traitSV = CleanSvalbardTrait(traitSV_raw),
  communitySV = CleanSvalbardCommunity(communitySV_raw),

  metaCommunityNO = CleanNorwayMetaCommunity(metaCommunityNO_raw),
  communityNO = CleanNorwayCommunity(communityNO_raw, spNO),
  traitNO = CleanNorwayTrait(traitNO_raw),
  fluxNO = CleanNorwayFlux(fluxNO_raw),

  metaCO  = CleanColoradoMeta(metaCO_raw),  
  communityCO = CleanColoradoCommunity(communityCO_raw),
  traitCO = CleanColoradoTrait(traitCO_raw),
  metaCommunityCO = CleanColoradoMetaCommunity(metaCommunityCO_raw),
  fluxCO = CleanColoradoFlux(fluxCO_raw),

  metaBioclim = CleanMetaBioclim(metaBioclim_raw),
  
  # make a list with all data sets
  CountryList = MakeCountryList(metaCH, metaCommunityCH, communityCH, traitCH, fluxCH,
                                metaPE, metaCommunityPE, communityPE, traitPE, fluxPE,
                                metaSV, metaCommunitySV, communitySV, traitSV, fluxSV,
                                metaNO, metaCommunityNO, communityNO, traitNO, fluxNO,
                                metaCO, metaCommunityCO, communityCO, traitCO, fluxCO),
  
  
  
  #### CALCULATIONS, ANALYSES, FIGURES
  TraitMeans = CountryList %>% 
    map_df(CommunityW_GlobalAndLocalMeans) %>% 
    left_join(metaBioclim),
    
  GradientPlot = MakeFigure(TraitMeans)

)


#configure and make drake plan
config <- drake_config(analyses)
# outdated(config)        # Which targets need to be (re)built?
make(analyses)          # Build the right things.
loadd()
readd(GradientPlot)

TraitMeans %>% filter(!is.na(Trait)) %>% count(Country)
#voew dependency graph
vis_drake_graph(config, targets_only = TRUE)
