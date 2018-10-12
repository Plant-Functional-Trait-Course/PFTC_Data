#### Construct data import drake plan ####

#import scripts
source("R/MergingCountriesToList.R")
source("R/CleanData.R")
source("R/LoadingFiles.R")
source("R/sync_from_dropbox.R")

# make an import drake plan
dataImport_plan = drake_plan(
  strings_in_dots = "literals",
  
  #### IMPORT & CLEAN DATA
  
  ## CHINA
  metaCH_raw = get(load(file = file_in("data/metaCH.Rdata"))),
  metaCommunityCH_raw = get(load(file = file_in("data/metaCommunity_CH_2012_2016.Rdata"))),
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
  metaPE_raw = get(load(file = file_in("data/metaPE.Rdata"))),
  metaCommunityPE_raw = get(load(file = file_in("data/metaCommunity_PE_2018.Rdata"))),
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
  metaSV_raw = get(load(file = file_in("data/metaSV.Rdata"))),
  metaCommunitySV_raw = get(load(file = file_in("data/metaCommunitySV_2018.Rdata"))),
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
  metaNO_raw = get(load(file = file_in("data/metaNO.Rdata"))),
  metaCommunityNO_raw = get(load(file = file_in("data/metaCommunityNO_2016.Rdata"))),
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
  metaCO_raw = get(load(file = file_in("data/metaCO.Rdata"))),
  metaCommunityCO_raw = get(load(file = file_in("data/metaCommunityCO_2016.Rdata"))),
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
  #metaAll = get(load(file = file_in("data/metaAllC.Rdata"))),
  metaBioclim_raw = target(
    drop_and_load(myfile = "transplant/USE THIS DATA/PFTC/MetaAllCountriesVPD_PET.RData",
                  localpath = "data/MetaAllCountriesVPD_PET.RData"),
    trigger = trigger(change = drop_get_metadata(path = "transplant/USE THIS DATA/PFTC/MetaAllCountriesVPD_PET.RData")$content_hash)
  ),
  
  
  ### CW-TRAIT MEANS BOOTSTRAPPING ###
  #CW_Means_Bootstrapped_raw = target(
    #drop_and_load.rds(myfile = "transplant/USE THIS DATA/PFTC/trait_distribution_output/pftc_bootstrapped_moments.RDS",
    #                  localpath = "data/pftc_bootstrapped_moments.RDS"),
    #trigger = trigger(change = drop_get_metadata(path = "transplant/USE THIS DATA/PFTC/trait_distribution_output/pftc_bootstrapped_moments.RDS")$content_hash)
 # ),
  
  #### CLEAN DATA SETS
  metaCH = CleanChinaMeta(metaCH_raw),
  traitCH = CleanChinaTrait(traitCH_raw),
  communityCH = CleanChinaCommunity(communityCH_raw),
  metaCommunityCH = CleanChinaMetaCommunity(metaCommunityCH_raw),
  
  metaPE = CleanSvalbardMeta(metaPE_raw),
  metaCommunityPE = CleanPeruMetaCommunity(metaCommunityPE_raw),
  traitPE = CleanPeruTrait(traitPE_raw),
  communityPE = CleanPeruCommunity(communityPE_raw),
  
  metaSV = CleanSvalbardMeta(metaSV_raw),
  metaCommunitySV = CleanSvalbardMetaCommunity(metaCommunitySV_raw),
  traitSV = CleanSvalbardTrait(traitSV_raw),
  communitySV = CleanSvalbardCommunity(communitySV_raw),
  
  metaNO = CleanSvalbardMeta(metaNO_raw), 
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
  
  #CW_Means_Bootstrapped = CleanCWMeansBoot(CW_Means_Bootstrapped_raw),
  
  
  # make a list with all data sets
  CountryList = MakeCountryList(metaCH, metaCommunityCH, communityCH, traitCH, fluxCH,
                                metaPE, metaCommunityPE, communityPE, traitPE, fluxPE,
                                metaSV, metaCommunitySV, communitySV, traitSV, fluxSV,
                                metaNO, metaCommunityNO, communityNO, traitNO, fluxNO,
                                metaCO, metaCommunityCO, communityCO, traitCO, fluxCO),

  
 ## Combine meta data
 metaAll = metaCH %>% 
   bind_rows(metaPE, metaSV, metaNO, metaCO),
 
 metaCommunityAll = metaCommunityCH %>% 
   bind_rows(metaCommunityPE, metaCommunitySV, metaCommunityNO, metaCommunityCO)
  
)
