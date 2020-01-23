##################
  ### PERU  ###
##################


#### CLEANING DATA ####

# Cleaning Peru metaCommunity
CleanPeruMetaCommunity <- function(metaCommunityPE_raw){
  metaCommunityPE <- metaCommunityPE_raw %>% 
    mutate(Treatment = recode(Treatment, "burned" = "B", "control" = "C", "double_burned" = "BB"),
           PlotID = paste(Site, PlotID, Treatment, sep="_"),
           Gradient = as.character(1),
           Vascular =  rowSums(select(., Forbs, Graminoids, Shrub, Ferns), na.rm = TRUE),
           Treatment = as.factor(Treatment),
           cover.shrub.layer = as.numeric(cover.shrub.layer),
           cover.field.layer = as.numeric (cover.field.layer),
           Country = "PE") %>% 
    rename(Forb = Forbs, Graminoid = Graminoids, Fern = Ferns) %>% 
    select(Site, Year, PlotID, Treatment, Forb, Graminoid, Shrub, Fern, BareGround, Rock, Litter, MedianHeight_cm, Vascular, Gradient, Country)
  return(metaCommunityPE)
}


# Cleaning Peru community

CleanPeruCommunity <- function(communityPE_raw){
  communityPE <- communityPE_raw %>% 
    mutate(Country = "PE", 
           BlockID = as.character(1),
           PlotID = paste(Site, PlotID, Treatment, sep="_"),
           Gradient = as.character(1)) %>% 
    select(Country, Year, Site, Gradient, BlockID, PlotID, Taxon, Cover) %>% 
    filter(!is.na(Cover))
  return(communityPE)
}

#Cleaning Peru Trait

CleanPeruTrait <- function(traitPE_raw){
  traitPE <- traitPE_raw %>%
    mutate(BlockID = as.character(1),
           PlotID = paste(Site, PlotID, Treatment, sep="_"),
           Gradient = as.character(1)) %>%
    select(Country, Year, Site, Gradient, BlockID, PlotID, Taxon, Plant_Height_cm, Wet_Mass_g, Dry_Mass_g, Leaf_Thickness_Ave_mm, Leaf_Area_cm2, SLA_cm2_g, LDMC) %>% 
    gather(key = Trait, value = Value, -Country, -Year, -Site, -BlockID, -PlotID, -Gradient, -Taxon) %>% 
    mutate(Taxon = recode(Taxon, "Agrostis 2" = "Agrostis sp2")) %>% 
    filter(!is.na(Value))
  
  return(traitPE)
  
}  


#### IMPORT, CLEAN AND MAKE LIST #### 
ImportClean_Peru <- function(){
  
  #### DOWNLOAD DATA FROM OSF

  
  ### IMPORT DATA
  # meta data
  #Download files from OSF
  get_file(node = "7mzjk",
           file = "metaPE.csv",
           path = "data_cleaned")
  
  # meta community
  metaCommunityPE_raw = get(load(file = file_in("data/metaCommunity_PE_2018.Rdata")))
  # community
  communityPE_raw = get(load(file = file_in("data/CommunityCover_2018_Peru.Rdata")))
  #communityPE_raw = target(drop_and_load(myfile = "transplant/USE THIS DATA/PFTC3_Peru/CommunityCover_2018_Peru.Rdata", localpath = "data/CommunityCover_2018_Peru.Rdata"), trigger = trigger(change = drop_get_metadata(path = "transplant/USE THIS DATA/PFTC3_Peru/CommunityCover_2018_Peru.Rdata")$content_hash))
  # trait
  traitPE_raw = get(load(file = file_in("data/traits_2018_Peru_cleaned.Rdata")))
  #traitPE_raw = target(drop_and_load(myfile = "transplant/USE THIS DATA/PFTC3_Peru/traits_2018_Peru_cleaned.Rdata", localpath = "data/traits_2018_Peru_cleaned.Rdata"), trigger = trigger(change = drop_get_metadata(path = "transplant/USE THIS DATA/PFTC3_Peru/traits_2018_Peru_cleaned.Rdata")$content_hash))
  # flux
  fluxPE = load("data/standardControlFluxPE_2016.Rdata")
  #fluxPE = target(drop_and_load(myfile = "transplant/USE THIS DATA/PFTC3_Peru/standardControlFluxPE_2016.Rdata", localpath = "data/standardControlFluxPE_2016.Rdata"), trigger = trigger(change = drop_get_metadata(path = "transplant/USE THIS DATA/PFTC3_Peru/standardControlFluxPE_2016.Rdata")$content_hash))
  hierarchyPE = c("Country", "Site", "BlockID", "PlotID")
  
  
  ### CLEAN DATA SETS
  metaCommunityPE = CleanPeruMetaCommunity(metaCommunityPE_raw)
  communityPE = CleanPeruCommunity(communityPE_raw)
  traitPE = CleanPeruTrait(traitPE_raw)
  
  # Make list
  Data_PE = list(meta = metaPE,
                 metaCommunity = metaCommunityPE,
                 community = communityPE,
                 trait = traitPE,
                 flux = fluxPE,
                 trait_hierarchy = hierarchyPE)
  
  return(Data_PE)
}
