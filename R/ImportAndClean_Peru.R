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
  
  ## DOWNLOAD DATA FROM OSF
  # meta
  get_file(node = "7mzjk",
           file = "metaPE.csv",
           path = "data_cleaned",
           remote_path = "Peru")
  # community
  get_file(node = "7mzjk",
           file = "PFTC3.1_CommunityCover_2018_Peru.csv",
           path = "data_cleaned",
           remote_path = "Peru")
  # metaCommunity
  get_file(node = "7mzjk",
           file = "metaCommunity_PE_2018.Rdata",
           path = "data_cleaned",
           remote_path = "Peru")
  # traits
  get_file(node = "7mzjk",
           file = "PFTC3.7_Traits_2018_Peru_cleaned.csv",
           path = "data_cleaned",
           remote_path = "Peru")
  # flux
  get_file(node = "7mzjk",
           file = "standardControlFluxPE_2016.Rdata",
           path = "data_cleaned",
           remote_path = "Peru")
  
  ## IMPORT DATA
  # meta data
  metaPE = read_csv(file_in("data_cleaned/metaPE.csv"))
  # meta community
  metaCommunityPE_raw = get(load(file = file_in("data_cleaned/metaCommunity_PE_2018.Rdata")))
  # community
  communityPE_raw = read_csv(file_in("data_cleaned/PFTC3.1_CommunityCover_2018_Peru.csv"))
  # trait
  traitPE_raw = read_csv(file_in("data_cleaned/PFTC3.7_Traits_2018_Peru_cleaned.csv"))
  # flux
  fluxPE = get(load("data_cleaned/standardControlFluxPE_2016.Rdata"))
  hierarchyPE = c("Country", "Site", "BlockID", "PlotID")
  
  ## CLEAN DATA SETS
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
