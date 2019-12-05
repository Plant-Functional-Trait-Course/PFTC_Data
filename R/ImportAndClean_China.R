##################
  ### CHINA  ###
##################


#### CLEANING DATA ####

# Cleaning China meta community data
CleanChinaMetaCommunity <- function(metaCommunityCH_raw){
  metaCommunityCH <- metaCommunityCH_raw %>% 
    select(PlotID, Year, Moss, Lichen2, Litter, BareGround, Rock, Vascular, Bryophyte, Lichen, MedianHeight_cm, MedianMossHeight_cm) %>% 
    mutate(Country = "CH",
           Site = substr(PlotID, 1,1),
           Gradient = "1")
  return(metaCommunityCH)
}
#Moss and bryophyte columns are not the same.
#Lichen and Lichen2 columns are not the same.


# Cleaning China community data
CleanChinaCommunity <- function(communityCH_raw){
  communityCH <- communityCH_raw %>% 
    filter(TTtreat %in% c("control", "local")) %>% 
    filter(year == 2016) %>% 
    rename(Year = year, Site = originSiteID, BlockID = originBlockID, PlotID = turfID, Treatment = TTtreat, Taxon = speciesName, Cover = cover) %>% 
    mutate(Country = "CH",
           Gradient = as.character(1)) %>% 
    select(Country, Year, Site, Gradient, BlockID, PlotID, Taxon, Cover) %>% 
    mutate(Taxon = recode(Taxon, "Potentilla stenophylla var. emergens" = "Potentilla stenophylla")) %>% 
    filter(!is.na(Cover), !Cover == 0)
  
  return(communityCH)
}


# Clean China trait data
CleanChinaTrait <- function(traitCH_raw){
  traitCH <- traitCH_raw %>% 
    filter(Project %in% c("LOCAL", "0", "C")) %>% 
    mutate(Treatment = plyr::mapvalues(Project, c("C", "0", "LOCAL"), c("C", "O", "Gradient"))) %>% 
    mutate(Taxon = trimws(Taxon)) %>% 
    mutate(Year = year(Date),
           Country = "CH",
           Gradient = as.character(1),
           Project = "T") %>% 
    rename(BlockID = Location) %>%
    mutate(PlotID = paste(BlockID, Treatment, sep = "-"),
           ID = paste(Site, Treatment, Taxon, Individual_number, Leaf_number, sep = "_")) %>% 
    select(Country, Year, Site, Gradient, BlockID, PlotID, Taxon, Wet_Mass_g, Dry_Mass_g, Leaf_Thickness_Ave_mm, Leaf_Area_cm2, SLA_cm2_g, LDMC, C_percent, N_percent , CN_ratio, dN15_percent, dC13_percent, P_AVG, P_Std_Dev, P_Co_Var) %>% 
    gather(key = Trait, value = Value, -Country, -Year, -Site, -Gradient, -BlockID, -PlotID, -Taxon) %>% 
    # remove high N values
    filter(!c(Trait == "N_percent" & Value > 9)) %>% 
    filter(!is.na(Value))
  
  return(traitCH)
}



#### IMPORT, CLEAN AND MAKE LIST #### 
ImportClean_China <- function(){
  
  ### IMPORT DATA
  # meta data
  # metaCH = tibble(Country = "CH",
  #                         Gradient = "1",
  #                         Site = c("H", "A", "M", "L"),
  #                         Elevation = c(4100, 3850, 3500, 3000),
  #                         Latitude = c(29.90742, 29.88911, 29.86192, 29.84347),
  #                         Longitude = c(102.0118, 102.0173, 102.0360, 102.0343))
  metaCH = read_delim("data/metaCH.csv", delim = ";")
  # meta community data
  metaCommunityCH_raw = get(load(file = file_in("data/metaCommunity_CH_2012_2016.Rdata")))
  # community data
  communityCH_raw = get(load(file = file_in("data/cover_thin_CH_2012_2016.Rdata")))
  #communityCH_raw = target(drop_and_load(myfile = "transplant/USE THIS DATA/cover_thin_CH_2012_2016.Rdata", localpath = "data/cover_thin_CH_2012_2016.Rdata"), trigger = trigger(change = drop_get_metadata(path = "transplant/USE THIS DATA/cover_thin_CH_2012_2016.Rdata")$content_hash))
  # trait data
  traitCH_raw = get(load(file = file_in("data/traits_2015_2016_China.Rdata")))
  #traitCH_raw = target(drop_and_load(myfile = "transplant/USE THIS DATA/traits.Rdata", localpath = "data/traits_2015_2016_China.Rdata"), trigger = trigger(change = drop_get_metadata(path = "transplant/USE THIS DATA/traits.Rdata")$content_hash))
  # flux data
  fluxCH = get(load(file = file_in("data/standardControlFluxCH_2016.Rdata")))
  #fluxCH = target(drop_and_load(myfile = "transplant/USE THIS DATA/standardControlFluxCH_2016.Rdata", localpath = "data/standardControlFluxCH_2016.Rdata"), trigger = trigger(change = drop_get_metadata(path = "transplant/USE THIS DATA/standardControlFluxCH_2016.Rdata")$content_hash))

  
  ### CLEAN DATA SETS
  ## CN_Gongga
  metaCommunityCH = CleanChinaMetaCommunity(metaCommunityCH_raw)
  communityCH = CleanChinaCommunity(communityCH_raw)
  traitCH = CleanChinaTrait(traitCH_raw)
  
  # Make list
  Data_CH = list(meta = metaCH,
                 metaCommunity = metaCommunityCH,
                 community = communityCH,
                 trait = traitCH,
                 flux = fluxCH)
  
  return(Data_CH)
}
