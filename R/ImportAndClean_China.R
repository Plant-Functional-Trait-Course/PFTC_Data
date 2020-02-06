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
    filter(Treatment %in% c("LOCAL", "0", "C")) %>% 
    mutate(Treatment = plyr::mapvalues(Treatment, c("C", "0", "LOCAL"), c("C", "O", "Gradient"))) %>% 
    mutate(Taxon = trimws(Taxon)) %>% 
    mutate(Year = year(Date),
           Country = "CH",
           Gradient = as.character(1),
           Project = "T") %>% 
    mutate(PlotID = paste(BlockID, Treatment, sep = "-"),
           ID = paste(Site, Treatment, Taxon, Individual_number, Leaf_number, sep = "_")) %>% 
    select(Country, Year, Site, Gradient, BlockID, PlotID, Taxon, Wet_Mass_g, Dry_Mass_g, Leaf_Thickness_Ave_mm, Leaf_Area_cm2, SLA_cm2_g, LDMC, C_percent, N_percent , CN_ratio, dN15_percent, dC13_percent, P_percent, P_Std_Dev, P_Co_Var) %>% 
    gather(key = Trait, value = Value, -Country, -Year, -Site, -Gradient, -BlockID, -PlotID, -Taxon) %>% 
    # remove high N values
    filter(!c(Trait == "N_percent" & Value > 9)) %>% 
    filter(!is.na(Value))
  
  return(traitCH)
}


#### DOWNLOAD, IMPORT, CLEAN AND MAKE LIST #### 
ImportClean_China <- function(){
  
  ## DOWNLOAD DATA FROM OSF
  # meta data
  get_file(node = "f3knq",
           file = "metaCH.csv",
           path = "data_cleaned")
  # community
  get_file(node = "7mzjk",
           file = "community_2012_2016_China.csv",
           path = "data_cleaned",
           remote_path = "China")
  # meta community
  get_file(node = "4hjzu",
           file = "metaCommunity_CH_2012_2016.Rdata",
           path = "data_cleaned")
  # traits
  get_file(node = "emzgf",
           file = "PFTC1.2_China_2015_2016_Traits.csv",
           path = "data_cleaned")
  # flux
  get_file(node = "f3knq",
           file = "standardControlFluxCH.Rdata",
           path = "data_cleaned")
  
  ## IMPORT DATA
  # meta data
  metaCH = read_delim(file_in("data_cleaned/metaCH.csv"), delim = ";")
  # meta community data
  metaCommunityCH_raw = get(load(file = file_in("data_cleaned/metaCommunity_CH_2012_2016.Rdata")))
  # community data
  communityCH_raw = read_csv(file_in("data_cleaned/community_2012_2016_China.csv"))
  # trait data
  traitCH_raw = read_csv(file_in("data_cleaned/PFTC1.2_China_2015_2016_Traits.csv"), col_types = cols(Treatment = col_character()))
  # flux data
  fluxCH = get(load(file = file_in("data_cleaned/standardControlFluxCH_2016.Rdata")))
  hierarchyCH = c("Country", "Site", "BlockID", "PlotID")
  
  ## CLEAN DATA SETS
  metaCommunityCH = CleanChinaMetaCommunity(metaCommunityCH_raw)
  communityCH = CleanChinaCommunity(communityCH_raw)
  traitCH = CleanChinaTrait(traitCH_raw)
  
  # Make list
  Data_CH = list(meta = metaCH,
                 metaCommunity = metaCommunityCH,
                 community = communityCH,
                 trait = traitCH,
                 flux = fluxCH,
                 trait_hierarchy = hierarchyCH)
  
  return(Data_CH)
}
