##################
### COLORADO  ###
##################


#### CLEANING DATA ####
# Cleaning Colorado meta
CleanColoradoMeta <- function(metaCO_raw){
  metaCO <- metaCO_raw %>% 
    mutate(Country = "CO",
           Gradient = "1",
           Site = as.character(Site))
  
  return(metaCO)
}


#Cleaning Colorado Meta Community
CleanColoradoMetaCommunity <- function(metaCommunityCO_raw){
  metaCommunityCO <- metaCommunityCO_raw %>%
    filter(!is.na(Site)) %>% 
    mutate(PlotID = recode(PlotID, "plot_3_pct" = "plot3_pct"),
           Gradient = as.character(1)) %>% 
    mutate(PlotID = recode(PlotID, "plot1_pct" = "1", "plot2_pct" = "2", "plot3_pct" = "3", "plot4_pct" = "4", "plot5_pct" = "5"),
           PlotID = paste(Site, PlotID, sep = "_")) %>% 
    spread(key = Group, value = Cover) %>%
    select(-Date.y, -MeanHeight_cm.y) %>% 
    rename(Date = Date.x, MeanHeight_cm = MeanHeight_cm.x, Graminoid = 'Total Graminoid', Herb = 'Total Herb', Shrub = 'Total Shrub', Bare_soil_litter_dead = 'Bare (Bare soil + Litter + Dead)', BareSoil = 'Bare soil')
  
  return(metaCommunityCO)
}


#Cleaning Colorado Community
CleanColoradoCommunity <- function(communityCO_raw){
  communityCO <- communityCO_raw %>%
    filter(!is.na(site)) %>% 
    filter(!species_or_ground_cover %in% c("Bare (Bare soil + Litter + Dead)", "Rock", "Total Graminoid", "Total Herb", "Total Shrub", "Bare soil", "Litter", "Dead")) %>% 
    select(-X16, -X17, -X18, -X19, -X20, -X21, -X22, -X23, -X24, -X25, -X26, -plot1_count, -plot2_count, -plot3_count, -plot4_count, -plot5_count, -total_site_percent) %>% 
    tidyr::gather(key = PlotID, value = Cover, -site, -date_yyyymmdd, -species_or_ground_cover, -growth_habit) %>% 
    mutate(Country = "CO",
           Year = year(date_yyyymmdd)) %>% 
    rename(Site = site, Taxon = species_or_ground_cover, functionalGroup = growth_habit) %>%
    mutate(PlotID = recode(PlotID, "plot_3_pct" = "plot3_pct"),
           Gradient = as.character(1),
           BlockID = as.character(1)) %>% 
    mutate(PlotID = recode(PlotID, "plot1_pct" = "1", "plot2_pct" = "2", "plot3_pct" = "3", "plot4_pct" = "4", "plot5_pct" = "5"),
           PlotID = paste(Site, PlotID, sep = "_")) %>% 
    select(Country, Year, Site, Gradient, BlockID, PlotID, Taxon, Cover) %>% 
    filter(!is.na(Cover), !Cover == 0)
  return(communityCO)
}


#Cleaning Colorado Trait
CleanColoradoTrait <- function(traitCO_raw){
  traitCO <- traitCO_raw %>% 
    filter(site %in% c("Almont", "CBT", "Road", "Pfeiler", "PBM", "Cinnamon")) %>% 
    select(year, site, block, taxon_std, leaf_area, wet_mass, dry_mass, SLA, height_flower, height_leaf, height, height_2, thickness, pc_C, pc_N, pc_P, d13C, d15N,  C_N,  N_C,  N_P) %>% 
    rename(Year = year, Site = site, PlotID = block, Taxon = taxon_std, Leaf_Area_cm2 = leaf_area, Wet_Mass_g = wet_mass, Dry_Mass_g = dry_mass, SLA_cm2_g = SLA, Plant_Height_cm = height_flower, Leaf_Thickness_Ave_mm = thickness, C_percent = pc_C, N_percent = pc_N, dC13_percent = d13C, dN15_percent = d15N, CN_ratio = C_N, NC_ratio = N_C, NP_ratio = N_P, P_AVG = pc_P) %>%
    mutate(Country = "CO",
           LDMC = Dry_Mass_g/Wet_Mass_g) %>%
    mutate(Gradient = as.character(1),
           BlockID = as.character(1),
           PlotID = paste(Site, gsub("Block", "", PlotID), sep = "_")) %>% 
    select(Country, Year, Site, Gradient, BlockID, PlotID, Taxon, Plant_Height_cm, Wet_Mass_g, Dry_Mass_g, Leaf_Thickness_Ave_mm, Leaf_Area_cm2, SLA_cm2_g, LDMC, C_percent, N_percent, dC13_percent, dN15_percent, CN_ratio, NC_ratio, NP_ratio, P_AVG) %>% 
    mutate_at(., vars(C_percent, N_percent, dC13_percent, dN15_percent, CN_ratio, NC_ratio, NP_ratio, P_AVG), funs(as.numeric(.))) %>% 
    tidyr::gather(key = Trait, value = Value, -Country, -Year, -Site, -BlockID, -PlotID, -Gradient, -Taxon) %>% 
    filter(!is.na(Value))
  
  return(traitCO)
}  


#### IMPORT, CLEAN AND MAKE LIST #### 
ImportClean_Colorado <- function(){
  
  ### IMPORT DATA
  # meta data
  metaCO_raw = get(load(file = file_in("data/metaCO.Rdata")))
  # meta community
  metaCommunityCO_raw  = get(load(file = file_in("data/metaCommunityCO_2016.Rdata")))
  # community
  communityCO_raw <- read_csv(file = "data/CO_gradient_2016_Species_Cover.csv", col_names = TRUE)
  #communityCO_raw = target(drop_and_load.csv(myfile = "transplant/USE THIS DATA/Colorado/CO_gradient_2016_Species_Cover.csv", localpath = "data/CO_gradient_2016_Species_Cover.csv"),trigger = trigger(change = drop_get_metadata(path = "transplant/USE THIS DATA/Colorado/CO_gradient_2016_Species_Cover.csv")$content_hash))
  # trait
  traitCO_raw <- read_csv(file = "data/rmbl_trait_data_master.csv", col_names = TRUE)
  #traitCO_raw = target(drop_and_load.csv(myfile = "transplant/USE THIS DATA/Colorado/rmbl_trait_data_master.csv", localpath = "data/rmbl_trait_data_master.csv"), trigger = trigger(change = drop_get_metadata(path = "transplant/USE THIS DATA/Colorado/rmbl_trait_data_master.csv")$content_hash))
  # flux
  fluxCO <- read_csv(file = "data/standardControlFluxCO_2016.Rdata", col_names = TRUE)
  #fluxCO = target(drop_and_load(myfile = "transplant/USE THIS DATA/Colorado/standardControlFluxCO_2016.Rdata", localpath = "data/standardControlFluxCO_2016.Rdata"), trigger = trigger(change = drop_get_metadata(path = "transplant/USE THIS DATA/Colorado/standardControlFluxCO_2016.Rdata")$content_hash))
  
  ### CLEAN DATA SETS
  ## CN_Gongga
  metaCO = CleanColoradoMeta(metaCO_raw)
  metaCommunityCO = CleanColoradoMetaCommunity(metaCommunityCO_raw)
  communityCO = CleanColoradoCommunity(communityCO_raw)
  traitCO = CleanColoradoTrait(traitCO_raw)
  
  # Make list
  Data_CO = list(meta = metaCO,
                 metaCommunity = metaCommunityCO,
                 community = communityCO,
                 trait = traitCO,
                 flux = fluxCO)
  
  return(Data_CO)
}
