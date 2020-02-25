##################
### COLORADO  ###
##################


#### CLEANING DATA ####
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
    filter(Site != "PBM") %>% 
    select(-"Common name/Morpho", -Total, -"Photo taken?", -"Specimen collected?") %>% 
    pivot_longer(cols = c(`Plot 1`:`Plot 5`), names_to = "PlotID", values_to = "Cover") %>% 
    rename("Taxon" = "Species") %>% 
    mutate(Date = dmy(Date),
           Country = "CO",
           Year = year(Date),
           Gradient = as.character(1)) %>% 
    mutate(BlockID = recode(PlotID, "Plot 1" = "1", "Plot 2" = "2", "Plot 3" = "3", "Plot 4" = "4", "Plot 5" = "5")) %>% 
    select(Country, Year, Site, Gradient, BlockID, Taxon, Cover) %>% 
    filter(!is.na(Cover), !Cover == 0) %>% 
    # add dummy variable
    mutate(PlotID = "1")

  return(communityCO)
}


#Cleaning Colorado Trait
CleanColoradoTrait <- function(species_dictionaryCO, traitCO_raw){
  Row5 <- tribble(
    ~Site, ~Species_from_abundance_data, ~Likely_same_species_from_trait_data,
    "Almont",   "Elymus elymoides", "Hordeum jubatum",
    "Almont",   "Elymus elymoides", "Hesperostipa comata"
  )
  
  species_dictionaryCO <- species_dictionaryCO %>% 
    slice(-5) %>% 
    bind_rows(Row5)
  
  traitCO <- traitCO_raw %>% 
    filter(site %in% c("Almont", "CBT", "Road", "Pfeiler", "Cinnamon")) %>% 
    select(year, site, block, taxon_std, leaf_area, wet_mass, dry_mass, SLA, height_flower, height_leaf, height, height_2, thickness, pc_C, pc_N, pc_P, d13C, d15N,  C_N,  N_C,  N_P) %>% 
    rename(Year = year, Site = site, BlockID = block, Taxon = taxon_std, Leaf_Area_cm2 = leaf_area, Wet_Mass_g = wet_mass, Dry_Mass_g = dry_mass, SLA_cm2_g = SLA, Plant_Height_cm = height_flower, Leaf_Thickness_Ave_mm = thickness, C_percent = pc_C, N_percent = pc_N, dC13_percent = d13C, dN15_percent = d15N, CN_ratio = C_N, NC_ratio = N_C, NP_ratio = N_P, P_percent = pc_P) %>%
    mutate(Country = "CO",
           LDMC = Dry_Mass_g/Wet_Mass_g) %>%
    mutate(Gradient = as.character(1),
           BlockID = gsub("Block|block", "", BlockID),
           # add dummy variable
           PlotID = "1") %>% 
    select(Country, Year, Site, BlockID, PlotID, Gradient, Taxon, Plant_Height_cm, Wet_Mass_g, Dry_Mass_g, Leaf_Thickness_Ave_mm, Leaf_Area_cm2, SLA_cm2_g, LDMC, P_percent, C_percent, N_percent, dC13_percent, dN15_percent, CN_ratio, NC_ratio, NP_ratio) %>% 
    mutate_at(., vars(P_percent, C_percent, N_percent, dC13_percent, dN15_percent, CN_ratio, NC_ratio, NP_ratio), funs(as.numeric(.))) %>% 
    pivot_longer(cols = c(Plant_Height_cm:NP_ratio), names_to = "Trait", values_to = "Value") %>% 
    filter(!is.na(Value)) %>% 
    filter(!is.na(Taxon)) %>% 
    # Replace species to match the community dataset
    left_join(species_dictionaryCO, by = c("Site", "Taxon" = "Likely_same_species_from_trait_data")) %>% 
    mutate(Taxon = if_else(!is.na(Species_from_abundance_data), Species_from_abundance_data, Taxon)) %>% 
    select(-Species_from_abundance_data, -Notes)
  
  return(traitCO)
}  


#### IMPORT, CLEAN AND MAKE LIST #### 
ImportClean_Colorado <- function(){
  
  ## DOWNLOAD FROM OSF
  #Download files from OSF
  get_file(node = "7mzjk",
           file = "metaCO.csv",
           path = "data_cleaned",
           remote_path = "Colorado")
  
  ## IMPORT DATA
  # meta data
  metaCO = read_csv(file_in("data_cleaned/metaCO.csv")) %>% filter(Site  != "PBM")
  # meta community
  metaCommunityCO_raw  = get(load(file = file_in("data/metaCommunityCO_2016.Rdata")))
  # community
  communityCO_raw <- read_csv(file = "data/Abundance_Data_2016_final.csv", col_names = TRUE)
  species_dictionaryCO <- read_csv(file = "data/Lorah_scrubbed_notes.csv", col_names = TRUE)
  #communityCO_raw = target(drop_and_load.csv(myfile = "transplant/USE THIS DATA/Colorado/CO_gradient_2016_Species_Cover.csv", localpath = "data/CO_gradient_2016_Species_Cover.csv"),trigger = trigger(change = drop_get_metadata(path = "transplant/USE THIS DATA/Colorado/CO_gradient_2016_Species_Cover.csv")$content_hash))
  # trait
  traitCO_raw <- read_csv(file = "data/rmbl_trait_data_master.csv", col_names = TRUE)
  #traitCO_raw = target(drop_and_load.csv(myfile = "transplant/USE THIS DATA/Colorado/rmbl_trait_data_master.csv", localpath = "data/rmbl_trait_data_master.csv"), trigger = trigger(change = drop_get_metadata(path = "transplant/USE THIS DATA/Colorado/rmbl_trait_data_master.csv")$content_hash))
  # flux
  fluxCO <- get(load("data/standardControlFluxCO_2016.Rdata"))
  #fluxCO = target(drop_and_load(myfile = "transplant/USE THIS DATA/Colorado/standardControlFluxCO_2016.Rdata", localpath = "data/standardControlFluxCO_2016.Rdata"), trigger = trigger(change = drop_get_metadata(path = "transplant/USE THIS DATA/Colorado/standardControlFluxCO_2016.Rdata")$content_hash))
  hierarchyCO = c("Country", "Site", "BlockID", "PlotID")
  
  ### CLEAN DATA SETS
  ## CN_Gongga
  metaCommunityCO = CleanColoradoMetaCommunity(metaCommunityCO_raw)
  communityCO = CleanColoradoCommunity(communityCO_raw)
  traitCO = CleanColoradoTrait(species_dictionaryCO, traitCO_raw)
  
  # Make list
  Data_CO = list(meta = metaCO,
                 metaCommunity = metaCommunityCO,
                 community = communityCO,
                 trait = traitCO,
                 flux = fluxCO,
                 trait_hierarchy = hierarchyCO)
  
  return(Data_CO)
}
