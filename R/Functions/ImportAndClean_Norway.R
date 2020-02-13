##################
 ### NORWAY  ###
##################


#### CLEANING DATA ####
#Cleaning Norway Meta Community 
CleanNorwayMetaCommunity <- function(metaCommunityNO_raw){
  metaCommunityNO <- metaCommunityNO_raw %>%
    rename(Site = SiteID) %>% 
    mutate(Gradient = case_when(Site %in% c("Fau", "Alr", "Ulv") ~ as.character(1),
                                Site %in% c("Vik", "Hog", "Lav") ~ as.character(2),
                                Site %in% c("Arh", "Ram", "Gud") ~ as.character(3),
                                Site %in% c("Ovs", "Ves", "Skj") ~ as.character(4))) %>% 
    mutate(BlockID = as.numeric(BlockID),
           Treatment = as.factor(Treatment),
           Rock = as.numeric(Rock),
           Bryophyte = rowSums(select(., Liver, Bryophytes), na.rm = TRUE)) %>% 
    filter(!is.na(MedianHeight_cm)) %>%
    select(Site, BlockID, Treatment, PlotID, Year, Lichen, Litter, BareGround, Rock, Graminoid, Forb, Bryophyte, MedianHeight_cm, MedianMossHeight_cm, Vascular, Gradient, Country)
  return(metaCommunityNO)
}  


#Cleaning Norway Community
#Dictionary to make names in the community data speak with the names in the trait data
Dictionary_communityNO <- read.table(header = TRUE, stringsAsFactors = FALSE, text = 
                                       "old new
                                     Nar_stri Nar_str
                                     Tarax Tar_sp
                                     Euph_sp Eup_sp
                                     Phle_alp Phl_alp
                                     Rhin_min Rhi_min
                                     Rum_ac-la Rum_acl
                                     Trien_eur Tri_eur
                                     Rub_idae Rub_ida
                                     Saus_alp Sau_alp
                                     Ave__pub Ave_pub
                                     Car_atra Car_atr
                                     Hypo_rad Hyp_rad
                                     Bart_alp Bar_alp
                                     Car_pulic Car_pul
                                     Carex_sp Car_sp
                                     Hier_sp Hie_sp
                                     Salix_sp Sal_sp
                                     Emp_her Emp_nig
                                     Emp Emp_nig
                                     Hie_vulg Hie_vul
                                     Vio_can Vio_riv")

CleanNorwayCommunity <- function(communityNO_raw, spNO){
  spNO <- spNO %>% 
    mutate(Species = gsub(" ", "_", Species))
  
  communityNO <- communityNO_raw %>% 
    filter(Measure == "Cover") %>%
    select(-Treatment, -'Nid herb', 'Nid gram', -'Nid rosett', -'Nid seedling', -liver, -lichen, -litter, -soil, -rock, -'#Seedlings', -TotalGraminoids, -totalForbs, -totalBryophytes...285, -vegetationHeight, -mossHeight, -comment, -'ver seedl', -canum, -totalVascular, -totalBryophytes...292, -acro, -pleuro, -totalLichen) %>% 
    gather(key = Taxon, value = Cover, -Site, -Block, -turfID, -subPlot, -year, -date, -Measure, -recorder) %>% 
    filter(!is.na(Cover)) %>% 
    mutate(Taxon = gsub(" ", "_", Taxon))%>%
    mutate(Taxon = plyr::mapvalues(Taxon, from = Dictionary_communityNO$old, to = Dictionary_communityNO$new, warn_missing = FALSE)) %>%
    left_join(spNO, by = c("Taxon" = "Species")) %>% 
    select(-Genus, -Family, -Order, -Taxon) %>% 
    rename(Taxon = Full_name) %>% 
    mutate(Site = substr(Site, 1, 3)) %>% 
    mutate(Country = "NO") %>% 
    mutate(Gradient = case_when(Site %in% c("Fau", "Alr", "Ulv") ~ as.character(1),
                                Site %in% c("Vik", "Hog", "Lav") ~ as.character(2),
                                Site %in% c("Arh", "Ram", "Gud") ~ as.character(3),
                                Site %in% c("Ovs", "Ves", "Skj") ~ as.character(4)
    )) %>% 
    rename(Year = year, BlockID = Block, PlotID = turfID) %>% 
    mutate(Cover = as.numeric(Cover),
           BlockID = as.character(BlockID)) %>% 
    select(Country, Year, Site, Gradient, BlockID, PlotID, Taxon, Cover) %>% 
    # Remove duplicate values
    group_by(Country, Year, Site, Gradient, BlockID, PlotID, Taxon, Cover) %>% 
    filter(n() == 1) %>% 
    # Remove duplicate sp in turf Skj3XC
    group_by(Country, Year, Site, Gradient, BlockID, PlotID, Taxon) %>% 
    filter(n() == 1) %>% 
    # remove NA in Taxon
    filter(!is.na(Taxon))
  
  return(communityNO)
}


#Cleaning Norway Trait
CleanNorwayTrait <- function(traitNO_raw){
  traitNO <- traitNO_raw %>% 
    mutate(Leaf_Thickness_Ave_mm = (Leaf_Thickness_1_mm + Leaf_Thickness_2_mm + Leaf_Thickness_3_mm)/3) %>% 
    mutate(Gradient = case_when(Site %in% c("Fau", "Alr", "Ulv") ~ as.character(1),
                                Site %in% c("Vik", "Hog", "Lav") ~ as.character(2),
                                Site %in% c("Arh", "Ram", "Gud") ~ as.character(3),
                                Site %in% c("Ovs", "Ves", "Skj") ~ as.character(4)
    )) %>%
    # mutate(BlockID = as.character(1),
    #        PlotID = as.character(1)) %>% 
    select(Country, Year, Site, Gradient, Taxon, Plant_Height_cm, Wet_Mass_g, Dry_Mass_g, Leaf_Thickness_Ave_mm, Leaf_Area_cm2, SLA_cm2_g, LDMC, N_percent, C_percent, CN_ratio) %>% 
    gather(key = Trait, value = Value, -Country, -Year, -Site, -Gradient, -Taxon) %>% 
    filter(!is.na(Value)) %>% 
    mutate(Taxon = recode(Taxon, "Empetrum nigrum subsp. Hermaphroditum" = "Empetrum nigrum")) %>% 
    # remove white space in Viola palustris
    mutate(Taxon = trimws(Taxon, which = "right")) %>% 
    filter(!is.na(Value)) %>% 
    mutate(Country = "NO",
           BlockID = "dummyBlockID",
           PlotID = "dummyPlotID") #Overwrite junk...
  
  return(traitNO)
}


#Cleaning Norway Flux
CleanNorwayFlux <- function(fluxNO_raw){
  fluxNO <- fluxNO_raw %>% 
    mutate(Site = tolower(Site)) %>% 
    mutate(Site = paste(toupper(substr(Site, 1, 1)), substr(Site, 2, 3), sep = "")) %>% 
    mutate(Gradient = case_when(Site %in% c("Fau", "Alr", "Ulv") ~ as.character(1),
                                Site %in% c("Vik", "Hog", "Lav") ~ as.character(2),
                                Site %in% c("Arh", "Ram", "Gud") ~ as.character(3),
                                Site %in% c("Ovs", "Ves", "Skj") ~ as.character(4)
    )) 
  
  return(fluxNO)
}


#### IMPORT, CLEAN AND MAKE LIST #### 
ImportClean_Norway <- function(){
  

  ## DOWNLOAD DATA FROM OSF
  # meta
  get_file(node = "7mzjk",
           file = "metaNO.csv",
           path = "data_cleaned",
           remote_path = "Norway")
  
  ## IMPORT DATA
  # meta data
  metaNO = read_csv(file_in("data_cleaned/metaNO.csv"))
  # meta community
  metaCommunityNO_raw = get(load(file = file_in("data/metaCommunityNO_2016.Rdata")))
  # community and sp data
  communityNO_raw <- read_excel(file_in("data/funcab_composition_2016.xlsx"))
  #communityNO_raw = target(drop_and_load.xlsx(myfile = "transplant/USE THIS DATA/Norway/funcab_composition_2016.xlsx", localpath = "data/funcab_composition_2016.xlsx"), trigger = trigger(change = drop_get_metadata(path = "transplant/USE THIS DATA/Norway/funcab_composition_2016.xlsx")$content_hash))
  spNO = read_excel(file_in("data/fsystematics_species.xlsx"))
  #spNO = target(drop_and_load.xlsx(myfile = "transplant/USE THIS DATA/Norway/systematics_species.xlsx", localpath = "data/fsystematics_species.xlsx"), trigger = trigger(change = drop_get_metadata(path = "transplant/USE THIS DATA/Norway/systematics_species.xlsx")$content_hash))
  # trait
  traitNO_raw <- read_delim(file = file_in("data/traitdata_NO.csv"), col_names = TRUE, delim = ",")
  #traitNO_raw = target(drop_and_load.csv(myfile = "transplant/USE THIS DATA/Norway/traitdata_NO.csv", localpath = "data/traitdata_NO.csv"), trigger = trigger(change = drop_get_metadata(path = "transplant/USE THIS DATA/Norway/traitdata_NO.csv")$content_hash))
  # flux
  fluxNO_raw <- get(load(file_in("data/standardControlFluxNO_2016.Rdata")))
  hierarchyNO = c("Country", "Site")
  
  ### CLEAN DATA
  metaCommunityNO = CleanNorwayMetaCommunity(metaCommunityNO_raw)
  communityNO = CleanNorwayCommunity(communityNO_raw, spNO)
  traitNO = CleanNorwayTrait(traitNO_raw)
  fluxNO = CleanNorwayFlux(fluxNO_raw)
  
  # Make list
  Data_NO = list(meta = metaNO,
                 metaCommunity = metaCommunityNO,
                 community = communityNO,
                 trait = traitNO,
                 trait_hierarchy = hierarchyNO,
                 flux = fluxNO)
  
  return(Data_NO)
}


