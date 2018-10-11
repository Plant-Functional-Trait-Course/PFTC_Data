##############################
### CODE FOR CLEANING DATA ###
##############################


### CHINA
# Clean China trait data
CleanChinaTrait <- function(dat){
  dat2 <- dat %>% 
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
    filter(!is.na(Value))
  
  return(dat2)
}



# Cleaning China community data
CleanChinaCommunity <- function(dat){
  dat2 <- dat %>% 
    filter(TTtreat %in% c("control", "local")) %>% 
    filter(year == 2016) %>% 
    rename(Year = year, Site = originSiteID, BlockID = originBlockID, PlotID = turfID, Treatment = TTtreat, Taxon = speciesName, Cover = cover) %>% 
    mutate(Country = "CH",
           Gradient = as.character(1)) %>% 
    select(Country, Year, Site, Gradient, BlockID, PlotID, Taxon, Cover) %>% 
    mutate(Taxon = recode(Taxon, "Potentilla stenophylla var. emergens" = "Potentilla stenophylla")) %>% 
    filter(is.na(Cover), !Cover == 0)

  return(dat2)
}

# Cleaning China meta community data
CleanChinaMetaCommunity <- function(dat){
  dat2 <- dat %>% 
    select(PlotID, Year, Moss, Lichen2, Litter, BareGround, Rock, Vascular, Bryophyte, Lichen, MedianHeight_cm, MedianMossHeight_cm) %>% 
    mutate(Country = "CH",
           Site = substr(PlotID, 1,1),
           Gradient = "1")
    return(dat2)
}

CleanSvalbardMeta <- function(dat){
  dat2 <- dat %>% 
    mutate(Elevation = as.numeric(as.character(Elevation)),
           Gradient = "1")
  
  return(dat2)
}


#____________________________________________________________________
### SVALBARD
# Cleaning Svalbard meta
CleanSvalbardMeta <- function(dat){
  dat2 <- dat %>% 
    mutate(Site = paste(Site, Gradient, sep =""))
  
  return(dat2)
}

# Cleaning Svalbard community
CleanSvalbardCommunity <- function(dat){
  dat2 <- dat %>% 
  rename(Latitude = Latitude_N, Longitude = Longitude_E, Elevation = Elevation_m)%>%
  mutate(PlotID = paste(Site, Gradient, PlotID, sep=""),
         Site = paste(Site, Gradient, sep =""),
         BlockID = as.character(1),
         Cover = as.numeric(Cover)) %>% 
    select(Country, Year, Site, Gradient, BlockID, PlotID, Taxon, Cover) %>% 
    mutate(Taxon = recode(Taxon, "micranthes hieracifolia" = "micranthes hieraciifolia")) %>% 
    filter(is.na(Cover), !Cover == 0)
  
  return(dat2)
}

# Cleaning Svalbard meta community
CleanSvalbardMetaCommunity <- function(dat){
  dat2 <- dat %>%
    mutate(Lichen_rock = gsub("_", ".", Lichen_rock),
           Lichen_rock = as.numeric(Lichen_rock)) %>% 
    mutate(Lichen = rowSums(select(., Lichen_soil, Lichen_rock), na.rm = TRUE)) %>% 
    rename(Bryophyte = Bryophytes) %>% 
    select(Gradient, Site, PlotID, MedianHeight_cm, Vascular, Bryophyte, Lichen, Rock, BareGround, BioCrust, Litter, Country, Year, Project)
    return(dat2)
}

# Cleaning Svalbard trait
CleanSvalbardTrait <- function(dat){
  dat2 <- dat %>% 
    rename(Latitude = Latitude_N, Longitude = Longitude_E, Elevation = Elevation_m)%>%
    mutate(PlotID = paste(Site, Gradient, PlotID, sep=""),
           BlockID = as.character(1),
           Site = paste(Site, Gradient, sep ="")
           ) %>%
    select(Country, Year, Site, Gradient, BlockID, PlotID, Taxon, Plant_Height_cm, Wet_Mass_g, Dry_Mass_g, Leaf_Thickness_Ave_mm, Leaf_Area_cm2, SLA_cm2_g, LDMC) %>% 
    gather(key = Trait, value = Value, -Country, -Year, -Site, -Gradient, -BlockID, -PlotID, -Taxon) %>% 
    filter(!is.na(Value))
  
  return(dat2)
}

#___________________________________________________________________________
### PERU
# Cleaning Peru metaCommunity

CleanPeruMetaCommunity <- function(dat){
  dat2 <- dat %>% 
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
  return(dat2)
}

# Cleaning Peru community

CleanPeruCommunity <- function(dat){
  dat2 <- dat %>% 
    mutate(Country = "PE", 
           BlockID = as.character(1),
           PlotID = paste(Site, PlotID, Treatment, sep="_"),
           Gradient = as.character(1)) %>% 
    select(Country, Year, Site, Gradient, BlockID, PlotID, Taxon, Cover)
  return(dat2)
}

#Cleaning Peru Trait

CleanPeruTrait <- function(dat){
  dat2 <- dat %>%
    mutate(BlockID = as.character(1),
           PlotID = paste(Site, PlotID, Treatment, sep="_"),
           Gradient = as.character(1)) %>%
  select(Country, Year, Site, Gradient, BlockID, PlotID, Taxon, Plant_Height_cm, Wet_Mass_g, Dry_Mass_g, Leaf_Thickness_Ave_mm, Leaf_Area_cm2, SLA_cm2_g, LDMC) %>% 
    gather(key = Trait, value = Value, -Country, -Year, -Site, -BlockID, -PlotID, -Gradient, -Taxon) %>% 
    mutate(Taxon = recode(Taxon, "Agrostis 2" = "Agrostis sp2")) %>% 
    filter(!is.na(Value))
  
  return(dat2)
  
}  

#____________________________________________________________________
### NORWAY
#Cleaning Norway Meta Community 

CleanNorwayMetaCommunity <- function(dat){
  dat2 <- dat %>%
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
  return(dat2)
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
    
CleanNorwayCommunity <- function(dat, sp){
  sp <- sp %>% 
    mutate(Species = gsub(" ", "_", Species))
  
  dat2 <- dat %>% 
    filter(Measure == "Cover") %>%
    select(-Treatment, -'Nid herb', 'Nid gram', -'Nid rosett', -'Nid seedling', -liver, -lichen, -litter, -soil, -rock, -'#Seedlings', -TotalGraminoids, -totalForbs, -totalBryophytes, -vegetationHeight, -mossHeight, -comment, -'ver seedl', -canum, -totalVascular, totalBryophytes__1, -acro, -pleuro, -totalLichen) %>% 
    gather(key = Taxon, value = Cover, -Site, -Block, -turfID, -subPlot, -year, -date, -Measure, -recorder) %>% 
    filter(!is.na(Cover)) %>% 
    mutate(Taxon = gsub(" ", "_", Taxon))%>%
    mutate(Taxon = plyr::mapvalues(Taxon, from = Dictionary_communityNO$old, to = Dictionary_communityNO$new)) %>%
    left_join(sp, by = c("Taxon" = "Species")) %>% 
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
    filter(is.na(Cover), !Cover == 0)
  
  return(dat2)
}

#Cleaning Norway Trait

CleanNorwayTrait <- function(dat){
  dat2 <- dat %>% 
    mutate(Leaf_Thickness_Ave_mm = (Leaf_Thickness_1_mm + Leaf_Thickness_2_mm + Leaf_Thickness_3_mm)/3) %>% 
    mutate(Gradient = case_when(Site %in% c("Fau", "Alr", "Ulv") ~ as.character(1),
                                Site %in% c("Vik", "Hog", "Lav") ~ as.character(2),
                                Site %in% c("Arh", "Ram", "Gud") ~ as.character(3),
                                Site %in% c("Ovs", "Ves", "Skj") ~ as.character(4)
    )) %>%
    mutate(BlockID = as.character(1),
           PlotID = as.character(1)) %>% 
    select(Country, Year, Site, BlockID, PlotID, Gradient, Taxon, Plant_Height_cm, Wet_Mass_g, Dry_Mass_g, Leaf_Thickness_Ave_mm, Leaf_Area_cm2, SLA_cm2_g, LDMC, N_percent, C_percent, CN_ratio) %>% 
    gather(key = Trait, value = Value, -Country, -Year, -Site, -BlockID, -PlotID, -Gradient, -Taxon) %>% 
    filter(!is.na(Value)) %>% 
    mutate(Taxon = recode(Taxon, "Empetrum nigrum subsp. Hermaphroditum" = "Empetrum nigrum")) %>% 
    filter(!is.na(Value))
  
  return(dat2)
}

#Cleaning Norway Flux

CleanNorwayFlux <- function(dat){
  dat2 <- dat %>% 
    mutate(Site = tolower(Site)) %>% 
    mutate(Site = paste(toupper(substr(Site, 1, 1)), substr(Site, 2, 3), sep = "")) %>% 
    mutate(Gradient = case_when(Site %in% c("Fau", "Alr", "Ulv") ~ as.character(1),
                                Site %in% c("Vik", "Hog", "Lav") ~ as.character(2),
                                Site %in% c("Arh", "Ram", "Gud") ~ as.character(3),
                                Site %in% c("Ovs", "Ves", "Skj") ~ as.character(4)
    )) 
  
  return(dat2)
}

#____________________________________________________________________
### COLORADO
# Cleaning Colorado meta
CleanColoradoMeta <- function(dat){
  dat2 <- dat %>% 
    mutate(Country = "CO")
  
  return(dat2)
}

#Cleaning Colorado Community

CleanColoradoCommunity <- function(dat){
  dat2 <- dat %>%
    filter(!is.na(site)) %>% 
    filter(!species_or_ground_cover %in% c("Bare (Bare soil + Litter + Dead)", "Rock", "Total Graminoid", "Total Herb", "Total Shrub", "Bare soil", "Litter", "Dead")) %>% 
    select(-X16, -X17, -X18, -X19, -X20, -X21, -X22, -X23, -X24, -X25, -X26, -plot1_count, -plot2_count, -plot3_count, -plot4_count, -plot5_count, -total_site_percent) %>% 
    gather(key = PlotID, value = Cover, -site, -date_yyyymmdd, -species_or_ground_cover, -growth_habit) %>% 
    mutate(Country = "CO",
           Year = 2016) %>% 
    rename(Site = site, Taxon = species_or_ground_cover, functionalGroup = growth_habit) %>%
    mutate(PlotID = recode(PlotID, "plot_3_pct" = "plot3_pct"),
           Gradient = as.character(1),
           BlockID = as.character(1)) %>% 
    mutate(PlotID = recode(PlotID, "plot1_pct" = "1", "plot2_pct" = "2", "plot3_pct" = "3", "plot4_pct" = "4", "plot5_pct" = "5"),
           PlotID = paste(Site, PlotID, sep = "_")) %>% 
    select(Country, Year, Site, BlockID, PlotID, Gradient, Taxon, Cover) %>% 
    filter(is.na(Cover), !Cover == 0)
  return(dat2)
}

#Cleaning Colorado Meta Community

CleanColoradoMetaCommunity <- function(dat){
  dat2 <- dat %>%
    filter(!is.na(Site)) %>% 
    mutate(PlotID = recode(PlotID, "plot_3_pct" = "plot3_pct"),
           Gradient = as.character(1)) %>% 
    mutate(PlotID = recode(PlotID, "plot1_pct" = "1", "plot2_pct" = "2", "plot3_pct" = "3", "plot4_pct" = "4", "plot5_pct" = "5"),
           PlotID = paste(Site, PlotID, sep = "_")) %>% 
  spread(key = Group, value = Cover) %>%
    select(-Date.y, -MeanHeight_cm.y) %>% 
    rename(Date = Date.x, MeanHeight_cm = MeanHeight_cm.x, Graminoid = 'Total Graminoid', Herb = 'Total Herb', Shrub = 'Total Shrub', Bare_soil_litter_dead = 'Bare (Bare soil + Litter + Dead)', BareSoil = 'Bare soil')
    
    return(dat2)
}

#Cleaning Colorado Flux

CleanColoradoFlux <- function(dat){
dat2 <- dat
  return(dat2)
}  

#Cleaning Colorado Trait

CleanColoradoTrait <- function(dat){
  dat2 <- dat %>% 
    filter(!is.na(site)) %>% 
    select(year, site, block, taxon_std, leaf_area, wet_mass, dry_mass, SLA, height_flower, height_leaf, height, height_2, thickness, pc_C, pc_N, pc_P, d13C, d15N,  C_N,  N_C,  N_P) %>% 
    rename(Year = year, Site = site, PlotID = block, Taxon = taxon_std, Leaf_Area_cm2 = leaf_area, Wet_Mass_g = wet_mass, Dry_Mass_g = dry_mass, SLA_cm2_g = SLA, Plant_Height_cm = height_flower, Leaf_Thickness_Ave_mm = thickness, C_percent = pc_C, N_percent = pc_N, dC13_percent = d13C, dN15_percent = d15N, CN_ratio = C_N, NC_ratio = N_C, NP_ratio = N_P, P_AVG = pc_P) %>%
    filter(Site %in% c("Almont", "CBT", "Road", "Pfeiler", "PBM", "Cinnamon")) %>%
    mutate(Country = "CO",
           LDMC = Dry_Mass_g/Wet_Mass_g) %>%
    mutate(Gradient = as.character(1),
           BlockID = as.character(1),
           PlotID = paste(Site, gsub("Block", "", PlotID), sep = "_")) %>% 
    select(Country, Year, Site, Gradient, BlockID, PlotID, Taxon, Plant_Height_cm, Wet_Mass_g, Dry_Mass_g, Leaf_Thickness_Ave_mm, Leaf_Area_cm2, SLA_cm2_g, LDMC, C_percent, N_percent, dC13_percent, dN15_percent, CN_ratio, NC_ratio, NP_ratio, P_AVG) %>% 
    gather(key = Trait, value = Value, -Country, -Year, -Site, -BlockID, -PlotID, -Gradient, -Taxon) %>% 
    filter(!is.na(Value)) %>% 
    mutate(Value = as.numeric(Value)) %>% 
    filter(!is.na(Value))
  
  return(dat2)
}  


#____________________________________________________________________
#Cleaning Meta Bioclim

CleanMetaBioclim <- function(metaBioclim_raw){
  metaBioclim2 <- metaBioclim_raw %>% 
    mutate(Site = ifelse(Country == "SV", paste(Site, Gradient, sep = ""), Site)) %>% 
    left_join(metaSV, by = c("Country", "Gradient", "Elevation", "Latitude", "Longitude")) %>% 
    mutate(Site = ifelse(Country == "SV", Site.y, Site.x)) %>% 
    select(-Site.x, Site.y) %>% 
    select(Country, Site, Elevation:bio9) %>% 
    rename(AnnMeanTemp = "bio1",
           MeanDiurnalRange = "bio2",
           Isothermal = "bio3",
           TempSeasonal = "bio4",
           MaxTempWarmMonth = "bio5",
           MaxTempColdMonth = "bio6",
           TempAnnRange = "bio7",
           MeanTempWetQuart = "bio8",
           MeanTempDryQuart = "bio9",
           MeanTempWarmQuart = "bio10",
           MeanTempColdQuart = "bio11",
           AnnPrecip = "bio12",
           PrecipWetMonth = "bio13",
           PrecipDryMonth = "bio14",
           PrecipSeasonal = "bio15",
           PrecipWetQuart = "bio16",
           PrecipDryQuart = "bio17",
           PrecipWarmQuart = "bio18",
           PrecipColdQuart = "bio19")
    return(metaBioclim2)
}


#Cleaning CWMeans Bootstrapping
CleanCWMeansBoot <- function(CW_Means_Bootstrapped_raw){
  CW_Means_Bootstrapped <- CW_Means_Bootstrapped_raw %>% 
    as.tibble() %>% 
    mutate(turf = gsub("plot|plot_|_pct", "", turf)) %>%
    mutate(Country = substr(turf, 1, 2),
           PlotID = gsub("^\\w{2}_", "", turf)) %>% 
    mutate(PlotID = ifelse(Country == "SV", 
                           paste0(substr(PlotID, 3, 3), substr(PlotID, 1, 1), substr(PlotID, 5, 5)),
                           PlotID)) %>% 
    rename(Year = year, Trait = trait, turfID = turf) %>% 
    select(Country, PlotID, turfID, Year, Trait, mean_lower:kurt_upper) %>% 
    mutate_at(vars(mean_lower:kurt_upper), ~as.numeric(as.character(.)))
  
  return(CW_Means_Bootstrapped)
}
