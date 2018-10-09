##############################
### CODE FOR CLEANING DATA ###
##############################

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
    rename(Year = year, Site = originSiteID, BlockID = originBlockID, PlotID = turfID, Treatment = TTtreat, Taxon = speciesName, Cover = cover) %>% 
    mutate(Country = "CH",
           Gradient = as.character(1)) %>% 
    select(Country, Year, Site, Gradient, BlockID, PlotID, Taxon, Cover)

  return(dat2)
}


# Cleaning Svalbard meta
CleanSvalbardMeta <- function(dat){
  dat2 <- dat %>% 
    mutate(SiteID = paste(Site, Gradient, sep =""))
  
  return(dat2)
}

# Cleaning Svalbard community
CleanSvalbardCommunity <- function(dat){
  dat2 <- dat %>% 
  rename(Latitude = Latitude_N, Longitude = Longitude_E, Elevation = Elevation_m)%>%
  mutate(Site = paste(Site, Gradient, sep =""),
         PlotID = paste(Site, Gradient, PlotID, sep=""),
         BlockID = as.character(1),
         Cover = as.numeric(Cover)) %>% 
    select(Country, Year, Site, Gradient, BlockID, PlotID, Taxon, Cover)
  
  return(dat2)
}

# Cleaning Svalbard trait
CleanSvalbardTrait <- function(dat){
  dat2 <- dat %>% 
    rename(Latitude = Latitude_N, Longitude = Longitude_E, Elevation = Elevation_m)%>%
    mutate(Site = paste(Site, Gradient, sep =""),
           BlockID = as.character(1),
           PlotID = paste(Site, Gradient, PlotID, sep="")) %>%
    select(Country, Year, Site, Gradient, BlockID, PlotID, Taxon, Plant_Height_cm, Wet_Mass_g, Dry_Mass_g, Leaf_Thickness_Ave_mm, Leaf_Area_cm2, SLA_cm2_g, LDMC) %>% 
    gather(key = Trait, value = Value, -Country, -Year, -Site, -Gradient, -BlockID, -PlotID, -Taxon) %>% 
    filter(!is.na(Value))
  
  return(dat2)
}

# Cleaning Peru metaCommunity

CleanPeruMetaCommunity <- function(dat){
  dat2 <- dat %>% 
    mutate(Treatment = recode(Treatment, "burned" = "B", "control" = "C", "double_burned" = "BB"),
           PlotID = paste(PlotID, Treatment, sep=""),
           Gradient = as.character(1))
  return(dat2)
}

# Cleaning Peru community

CleanPeruCommunity <- function(dat){
  dat2 <- dat %>% 
    mutate(Country = "PE", 
           BlockID = as.character(1),
           PlotID = paste(PlotID, Treatment, sep=""),
           Gradient = as.character(1)) %>% 
    select(Country, Year, Site, Gradient, BlockID, PlotID, Taxon, Cover)
  return(dat2)
}

#Cleaning Peru Trait

CleanPeruTrait <- function(dat){
  dat2 <- dat %>%
    mutate(BlockID = as.character(1),
           PlotID = paste(PlotID, Treatment, sep=""),
           Gradient = as.character(1)) %>%
  select(Country, Year, Site, Gradient, BlockID, PlotID, Taxon, Plant_Height_cm, Wet_Mass_g, Dry_Mass_g, Leaf_Thickness_Ave_mm, Leaf_Area_cm2, SLA_cm2_g, LDMC) %>% 
    gather(key = Trait, value = Value, -Country, -Year, -Site, -BlockID, -PlotID, -Gradient, -Taxon) %>% 
    filter(!is.na(Value))
  return(dat2)
}  
  
#Cleaning Norway Meta Community 

CleanNorwayMetaCommunity <- function(dat){
  dat2 <- dat %>%
    rename(Site = SiteID) %>% 
    mutate(Gradient = case_when(Site %in% c("Fau", "Alr", "Ulv") ~ as.character(1),
                                Site %in% c("Vik", "Hog", "Lav") ~ as.character(2),
                                Site %in% c("Arh", "Ram", "Gud") ~ as.character(3),
                                Site %in% c("Ovs", "Ves", "Skj") ~ as.character(4))) %>% 
    mutate(BlockID = as.numeric(BlockID))
  return(dat2)
}  

#Cleaning Norway Community
    
CleanNorwayCommunity <- function(dat){
  dat2 <- dat %>% 
    filter(Measure == "Cover") %>%
    select(-Treatment, -'Nid herb', 'Nid gram', -'Nid rosett', -'Nid seedling', -liver, -lichen, -litter, -soil, -rock, -'#Seedlings', -TotalGraminoids, -totalForbs, -totalBryophytes, -vegetationHeight, -mossHeight, -comment, -'ver seedl', -canum, -totalVascular, totalBryophytes__1, -acro, -pleuro, -totalLichen) %>% 
    gather(key = Taxon, value = Cover, -Site, -Block, -turfID, -subPlot, -year, -date, -Measure, -recorder) %>% 
    filter(!is.na(Cover)) %>% 
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
    select(Country, Year, Site, Gradient, BlockID, PlotID, Taxon, Cover)
  
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

#Cleaning Colorado Community

CleanColoradoCommunity <- function(dat){
  dat2 <- dat %>%
    filter(!species_or_ground_cover %in% c("Bare (Bare soil + Litter + Dead)", "Rock", "Total Graminoid", "Total Herb", "Total Shrub", "Bare soil", "Litter", "Dead")) %>% 
    select(-X16, -X17, -X18, -X19, -X20, -X21, -X22, -X23, -X24, -X25, -X26, -plot1_count, -plot2_count, -plot3_count, -plot4_count, -plot5_count, -total_site_percent) %>% 
    gather(key = PlotID, value = Cover, -site, -date_yyyymmdd, -species_or_ground_cover, -growth_habit) %>% 
    mutate(Country = "CO",
           Year = 2016) %>% 
    rename(Site = site, Taxon = species_or_ground_cover, functionalGroup = growth_habit) %>%
    mutate(PlotID = recode(PlotID, "plot_3_pct" = "plot3_pct"),
           Gradient = as.character(1),
           BlockID = as.character(1)) %>% 
    select(Country, Year, Site, BlockID, PlotID, Gradient, Taxon, Cover)
  return(dat2)
}

#Cleaning Colorado Meta Community

CleanColoradoMetaCommunity <- function(dat){
  dat2 <- dat %>%
    mutate(PlotID = recode(PlotID, "plot_3_pct" = "plot3_pct"),
           Gradient = as.character(1))
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
    select(year, site, taxon_std, leaf_area, wet_mass, dry_mass, SLA, LDMC,  height_flower, height_leaf, height, height_2, thickness, pc_C, pc_N, pc_P, d13C, d15N,  C_N,  N_C,  N_P) %>% 
    rename(Year = year, Site = site, Taxon = taxon_std, Leaf_Area_cm2 = leaf_area, Wet_Mass_g = wet_mass, Dry_Mass_g = dry_mass, SLA_cm2_g = SLA, Plant_Height_cm = height_flower, Leaf_Thickness_Ave_mm = thickness, C_percent = pc_C, N_percent = pc_N, dC13_percent = d13C, dN15_percent = d15N, CN_ratio = C_N, NC_ratio = N_C, NP_ratio = N_P, P_AVG = pc_P) %>%
    filter(Site %in% c("Almont", "CBT", "Road", "Pfeiler", "PBM", "Monument")) %>%
    mutate(Country = "CO")%>%
    mutate(Gradient = as.character(1),
           BlockID = as.character(1),
           PlotID = as.character(1)) %>% 
    select(Country, Year, Site, Gradient, BlockID, PlotID, Taxon, Plant_Height_cm, Wet_Mass_g, Dry_Mass_g, Leaf_Thickness_Ave_mm, Leaf_Area_cm2, SLA_cm2_g, LDMC, C_percent, N_percent, dC13_percent, dN15_percent, CN_ratio, NC_ratio, NP_ratio, P_AVG) %>% 
    gather(key = Trait, value = Value, -Country, -Year, -Site, -BlockID, -PlotID, -Gradient, -Taxon) %>% 
    filter(!is.na(Value)) %>% 
    mutate(Value = as.numeric(Value)) %>% 
    filter(!is.na(Value))
  
  return(dat2)
}  
