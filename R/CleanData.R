##############################
### CODE FOR CLEANING DATA ###
##############################

# Clean China trait data
CleanChinaTrait <- function(dat){
  dat2 <- dat %>% 
    filter(Project %in% c("LOCAL", "0", "C")) %>% 
    mutate(Treatment = plyr::mapvalues(Project, c("C", "0", "LOCAL"), c("control", "local", "gradient"))) %>% 
    mutate(Year = year(Date),
           Country = "CH",
           Gradient = 1,
           Project = "T") %>% 
    rename(BlockID = Location) %>%
    mutate(PlotID = paste(BlockID, Treatment, sep = "_"),
           ID = paste(Site, Treatment, Taxon, Individual_number, Leaf_number, sep = "_")) %>% 
    select(ID, Country, Year, Project, Treatment, Site, Elevation, BlockID, PlotID, Taxon, Individual_number, Leaf_number, Wet_Mass_g, Dry_Mass_g, Leaf_Thickness_Ave_mm, Leaf_Area_cm2, SLA_cm2_g, LDMC, Leaf_Thickness_1_mm, Leaf_Thickness_2_mm, Leaf_Thickness_3_mm, AreaFlag, WetFlag, DryFlag, ThickFlag, GeneralFlag, allComments, C_percent, N_percent , CN_ratio, dN15_percent, dC13_percent, P_AVG, P_Std_Dev, P_Co_Var)
  
  return(dat2)
}



# Cleaning China community data
CleanChinaCommunity <- function(dat){
  dat2 <- dat %>% 
    filter(TTtreat %in% c("control", "local")) %>% 
    rename(Year = year, Site = originSiteID, BlockID = originBlockID, PlotID = turfID, Treatment = TTtreat, Taxon = speciesName, Cover = cover) %>% 
    mutate(Country = "CH",
           Gradient = 1) %>% 
    select(Country, Gradient, Year, Site, BlockID, PlotID, Treatment, Taxon, Cover)
  
  return(dat2)
}

# Cleaning Svalbard community
CleanSvalbardCommunity <- function(dat){
  dat2 <- dat %>% 
  rename(Latitude = Latitude_N, Longitude = Longitude_E, Elevation = Elevation_m)
  
  return(dat2)
}


CleanPeruCommunity <- function(dat){
  dat2 <- dat %>% 
    mutate(Country = "PE")
  return(dat2)
}
  
  

CleanNorwayMetaCommunity <- function(dat){
  dat2 <- dat %>% 
    rename(Site = SiteID)
  return(dat2)
}  

    
CleanNorwayCommunity <- function(dat){
  dat2 <- dat %>% 
    filter(Measure == "Cover") %>% 
    select(-Treatment, -'Nid herb', 'Nid gram', -'Nid rosett', -'Nid seedling', -liver, -lichen, -litter, -soil, -rock, -'#Seedlings', -TotalGraminoids, -totalForbs, -totalBryophytes, -vegetationHeight, -mossHeight, -comment, -'ver seedl', -canum, -totalVascular, totalBryophytes__1, -acro, -pleuro, -totalLichen) %>% 
    gather(key = Taxon, value = Cover, -Site, -Block, -turfID, -subPlot, -year, -date, -Measure, -recorder) %>% 
    filter(!is.na(Cover)) %>% 
    mutate(Site = substr(Site, 1, 3)) %>% 
    mutate(Country = "NO",
           Gradient = 1) %>% 
    rename(Year = year, BlockID = Block, PlotID = turfID)
  
  return(dat2)
}


CleanNorwayTrait <- function(dat){
  dat2 <- dat %>% 
    mutate(Leaf_Thickness_Ave_mm = (Leaf_Thickness_1_mm + Leaf_Thickness_2_mm + Leaf_Thickness_3_mm)/3)
  
  return(dat2)
}



CleanColoradoCommunity <- function(dat){
  dat2 <- dat %>% 
    filter(!species_or_ground_cover %in% c("Bare (Bare soil + Litter + Dead)", "Rock", "Total Graminoid", "Total Herb", "Total Shrub", "Bare soil", "Litter", "Dead")) %>% 
    select(-X16, -X17, -X18, -X19, -X20, -X21, -X22, -X23, -X24, -X25, -X26, -plot1_count, -plot2_count, -plot3_count, -plot4_count, -plot5_count, -total_site_percent) %>% 
    gather(key = PlotID, value = Cover, -site, -date_yyyymmdd, -species_or_ground_cover, -growth_habit) %>% 
    mutate(Country = "CO",
           Year = 2016) %>% 
    rename(Site = site, Taxon = species_or_ground_cover, functionalGroup = growth_habit)
  return(dat2)
}  


CleanColoradoTrait <- function(dat){
  dat2 <- dat %>% 
    select(year, site, elev, block, taxon_std, family, no_of_leaves, leaf_area, wet_mass, dry_mass, SLA, LDMC, WC, height_flower, height_leaf, height, height_2, thickness_1, thickness_2, thickness_3, thickness, pc_C, pc_N, pc_P, d13C, d15N,  C_N,  N_C,  N_P) %>% 
    rename(Year = year, Site = site, Elevation = elev, PlotID = block, Taxon = taxon_std, Family = family, Leaf_Area_cm2 = leaf_area, Wet_mass_g = wet_mass, Dry_Mass_g = dry_mass, SLA_cm2_g = SLA, Plant_Height_cm = height_flower, Leaf_Thickness_1_mm = thickness_1, Leaf_Thickness_2_mm = thickness_2, Leaf_Thickness_3_mm = thickness_3, Leaf_Thickness_Ave_mm = thickness, C_percent = pc_C, N_percent = pc_N, dC13_percent = d13C, dN15_percent = d15N, CN_ratio = C_N, NC_ratio = N_C, NP_ratio = N_P, P_AVG = pc_P) %>%
    mutate(Country = "CO")
  return(dat2)
}  
