##############################
### CODE FOR CLEANING DATA ###
##############################

# Clean China trait data
CleanChinaTrait <- function(dat){
  dat2 <- dat %>% 
    filter(Project %in% c("LOCAL", "0", "C")) %>% 
    mutate(Treatment = plyr::mapvalues(Project, c("C", "0", "LOCAL"), c("control", "local", "gradient"))) %>% 
    mutate(Year = year(Date),
           Country = "China",
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
    mutate(Country = "China",
           Gradient = 1) %>% 
    select(Country, Gradient, Year, Site, BlockID, PlotID, Treatment, Taxon, Cover)
  
  return(dat2)
}



