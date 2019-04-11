######################
### Database data  ###
######################


#### CLEANING DATA ####

CleanDatabaseTrait <- function(Database_trait_raw){
  Database_trait <- Database_trait_raw %>% 
    Global1 <- Global %>% 
    filter(trait_name %in% c("leaf area", "leaf area per leaf dry mass", "leaf carbon content per leaf nitrogen content", "leaf delta 13c", "leaf delta 15n", "leaf dry mass", "leaf fresh mass", "leaf thickness", "whole plant height", "whole plant height vegetative", "leaf dry mass per leaf fresh mass")) %>% 
    rename(Taxon = taxon, Trait = trait_name, Value = trait_value) %>%
    mutate(unit = as.character(unit)) %>% 
    mutate(comment = as.character(comment)) %>% 
    mutate(Value = as.numeric(Value)) %>% 
    mutate(Trait = recode(Trait, "leaf area" = "Leaf_Area_cm2",
                          "leaf area per leaf dry mass" = "SLA_cm2_g",
                          "leaf carbon content per leaf nitrogen content" = "CN_ratio",
                          "leaf delta 13c" = "dC13_percent",
                          "leaf delta 15n" = "dN15_percent",
                          "leaf dry mass" = "Dry_Mass_g",
                          "leaf fresh mass" = "Wet_Mass_g",
                          "leaf thickness" = "Leaf_Thickness_Ave_mm",
                          "whole plant height" = "Plant_Height_cm",
                          "whole plant height vegetative" = "Plant_Height_cm",
                          "leaf dry mass per leaf fresh mass" = "LDMC"))
  return(Database_trait)
}

#### IMPORT, CLEAN AND MAKE LIST #### 
ImportClean_Database <- function(){
  
  ### IMPORT DATA
  # BIEN and TundraTraitTeam
  Database_BIEN_TTT_raw <- readRDS(file = file_in("data/BIEN_TTT_global_pftc_data.rds"), refhook = NULL)
  
  # TRY
  Database_TRY_raw <- read_delim("data/TRY_data.txt", delim = "\t", quote = "", col_names = TRUE)
  #read_delim from reader package
  
  ### CLEAN DATA
  traitDatabase = CleanDatabaseTrait(Database_raw)
  
  # Make list
  Database = list(trait = traitDatabase)
  
  return(Database)
}
