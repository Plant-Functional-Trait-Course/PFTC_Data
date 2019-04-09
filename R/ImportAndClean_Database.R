######################
### Database data  ###
######################


#### CLEANING DATA ####

CleanDatabaseTrait <- function(Database_trait_raw){
  Database_trait <- Database_trait_raw %>% 
    select(trait_name %in% c("leaf area", "leaf area per leaf dry mass", "leaf carbon content per leaf nitrogen content", "leaf delta 13c", "leaf delta 15 n", "leaf dry mass", "leaf dry mass per leaf fresh mass", "leaf fresh mass", "leaf thickness", "whole plant height", "whole plant height vegetative")) %>% 
    rename(Taxon = taxon, Trait = trait_name, Value = trait_value) %>% 
    

  return(Database_trait)
}

#### IMPORT, CLEAN AND MAKE LIST #### 
ImportClean_Database <- function(){
  
  ### IMPORT DATA
  # trait
  Database_trait_raw <- readRDS(file = file_in("data/BIEN_TTT_global_pftc_data.rds"), refhook = NULL)
  
  ### CLEAN DATA
  traitDatabase = CleanDatabaseTrait(Database_raw)
  
  # Make list
  Database = list(trait = traitDatabase)
  
  return(Database)
}
