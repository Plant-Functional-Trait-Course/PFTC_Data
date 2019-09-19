######################
### Database data  ###
######################


#### CLEANING DATA ####

# BIEN and TundraTraitTeam data cleaning

CleanDatabase_BIEN_TTT_Trait <- function(Database_BIEN_TTT_raw){
  Database_BIEN_TTT_trait <- Database_BIEN_TTT_raw %>% 
    filter(trait_name %in% c("leaf area", "leaf area per leaf dry mass", "leaf carbon content per leaf nitrogen content", "leaf delta 13c", "leaf delta 15n", "leaf dry mass", "leaf fresh mass", "leaf thickness", "whole plant height", "whole plant height vegetative", "leaf dry mass per leaf fresh mass")) %>% 
    rename(Taxon = taxon, Trait = trait_name, Value = trait_value) %>%
    filter(!Value == "*") %>% 
    mutate(unit = as.character(unit),
           comment = as.character(comment),
           Value = as.numeric(Value),
           Taxon = as.character(Taxon)) %>% 
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
  return(Database_BIEN_TTT_trait)
}

#TRY data cleaning

CleanDatabase_TRY_Trait <- function(Database_TRY_raw){
  Database_TRY_trait <- Database_TRY_raw %>% 
    filter(TraitName %in% c("Leaf area (in case of compound leaves: leaflet, petiole and rachis inlcuded)", "Leaf area per leaf dry mass (SLA or 1/LMA): petiole and rachis included", "Leaf carbon/nitrogen (C/N) ratio", "Leaf dry mass", "Leaf fresh mass", "Leaf thickness", "Plant height vegetative", "Leaf dry mass per leaf fresh mass (leaf dry matter content, LDMC)"))%>% 
    select(DatasetID, ObservationID, AccSpeciesName, TraitName, StdValue, UnitName, ValueKindName) %>% 
    filter(ValueKindName %in% c("Single", "Best estimate", "Mean", "Median", "Site specific mean")) %>% 
    rename(Taxon = AccSpeciesName, Trait = TraitName, Value = StdValue, unit = UnitName) %>%
    mutate(Value = as.numeric(Value)) %>% 
    mutate(Trait = recode(Trait, "Leaf area (in case of compound leaves: leaflet, petiole and rachis inlcuded)" = "Leaf_Area_cm2",
                            "Leaf area per leaf dry mass (SLA or 1/LMA): petiole and rachis included" = "SLA_cm2_g",
                            "Leaf carbon/nitrogen (C/N) ratio" = "CN_ratio",
                            "Leaf dry mass" = "Dry_Mass_g",
                            "Leaf fresh mass" = "Wet_Mass_g",
                            "Leaf thickness" = "Leaf_Thickness_Ave_mm",
                            "Plant height vegetative" = "Plant_Height_cm",
                            "Leaf dry mass per leaf fresh mass (leaf dry matter content, LDMC)" = "LDMC"))
  
TRY_meta_info <- Database_TRY_raw %>% 
  select(DatasetID, ObservationID, DataName, StdValue, Reference) %>% 
  filter(DataName %in% c("Latitude", "Longitude")) %>% 
  spread(key = DataName, value = StdValue) %>% 
  rename(latitude = Latitude, longitude = Longitude) %>% 
  distinct()

Database_TRY_trait <- Database_TRY_trait %>%
  left_join(TRY_meta_info, by =c("ObservationID", "DatasetID"))

    return(Database_TRY_trait)
}

# Standardise the units
#Make dictionary for unit transformation
traitdict <- tribble(~Trait, ~unit, ~transform,
                     "CN_ratio", "ratio", 1,
                     "CN_ratio", "g/g", 1,
                     "dC13_percent", "parts per thousand", 1, 
                     "dN15_percent", "parts per thousand", 1, 
                     "Dry_Mass_g", "g", 1,
                     "Dry_Mass_g", "mg", 0.001,
                     "LDMC", " ", 1,
                     "LDMC", "g/g", 1,
                     "LDMC", "mg.g-1", 0.001,
                     "Leaf_Area_cm2", "cm2", 1,
                     "Leaf_Area_cm2", "mm2", 0.001,
                     "Leaf_Thickness_Ave_mm", "mm", 1,
                     "N_percent", "mg/g", 0.001,
                     "Plant_Height_cm", "m", 100,
                     "SLA_cm2_g", "m2.kg-1", 10,
                     "SLA_cm2_g", "cm2_g", 1,
                     "SLA_cm2_g", "mm2 mg-1", 10,
                     "Wet_Mass_g", "g", 1
)

StandardiseDatabase <- function(Database, traitdict){
  ModifiedDatabase <- Database %>% 
  left_join(traitdict, by = c("Trait", "unit")) %>%
  mutate(value_2 = Value * transform) %>% 
  select(-Value) %>% 
  rename(Value = value_2)

return(ModifiedDatabase)
}


# Combining datasets

Combine_trait_database <- function(Database_BIEN_TTT_trait, Database_TRY_trait){
  Database_trait <- Database_BIEN_TTT_trait %>% 
    #bind_rows()
 return(Database_trait) 
}


#### IMPORT, CLEAN AND MAKE LIST #### 
ImportClean_Database <- function(){
  
  ### IMPORT DATA
  # BIEN and TundraTraitTeam
  Database_BIEN_TTT_raw <- readRDS(file = file_in("data/BIEN_TTT_global_pftc_data.rds"), refhook = NULL)
  
  # TRY
  Database_TRY_raw <- read_delim("data/TRY_data.txt", delim = "\t", quote = "", col_names = TRUE)
  
  ### CLEAN DATA
  traitDatabase_BIEN_TTT = CleanDatabase_BIEN_TTT_Trait(Database_BIEN_TTT_raw) %>% 
    StandardiseDatabase(traitdict)
  traitDatabase_TRY = CleanDatabase_TRY_Trait(Database_TRY_raw) %>% 
    StandardiseDatabase(traitdict)
  
  #Combine datasets
  traitDatabase = bind_rows(traitDatabase_BIEN_TTT, traitDatabase_TRY)
  Database = list(trait = traitDatabase)
  
  return(Database)
}
