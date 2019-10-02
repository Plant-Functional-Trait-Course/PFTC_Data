### CALCULATE COMMUNITY WEIGHTED MEANS ###

# NORMAL WAY WITHOUT BOOTSTRAPPING
CalculateTraitMeans <- function(countrylist){
  
    meanTraits <- countrylist$trait %>% 
      select(-matches("Year")) %>% 
      # Global means
      
      group_by(Taxon, Trait_trans) %>%
      mutate(TraitMean_global = mean(Value_trans, na.rm = TRUE)) %>% 
      
      
      # Regional means
      group_by(Taxon, Trait_trans) %>% 
      mutate(TraitMean_regional = mean(Value_trans, na.rm = TRUE))
      
      # Site means (site level)
    if("Site" %in% names(meanTraits)){
      meanTraits <- meanTraits %>% 
        group_by(Site, Taxon, Trait_trans) %>%
        mutate(TraitMean_site = mean(Value_trans, na.rm = TRUE)) 
    }
    
    #If the dataset has plot level trait data: calculate the plot level trait mean
    if("PlotID" %in% names(meanTraits)){
      meanTraits <- meanTraits %>% 
        # Plot means
        group_by(PlotID, BlockID, Site, Taxon, Trait_trans) %>%
        mutate(TraitMean_plot = mean(Value_trans, na.rm = TRUE)) 
    } 
    
    meanTraits <- meanTraits %>% 
      select(-Value, -Value_trans) %>% 
      ungroup() %>% 
      distinct() %>% 
      mutate(TraitMean_global = if_else(Country == "Database",
                                        true = TraitMean_global,
                                        false = NA_real_), 
             TraitMean_regional = if_else(Country == "Database",
                                          true = NA_real_,
                                          false = TraitMean_regional))
  return(meanTraits)
}


# # Calculate global
#GlobalMeans <- function(countrylist){
#  meanTraits <- countrylist$trait %>% 
#    group_by(Taxon, Trait_trans) %>%
#    summarise(TraitMean_global = mean(Value_trans))

#   return(meanTraits)
# }


#country <- CountryList[[1]]
#trait <- TraitMeans
Community_TraitMeans <- function(countrylist, meantrait) {  
  
  dat2 <- countrylist[names(countrylist) != "Database"] %>% 
    map_df("community") %>% 
    # Calculate total sum of cover for each plot and percent cover per species per plot
    group_by(Country, Site, Gradient, BlockID, PlotID) %>% 
    mutate(SumCover = sum(Cover),
           PercentCover = Cover / SumCover * 100) %>% 
    
    # join site level means
    left_join(meantrait %>% 
      select(TraitMean_site, Country, Taxon, Site, Trait_trans) %>% 
        distinct(),
              by = c("Country", "Taxon", "Site")) %>% 
    
    # join regional means
    left_join(meantrait %>% 
      select(TraitMean_regional, Country, Taxon, Trait_trans) %>% 
      distinct(),
      by = c("Country", "Trait_trans", "Taxon")) %>% 
  
    # join global means
    left_join(meantrait %>% 
                filter(Country == "Database") %>% 
      select(TraitMean_global, Taxon, Trait_trans) %>%
        distinct(),
      by = c("Trait_trans", "Taxon")) %>% 
  
    # join plot level means
      left_join(meantrait %>% 
                  select(TraitMean_plot, Country, Taxon, Site, Trait_trans, BlockID, PlotID),
                by = c("Country", "Trait_trans", "Taxon", "Site", "BlockID", "PlotID")) %>% 
    ungroup()
  
  return(dat2)
}


Threshold_filter <- function(community_trait, trait_level) {  

  trait_level2 <- enquo(trait_level)
  
  # make new function for selecting 80% on different levels (plot, site, ...)
  dat2 <- community_trait %>% 
    group_by(Country, Site, Gradient, BlockID, PlotID, Trait_trans) %>% 
    mutate(PercentCover2 = ifelse(!is.na(!!trait_level2), PercentCover, NA),
           CommCover = sum(PercentCover2, na.rm = TRUE)) %>% 
    ungroup() %>% 
   filter(CommCover > 80)
  
  return(dat2)
}


CommunityW_TraitMeans <- function(community_trait) {  
  
  ### Calculate Community weighted means
  dat2 <- community_trait %>%
  gather(key = TraitLevel, value = TraitMean, TraitMean_plot, TraitMean_site, TraitMean_regional, TraitMean_global) %>% 
  group_by(Country, Site, BlockID, PlotID, Trait_trans, TraitLevel) %>% 
  mutate(CWTraitMean = weighted.mean(TraitMean, Cover, na.rm=TRUE)) %>% 
  ungroup()

  return(dat2)
}

# Reduce to only CWMeans
CommunityW_Means <- function(Full_CWTraitMeans){
  CWTraitMeans <- Full_CWTraitMeans %>% 
    filter(!is.na(Trait_trans)) %>% 
    select(-Taxon, -Cover, -TraitMean) %>% 
    distinct()
  
  return(CWTraitMeans)
}


# TRANSFORMING THE TRAITS
LogTransformation <- function(Country){

#Making a function to log transform some traits in a new column of the previous trait dataset
  fun <- . %>% 
    mutate(Value_trans = ifelse(Trait %in% c("Plant_Height_cm", "Wet_Mass_g", "Dry_Mass_g", "Leaf_Area_cm2"), suppressWarnings(log(Value)), Value),
           Trait_trans = recode(Trait, "Plant_Height_cm" = "Plant_Height_cm_log", "Wet_Mass_g" = "Wet_Mass_g_log", "Dry_Mass_g" = "Dry_Mass_g_log", "Leaf_Area_cm2" = "Leaf_Area_cm2_log"))
  
#If the dataset is a fata frame it will run the function on that, if it is a list it iwll run the function on the trait list
  
  if(inherits(Country, "data.frame")){
    Country <- Country %>% fun()
  } else {
    Country$trait <- Country$trait %>% fun()
  }
  
  return(Country)
}


