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
      select(-Value_trans, -Value) %>% 
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
CommunityW_TraitMeans <- function(countrylist, meantrait) {  
  
  dat2 <- countrylist[names(countrylist) != "Database"] %>% 
    map_df("community") %>% 
    # Calculate total sum of cover for each plot and percent cover per species per plot
    group_by(Country, Site, Gradient, BlockID, PlotID) %>% 
    mutate(SumCover = sum(Cover), PercentCover = Cover / SumCover * 100) %>% 
    
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
                by = c("Country", "Trait_trans", "Taxon", "Site", "BlockID", "PlotID"))

# close function here!!!
  
  # make new function for selecting 80% on different levels (plot, site, ...)
  CommunityCover_PlotLevel <- dat2 %>% 
    group_by(Country, Site, Gradient, BlockID, PlotID, Trait_trans) %>% 
    mutate(PercentCover2 = ifelse(!is.na(TraitMean_plot), PercentCover, NA),
           CommCover_plot = sum(PercentCover2, na.rm = TRUE))
  
  # filter < 80%
  
# new function or in the selection funtion
  ### Calculate Community weighted means
  dat2 <- dat2 %>%
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


# WITH BOOTSTRAPPING
CWM_Bootstrapping <- function(dat, nrep = 100, samplesize = 200){
  comm <- dat$community %>% 
    filter(!Cover == 0) %>% 
    group_by(Country, Year, Site, Gradient, BlockID, PlotID) %>% 
    mutate(sumCover = sum(Cover))
  
  trait <- dat$trait_trans %>% filter(!is.na(Value))
  
  TraitWeights_plot <- comm %>% 
    left_join(trait %>% select(-Year), by = c("Country", "Site", "Gradient", "BlockID", "PlotID", "Taxon")) %>% 
    group_by(Country, Year, Site, Gradient, BlockID, PlotID, Taxon, Trait) %>% 
    mutate(weight = Cover/n()) %>% 
    group_by(Country, Year, Site, Gradient, BlockID, PlotID, Trait) 
  
  
  # Site level weights and traits
  TraitWeights_site <- comm %>%
    left_join(trait %>% select(-Year, -BlockID, -PlotID), by = c("Country", "Site", "Gradient", "Taxon")) %>% 
    group_by(Country, Year, Site, Gradient, Taxon, Trait) %>% 
    mutate(weight = Cover/n()) %>% 
    group_by(Country, Year, Site, Gradient, Trait) 
  
  
  # Global level weights and traits
  TraitWeights_global <- comm %>% 
    left_join(trait %>% select(-Year, -BlockID, -PlotID, -Site), by = c("Country", "Gradient", "Taxon")) %>% 
    group_by(Country, Year, Gradient, Taxon, Trait) %>% 
    mutate(weight = Cover/n()) %>% 
    group_by(Country, Year, Gradient, Trait) 
  
  
  TraitWeights_all <- bind_rows(plot = TraitWeights_plot, site = TraitWeights_site, global = TraitWeights_global, .id = "level") %>% 
    mutate(level = factor(level, levels = c("plot", "site", "global"), ordered = TRUE)) %>%
    filter(!is.na(Value)) %>% 
    group_by(Country, Year, Site, Gradient, BlockID, PlotID, Trait, Taxon) %>% 
    filter(level == min(level)) %>% 
    group_by(Country, Year, Site, Gradient, BlockID, PlotID, Trait)
  
  
  BootstrapMoments_All <- rerun(.n = nrep, sample_n(TraitWeights_all, size = samplesize,  replace = TRUE, weight = TraitWeights_all$weight)) %>%
    bind_rows(.id = "n") %>% 
    group_by(n, add = TRUE) %>% 
    # get all the happy moments
    summarise(Mean = mean(Value), Variance = var(Value), Skewness = skewness(Value), Kurtosis = kurtosis(Value))

  
   
  return(BootstrapMoments_All)
}

SummarizeBootMoments <- function(BootstrapMoments_All){
  # calculate means and 
  BootstrapMoments <- BootstrapMoments_All %>% 
    group_by(Country, Year, Site, Gradient, BlockID, PlotID, Trait) %>% 
    summarise(n = n(),
              meanMean = mean(Mean), CIlow.Mean = meanMean - sd(Mean), CIhigh.Mean = meanMean + sd(Mean),
              meanVar = mean(Variance), CIlow.Var = meanVar - sd(Variance), CIhigh.Var = meanVar + sd(Variance),
              meanSkew = mean(Skewness), CIlow.Skew = meanSkew - sd(Skewness), CIhigh.Skew = meanSkew + sd(Skewness),
              meanKurt = mean(Kurtosis), CIlow.Kurt = meanKurt - sd(Kurtosis), CIhigh.Kurt = meanKurt + sd(Kurtosis)) 
  
  return(BootstrapMoments)
}


#NEEDS TO BE INCORPORATED IN BOOTSTRAPPING FUNCTION !!!!!
#TraitWeights_all %>% 
  #group_by(Country, Year, Site, Gradient, BlockID, PlotID, Trait, Taxon) %>% 
  #slice(1) %>% 
  #group_by(Country, Year, Site, Gradient, BlockID, PlotID, Trait) %>% summarise(covered = sum(Cover)) %>% mutate(percent_cover = covered/sumCover * 100) %>% arrange(percent_cover) %>% pn


#### Filtering out turfs with less than 70% of the community present ###

#check_community_df <- wcommunity %>%
  #group_by(Site, Species, turfID)%>%
  #select(Site, turfID, Species, cover, SLA_mean, Lth_mean, Height_mean, LDMC_mean, LA_mean, CN_ratio_mean, sum_cover)%>%
  #unique()%>%
  #ungroup()%>%
  #group_by(turfID)%>%
  #mutate(cover_traits = (sum(cover)))%>%
  #filter(!is.na(SLA_mean))%>%
  #mutate(community_covered_trait=cover_traits/sum_cover*100)

#complete_turf <- check_community_df%>%
  #filter(community_covered_trait>80)%>%
  #distinct(turfID, .keep_all=TRUE)

#Complete_turfs<-as.vector(complete_turf$turfID)

#wcommunity_df <- filter(wcommunity, turfID %in% Complete_turfs)




