### BOOTSTRAPPING METHOD FOR CWM

#countrylist <- CountryList

CWM_Bootstrapping <- function(countrylist, nrep = 100, samplesize = 200){
  comm <- countrylist$China$community %>% 
    filter(!Cover == 0) %>% # can be removed when SV is fixed
    group_by(Country, Year, Site, Gradient, BlockID, PlotID) %>% 
    mutate(sumCover = sum(Cover))
  
  trait <- countrylist$China$trait
  
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




