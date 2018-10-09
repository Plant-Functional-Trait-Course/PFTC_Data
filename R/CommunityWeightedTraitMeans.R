CommunityW_GlobalAndLocalMeans <- function(dat){
  meanTraits <- dat$trait %>% 
    # Global means
    group_by(Taxon, Trait) %>% 
    mutate(TraitMean_global = mean(Value, na.rm = TRUE)) %>% 
    
    # Site means (site level)
    group_by(Site, Taxon, Trait) %>%
    mutate(TraitMean_site = mean(Value, na.rm = TRUE)) %>% 
    
    # Plot means
    group_by(PlotID, BlockID, Site, Taxon, Trait) %>%
    mutate(TraitMean_plot = mean(Value, na.rm = TRUE)) %>% 
    select(-Year, -Value) %>% 
    ungroup() %>% 
    distinct()
    
  dat2 <- dat$community %>% 
    
    # join plot level means
    left_join(meanTraits %>% select(-TraitMean_global, -TraitMean_site)) %>% 
    # join site level means
    left_join(meanTraits %>% select(-TraitMean_global, -TraitMean_plot, -BlockID, -PlotID) %>% distinct()) %>% 
    
    # join global means
    left_join(meanTraits %>% select(-TraitMean_plot, -TraitMean_site, -Site, -BlockID, -PlotID) %>% distinct()) %>% 
    
    
    mutate(TraitMean = coalesce(TraitMean_plot, TraitMean_site, TraitMean_global))
  
  return(dat2)
}



#### Weighting the traits data by the community ####
group_by(PlotID, Site) %>%
  mutate(
    Wmean_Lth= weighted.mean(Lth_mean, Cover, na.rm=TRUE),
    Wmean_LA= weighted.mean(LA_mean, Cover, na.rm=TRUE),
    #Wmean_Height= weighted.mean(Height_mean, Cover, na.rm=TRUE),
    Wmean_WetMass= weighted.mean(WetMass_mean, Cover, na.rm=TRUE)
  ) %>% 
  
  mutate(
    Wmean_global_Lth= weighted.mean(Lth_mean_global, Cover, na.rm=TRUE),
    Wmean_global_LA= weighted.mean(LA_mean_global, Cover, na.rm=TRUE),
    #Wmean_global_Height= weighted.mean(Height_mean_global, Cover, na.rm=TRUE),
    Wmean_global_WetMass = weighted.mean(WetMass_mean_global, Cover, na.rm=TRUE)) %>%
  
  #group_by(functionalGroup, turfID, Site) %>%
  #mutate(Wmean_Height= weighted.mean(Height_mean, cover, na.rm=TRUE),
  #Wmean_global_Height = weighted.mean(Height_mean_global, cover, na.rm=TRUE)) %>%
  ungroup()

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
