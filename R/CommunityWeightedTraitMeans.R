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
    
  dat2 <- dat$community 
    
  if(!dat2$Country[1] %in% c("NO", "CO")) {
    dat2 <- dat2 %>% 
      # join plot level means
      left_join(meanTraits %>% select(-TraitMean_global, -TraitMean_site))
  }
    
  dat2 <- dat2 %>% 
    # join site level means
    left_join(meanTraits %>% select(-TraitMean_global, -TraitMean_plot, -BlockID, -PlotID) %>% distinct()) %>% 
    
    # join global means
    left_join(meanTraits %>% select(-TraitMean_plot, -TraitMean_site, -Site, -BlockID, -PlotID) %>% distinct())
    
  if(dat2$Country[1] %in% c("NO", "CO")) {
    dat2 <- dat2 %>% 
      mutate(TraitMean = coalesce(TraitMean_site, TraitMean_global))
  } else{
    dat2 <- dat2 %>% 
      mutate(TraitMean = coalesce(TraitMean_plot, TraitMean_site, TraitMean_global))
  }
  
  ### Calculate Community weighted means
  dat2 <- dat2 %>% 
    group_by(Trait, Site, PlotID) %>% 
    mutate(CWTraitMean = weighted.mean(TraitMean, Cover, na.rm=TRUE)) %>% 
    ungroup()
  
  return(dat2)
}


# Reduce to only CWMeans
CommunityW_Means <- function(TraitMeans_All){
  CWTraitMeans <- TraitMeans %>% 
    filter(!is.na(Trait)) %>% 
    select(-Taxon, -Cover, -TraitMean_plot, -TraitMean_site, -TraitMean_global, -TraitMean) %>% 
    distinct()
  return(CWTraitMeans)
}




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
