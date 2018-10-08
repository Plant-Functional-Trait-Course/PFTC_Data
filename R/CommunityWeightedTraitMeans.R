CommunityW_GlobalAndLocalMeans <- function(dat){
  dat2 <- dat %>% 
    group_by(Taxon) %>% 
    mutate(
      Lth_mean_global = mean(Leaf_Thickness_Ave_mm, na.rm = TRUE),
      Height_mean_global = mean(Plant_Height_cm, na.rm = TRUE),
      LA_mean_global = mean(Leaf_Area_cm2, na.rm = TRUE),
      WetMass_mean_global = mean(Wet_Mass_g, na.rm = TRUE)
    ) %>% 
    ungroup() %>%
    group_by(Site, Taxon) %>%
    mutate(
      Lth_mean = mean(Leaf_Thickness_Ave_mm, na.rm = TRUE),
      Height_mean = mean(Plant_Height_cm, na.rm = TRUE),
      LA_mean = mean(Leaf_Area_cm2, na.rm = TRUE),
      WetMass_mean = mean(Wet_Mass_g, na.rm = TRUE)
    ) %>%
    ungroup() %>% 
    
    #### Weighting the traits data by the community ####
    group_by(PlotID, Site) %>%
    mutate(
           Wmean_Lth= weighted.mean(Lth_mean, Cover, na.rm=TRUE),
           Wmean_LA= weighted.mean(LA_mean, Cover, na.rm=TRUE),
           Wmean_Height= weighted.mean(Height_mean, Cover, na.rm=TRUE),
           Wmean_WetMass= weighted.mean(WetMass_mean, Cover, na.rm=TRUE)
    ) %>% 
    
    mutate(
           Wmean_global_Lth= weighted.mean(Lth_mean_global, Cover, na.rm=TRUE),
           Wmean_global_LA= weighted.mean(LA_mean_global, Cover, na.rm=TRUE),
           Wmean_global_Height= weighted.mean(Height_mean_global, Cover, na.rm=TRUE),
           Wmean_global_WetMass = weighted.mean(WetMass_mean_global, Cover, na.rm=TRUE)) %>%
    
    #group_by(functionalGroup, turfID, Site) %>%
    #mutate(Wmean_Height= weighted.mean(Height_mean, cover, na.rm=TRUE),
           #Wmean_global_Height = weighted.mean(Height_mean_global, cover, na.rm=TRUE)) %>%
    ungroup()
  
  return(dat2)
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





