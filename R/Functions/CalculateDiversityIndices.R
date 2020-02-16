CalculateDiversityIndices <- function(CountryList){
  # add plot id for colorado
  cols <- c(PlotID = "1")
  
  diversity <- CountryList$community %>% 
    # Calculate species pool per gradient
    group_by(Country) %>% 
    mutate(spPool_global = n()) %>% 
    group_by(Country, Site) %>% 
    mutate(spPool_site = n()) %>% 
    add_column(!!!cols[!names(cols) %in% names(.)]) %>% 
    group_by(Country, Gradient, Site, BlockID, PlotID, spPool_global, spPool_site) %>%  
    summarise(n = n(),
              Richness = n,
              Diversity = diversity(Cover), 
              N1 = exp(Diversity),
              Evenness = Diversity/log(Richness),
              sumAbundance = sum(Cover))
  
  return(diversity)
}

        