CalculateDiversityIndices <- function(CountryList){
  diversity <- CountryList$community %>% 
    # Calculate species pool per gradient
    group_by(Country) %>% 
    mutate(spPool = n()) %>% 
    group_by(Country, Gradient, Site, PlotID, spPool) %>%  
    summarise(n = n(),
              Richness = n,
              Diversity = diversity(Cover), 
              N1 = exp(Diversity),
              Evenness = Diversity/log(Richness),
              sumAbundance = sum(Cover))
  
  return(diversity)
}
