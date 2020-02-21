# Calculate Diversity Indices
CalculateDiversityIndices <- function(CountryList){

  diversity <- CountryList$community %>% 
    # Calculate species pool per gradient
    group_by(Country) %>% 
    mutate(spPool_regional = n()) %>% 
    group_by(Country, Gradient, Site, BlockID, PlotID, spPool_regional) %>%  
    summarise(Richness = n(),
              Diversity = diversity(Cover), 
              #N1 = exp(Diversity),
              Evenness = Diversity/log(Richness)
              #sumAbundance = sum(Cover)
              )
  
  return(diversity)
}


# Test relashionship of diversity and Elevation, MeanTemp and AnnPrec
RegressionDiversityIndeces <- function(Diversity){
  Result <- Diversity %>%
    mutate(CG = paste(Country, Gradient, sep = "_")) %>%
    mutate(PropAbundance = Richness / spPool_regional) %>%
    pivot_longer(cols = c("Richness", "Evenness"), names_to = "Index", values_to = "Value") %>%
    group_by(CG, Index) %>%
    nest() %>%
    mutate(mod = map(data, ~lmer(Value ~ Elevation + (1|Site), data = .x)), result = map(mod, tidy)) %>%  # or glance
    unnest(result)

  return(Result)
}
    
