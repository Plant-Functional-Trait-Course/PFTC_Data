# Calculate Diversity Indices
CalculateDiversityIndices <- function(CountryList){
  # add plot id for colorado
  cols <- c(PlotID = "1")
  
  diversity <- CountryList$community %>% 
    # Calculate species pool per gradient
    group_by(Country) %>% 
    mutate(spPool_regional = n()) %>% 
    add_column(!!!cols[!names(cols) %in% names(.)]) %>% 
    group_by(Country, Gradient, Site, BlockID, PlotID, spPool_regional) %>%  
    summarise(Richness = n(),
              Diversity = diversity(Cover), 
              N1 = exp(Diversity),
              Evenness = Diversity/log(Richness),
              sumAbundance = sum(Cover))
  
  return(diversity)
}


# Test relashionship of diversity and Elevation, MeanTemp and AnnPrec
# RegressionDiversityIndeces <- function(Diversity){
#   Diversity %>%
#     mutate(CG = paste(Country, Gradient, sep = "_")) %>%
#     mutate(PropAbundance = Richness / spPool_regional) %>%
#     pivot_longer(cols = c("PropAbundance", "Richness", "Diversity", "N1", "Evenness", "sumAbundance"), names_to = "Index", values_to = "Value") %>%
#     group_by(CG, Index) %>%
#     nest() %>% 
#     mutate(mod = map(data, lm(Value ~ Elevation, data = .x)))
#     map(., tidy, fit)
# 
#   return(Result)
# }
    