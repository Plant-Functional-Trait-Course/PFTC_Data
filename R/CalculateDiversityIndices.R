CalculateDiversityIndices <- function(dat){
  dat2 <- dat %>% 
    group_by(Year, Site, PlotID) %>%  
    summarise(n = n(),
              Richness = n 
              #Diversity = diversity(Cover), 
              #N1 = exp(Diversity),
              #Evenness = Diversity/log(Richness),
              sumAbundance = sum(Cover)
    )
  return(dat2)
}
