#### A PRETTY DATA PROCESSING PLAN ####

#### MAKE COUNTRY LIST
CountryListAndTraitMeanDrakePlan <- drake_plan(
  
  # make a list with all data sets
  CountryList = list(China = Data_CH,
                     Peru = Data_PE,
                     Svalbard = Data_SV,
                     Norway = Data_NO,
                     Colorado = Data_CO) %>%
    #Log transforming height, mass and area
    map(LogTransformation)
) 


#### CALCULATE CWM USING BOOTSTRAPPING
# Using traitstrap package: imputation, bootstrapping, moments
DataProcessingDrakePlan <- drake_plan(

  # Create meta data
  MetaData = map_df(CountryList, ~ mutate(.x$meta, Gradient = as.character(Gradient))),
  
  # Diversity indices
  Diversity = map_df(CountryList, CalculateDiversityIndices) %>% 
    left_join(Climate, by = c("Country", "Gradient", "Site")),
  
  # Inmpute traits
  ImputetTraits = ImputeTraits(CountryList),
  
  # Bootstrapped CWM
  HappyMoments = BootstrappedCWM(ImputetTraits) %>% 
    left_join(Climate, by = c("Country", "Gradient", "Site")),
  
  # Bootstrapped CWM at Site Level
  HappyMoments_Site = BootstrappedCWM_Site(ImputetTraits) %>% 
    left_join(Climate, by = c("Country", "Gradient", "Site")),
  
  # Summarize Bootstrap Moments
  #BootstrapMoments = SummarizeBootMoments(HappyMoments) # should use this function!!!
  SummarisedMoments = SummariseHappyMoments(HappyMoments)  %>% 
    left_join(Climate, by = c("Country", "Gradient", "Site")),
  
  # Summarize Bootstrap Moments at Site level
  SummarisedMoments_Site = SummariseHappyMoments(HappyMoments_Site)  %>% 
    left_join(Climate, by = c("Country", "Gradient", "Site"))
)
