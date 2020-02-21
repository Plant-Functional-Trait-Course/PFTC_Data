#### A PRETTY DATA PROCESSING PLAN ####

#### MAKE COUNTRY LIST
CountryListAndTraitMeanDrakePlan <- drake_plan(
  
  # make a list with all data sets
  CountryList = list(China = Data_CH,
                     Peru = Data_PE,
                     Svalbard = Data_SV,
                     Norway = Data_NO,
                     Colorado = Data_CO) %>%
    map(LogTransformation)  #Log transforming trait data (height, mass and area)
  
) 


#### CALCULATE CWM USING BOOTSTRAPPING
# Using traitstrap package: imputation, bootstrapping, moments
DataProcessingDrakePlan <- drake_plan(
  
  # Meta data
  MetaData = map_df(CountryList, ~ mutate(.x$meta, Gradient = as.character(Gradient))),
  
  # Diversity indices
  Diversity = map_df(CountryList, CalculateDiversityIndices) %>% 
    left_join(Climate, by = c("Country", "Gradient", "Site")),
  
  
  #Deciding what level you want to filter for 80% of the community, here I chose the global level
  #Community_Trait = Threshold_filter(Full_TraitMeans, TraitMean_global)
  
  # Inmpute traits
  ImputetTraits = ImputeTraits(CountryList),
  
  # Bootstrapped CWM
  HappyMoments = BootstrappedCWM(ImputetTraits) %>% 
    left_join(Climate, by = c("Country", "Gradient", "Site"))
  
  # Summarize Bootstrap Moments
  #BootstrapMoments = SummarizeBootMoments(BootstrapMoments_All),
  
  
  #BootstrapMoments_Bio = BootstrapMoments %>% 
  # left_join(metaBioclim, by = c("Country", "Site")),
  
  #GradientPlot = MakeFigure(BootstrapMoments),
  #GradientMeanPlot = MakeMeanFigure(CW_Means_Bootstrapped_Bio),
  #GradientVarPlot = MakeVarFigure(CW_Means_Bootstrapped_Bio),
  #GradientSkewPlot = MakeSkewFigure(CW_Means_Bootstrapped_Bio),
  #GradientKurtPlot = MakeKurtFigure(CW_Means_Bootstrapped_Bio)
  
)
