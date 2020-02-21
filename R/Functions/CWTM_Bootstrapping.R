### BOOTSTRAPPING METHOD FOR CWM

# Impute Traits
ImputeTraits <- function(CountryList){
  
  ImputetTraits <- map(.x = CountryList, 
                       ~trait_impute(comm = .x$community,
                                     traits = .x$trait %>% select(-Gradient, -Year), 
                                     scale_hierarchy = .x$trait_hierarchy,
                                     taxon_col = "Taxon",
                                     trait_col = "Trait_trans",
                                     value_col = "Value_trans", 
                                     abundance_col = "Cover", 
                                     other_col = c("Gradient", "Year")))
  
  return(ImputetTraits)
  
}

# Bootstrap
BootstrappedCWM <- function(ImputetTraits){
    
  HappyMoments <-  map_df(.x = ImputetTraits, ~trait_np_bootstrap(imputed_traits = .))
  
  return(HappyMoments)
  
}




# Summarize moments
# the SummariseBootMoments should fix this
SummariseHappyMoments <- function(HappyMoments){
  
  SummarizedMoments <- HappyMoments %>% 
    group_by(Country, Gradient, Site, BlockID, PlotID, PlotID, Trait_trans) %>% 
    summarise(
      n = n(),
      Mean = mean(.data$mean),
      CIlow.mean = .data$Mean - sd(.data$mean),
      CIhigh.mean = .data$Mean + sd(.data$mean),
      
      Var = mean(.data$variance),
      CIlow.var = .data$Var - sd(.data$variance),
      CIhigh.var = .data$Var + sd(.data$variance),
      
      Skew = mean(.data$skewness),
      CIlow.skew = .data$Skew - sd(.data$skewness),
      CIhigh.skew = .data$Skew + sd(.data$skewness),
      
      Kurt = mean(.data$kurtosis),
      CIlow.kurt = .data$Kurt - sd(.data$kurtosis),
      CIhigh.Kurt = .data$Kurt + sd(.data$kurtosis)
    )
  
}

