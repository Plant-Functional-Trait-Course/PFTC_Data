### BOOTSTRAPPING METHOD FOR CWM

# Impute Traits
BootstrappedCWM <- function(CountryList){
  
  HappyMoments <- map(.x = CountryList, 
                      ~trait_impute(comm = .x$community,
                                    traits = .x$trait %>% select(-Gradient, -Year), 
                                    scale_hierarchy = .x$trait_hierarchy,
                                    taxon_col = "Taxon",
                                    trait_col = "Trait_trans",
                                    value_col = "Value_trans", 
                                    abundance_col = "Cover", 
                                    other_col = c("Gradient", "Year"))) %>% 
    map_df(.x = ., ~trait_np_bootstrap(imputed_traits = .))
  
  return(HappyMoments)
  
}

