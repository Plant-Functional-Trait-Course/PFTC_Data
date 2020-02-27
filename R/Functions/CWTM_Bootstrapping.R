### BOOTSTRAPPING METHOD FOR CWM

# Impute Traits
ImputeTraits <- function(CountryList){
  
  ImputetTraits <- map(.x = CountryList, 
                       ~trait_impute(comm = .x$community,
                                     traits = .x$trait %>% select(-Gradient, -Year),
                                     scale_hierarchy = .x$trait_hierarchy,
                                     #scale_hierarchy = .x$trait_hierarchy[1:2],
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


BootstrappedCWM_Site <- function(ImputetTraits){

ImputetTraits2 <- map(.x = ImputetTraits, ~ mutate(., level = recode(level, "BlockID" = "Site", "PlotID" = "Site")))
HappyMoments_Site <- map2(ImputetTraits2, ImputetTraits, ~{
  attr(.x, "attrib") <- attr(.y, "attrib")
  .x}) %>%
    map_df(.x = ., ~ trait_np_bootstrap(imputed_traits = .x))

  return(HappyMoments_Site)

}


MomentRegression <- function(HappyMoments_Site){
  res <- HappyMoments_Site %>% 
    filter(Trait_trans %in% c("Plant_Height_cm_log", "Dry_Mass_g_log", "Leaf_Area_cm2_log", "Leaf_Thickness_Ave_mm", "LDMC", "SLA_cm2_g")) %>% 
    mutate(CG = paste(Country, Gradient, sep = "")) %>% 
    pivot_longer(cols = c(mean, variance, skewness, kurtosis), names_to = "Moment", values_to = "Value") %>% 
    mutate(Moment = factor(Moment, levels = c("mean", "variance", "skewness", "kurtosis"))) %>% 
    filter(!is.na(Value),
           !Value %in% c(-Inf, Inf, NaN)) %>% 
    group_by(Country, Trait_trans, Moment) %>%
    nest() %>%
    mutate(mod = map(data, ~ lmer(Value ~ MeanTemp + (1|Site), data = .x)), result = map(mod, tidy)) %>% 
    unnest(result) %>% 
      filter(term == "MeanTemp") %>% 
      mutate(CI.lower = estimate - 1.96 * std.error,
             CI.higher = estimate + 1.96 * std.error,
             OverlapZero = case_when(CI.lower > 0 & CI.higher > 0 ~ "no",
                                     CI.lower < 0 & CI.higher < 0 ~ "no",
                                     TRUE ~ "yes"))
  
  return(res)
}




# Summarize moments
# the SummariseBootMoments should be used fix this
SummariseHappyMoments <- function(HappyMoments){
  
  SummarizedMoments <- HappyMoments %>% 
    group_by(Country, Gradient, Site, BlockID, PlotID, Trait_trans) %>% 
    #group_by(Country, Gradient, Site, Trait_trans) %>% 
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

