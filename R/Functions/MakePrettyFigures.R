### Coverage Figure

MakeCoverageFigure <- function(ImputetTraits){
  
  CoverageFigure <- map(.x = ImputetTraits, ~ filter(., Trait == "Leaf_Area_cm2")) %>% 
    map_df(fortify, .id = "FullCountry") %>% 
    filter(Country %in% c("NO", "SV")) %>% 
    mutate(level = factor(level, levels = c("Country", "Site", "BlockID", "PlotID"))) %>% 
    ggplot(aes(x = .id, y = s, fill = level)) +
    geom_col() +
    scale_y_continuous(expand = c(0, 0)) +
    scale_fill_viridis_d() +
    labs(x = "", y = "Proportion of cover", fill = "Data source") +
    facet_wrap(~FullCountry, scales = "free_x") +
    theme(axis.text.x = element_blank())
  
  return(CoverageFigure)
}


MakeCoverageSiteLevel <- function(ImputetTraits){
  
  CoverageSiteLevel <- map(.x = ImputetTraits, ~ filter(., Trait_trans == "Leaf_Area_cm2_log")) %>% 
    map_df(fortify, .id = "FullCountry") %>% 
    # recode block and plot ID to site level
    mutate(level = recode(level, "BlockID" = "Site", "PlotID" = "Site")) %>% 
    mutate(level = factor(level, levels = c("Country", "Site"))) %>% 
    ggplot(aes(x = .id, y = s, fill = level)) +
    geom_col() +
    scale_y_continuous(expand = c(0, 0)) +
    scale_fill_viridis_d() +
    labs(x = "", y = "Proportion of cover", fill = "Data source") +
    facet_wrap(~FullCountry, scales = "free_x") +
    theme(axis.text.x = element_blank())
  
  return(CoverageSiteLevel)
}


# Plot summarised moments - mean
MakeMeanFigure <- function(SummarisedMoments_Site){

  GradientMeanPlot <- SummarisedMoments_Site %>% 
    filter(Trait_trans %in% c("Plant_Height_cm_log")) %>% 
    select(-c(MeanDiurnalRange:PrecColdQuart)) %>% 
    pivot_longer(cols = c("Mean", "Var", "Skew", "Kurt"), names_to = "Moment", values_to = "Value") %>% 
    mutate(Moment = recode(Moment, "Mean" = "mean", "Var" = "variance", "Skew" = "skewness", "Kurt" = "kurtosis"),
           Moment = factor(Moment, levels = c("mean", "variance", "skewness", "kurtosis"))) %>% 
    mutate(CG = paste(Country, Gradient, sep = ""),
           FullCountry = recode(Country, "CH" = "China", "CO" = "Colorado", "NO" = "Norway", "PE" = "Peru", "SV" = "Svalbard")) %>% 
    mutate(GradNum = case_when(Gradient %in% c("1", "C") ~ 1,
                               Gradient %in% c("2") ~ 2,
                               Gradient %in% c("3") ~ 3,
                               Gradient %in% c("4") ~ 4)) %>% 
    left_join(RegMoment %>% select(-data, -mod), by = c("Country", "Trait_trans", "Moment")) %>% 
    filter(Moment == "mean") %>% 
    ggplot(aes(x = MeanTemp, y = Value, color = FullCountry, linetype = OverlapZero)) +
    geom_point() +
    scale_colour_viridis_d(option = "plasma", end = 0.9) +
    labs(x = "Mean annual temperature in °C", y = "", title = "log(Plant height in cm)", colour = "") +
    geom_smooth(method = "lm", se = FALSE) +
    #facet_wrap( ~ Moment, scales = "free_y") +
    guides(linetype = FALSE) +
    theme_bw() +
    theme(legend.position = "top",
          axis.title = element_text(size = 15),
          axis.text = element_text(size = 12),
          legend.text = element_text(size = 12))
}



# Plot summarized moments - variance
MakeMomentFigure <- function(){
  
  HigherMomentPlot <- SummarisedMoments_Site %>% 
    filter(Trait_trans %in% c("SLA_cm2_g")) %>% 
    select(-c(MeanDiurnalRange:PrecColdQuart)) %>% 
    pivot_longer(cols = c("Mean", "Var", "Skew", "Kurt"), names_to = "Moment", values_to = "Value") %>% 
    mutate(Moment = recode(Moment, "Mean" = "mean", "Var" = "variance", "Skew" = "skewness", "Kurt" = "kurtosis"),
           Moment = factor(Moment, levels = c("mean", "variance", "skewness", "kurtosis"))) %>% 
    mutate(CG = paste(Country, Gradient, sep = ""),
           FullCountry = recode(Country, "CH" = "China", "CO" = "Colorado", "NO" = "Norway", "PE" = "Peru", "SV" = "Svalbard")) %>% 
    mutate(GradNum = case_when(Gradient %in% c("1", "C") ~ 1,
                               Gradient %in% c("2") ~ 2,
                               Gradient %in% c("3") ~ 3,
                               Gradient %in% c("4") ~ 4)) %>% 
    left_join(RegMoment, by = c("Country", "Trait_trans", "Moment")) %>% 
    filter(Moment != "mean") %>% 
    ggplot(aes(x = MeanTemp, y = Value, color = FullCountry, linetype = OverlapZero)) +
    geom_point() +
    scale_colour_viridis_d(option = "plasma", end = 0.9) +
    labs(x = "Mean annual temperature in °C", y = "", title = "log(Plant height in cm)", colour = "") +
    geom_smooth(method = "lm", se = FALSE) +
    facet_wrap( ~ Moment, scales = "free_y") +
    guides(linetype = FALSE) +
    theme_bw() +
    theme(legend.position = "top",
          axis.title = element_text(size = 15),
          axis.text = element_text(size = 12),
          legend.text = element_text(size = 12))
  
  return(HigherMomentPlot)
}


# Smiley plot - kurtosis and skewness
SmileyPlot <- function(){
  SummarisedMoments %>% 
    filter(Trait_trans %in% c("Plant_Height_cm_log", "Leaf_Thickness_Ave_mm", "LDMC", "SLA_cm2_g")) %>% 
    mutate(Trait_trans = factor(Trait_trans, level = c("Plant_Height_cm_log","Leaf_Thickness_Ave_mm", "LDMC", "SLA_cm2_g"))) %>% 
    mutate(FullCountry = recode(Country, "CH" = "China", "CO" = "Colorado", "NO" = "Norway", "PE" = "Peru", "SV" = "Svalbard")) %>% 
    ggplot(aes(x = Skew, y = Kurt, color = FullCountry)) +
    geom_point(alpha = 0.4, size = 3) +
    scale_colour_viridis_d(option = "plasma", end = 0.9) +
    facet_wrap( ~ Trait_trans) +
    theme_bw() +
    theme(legend.position = "top")
}



DiverstiyPlot <- function(Diversity){
  DivPlot <- Diversity %>% 
    filter(Gradient != "B") %>% 
    mutate(Biogeo = case_when(Country %in% c("PE") ~ "Tropic",
                              Country %in% c("CO", "NO", "CH") ~ "Temperate",
                              TRUE ~ "Arctic")) %>% 
    ggplot(aes(x = Elevation, y = Richness, colour = Country)) +
    geom_point() +
    geom_point(aes(y = Evenness * 20)) +
    scale_colour_viridis_d(option = "plasma", end = 0.9) +
    geom_smooth(method = "lm") +
    geom_smooth(aes(x = Elevation, y = Evenness * 20), method = "lm", linetype = "dashed") +
    scale_y_continuous(sec.axis = sec_axis(~./20)) +
    labs(y = "", title = "Richenss and evenness") +
    facet_wrap(~ Biogeo, scales = "free_x") +
    theme_bw() +
    theme(panel.grid = element_blank())
  
  return(DivPlot)
}




MakeLocal_Global_plot_SLA <- function(CW_Traits, MetaBioclim){
  myplot <- CW_Traits %>% 
    filter(Trait_trans == "SLA_cm2_g") %>% 
    mutate(TraitLevel = factor(TraitLevel, level = c("TraitMean_global", "TraitMean_regional", "TraitMean_site", "TraitMean_plot"))) %>% 
    ggplot(aes(x = MeanTempWarmestQuarter, y = CWTraitMean, fill = TraitLevel)) +
    geom_boxplot() +
    labs(x = "Level of trait collection", y = "Community weighted mean SLA") +
    facet_wrap(~ Country, scales = "free_y")
  return(myplot)
}
