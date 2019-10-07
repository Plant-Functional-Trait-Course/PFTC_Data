MakeFigure <- function(BootstrapMoments){
  myplot <- BootstrapMoments %>% 
    filter(!is.na(Trait), !Trait %in% c("P_Co_Var", "P_Std_Dev", "NC_ratio", "NP_ratio")) %>% 
    ungroup() %>% 
    mutate(Trait = factor(Trait, level = c("Wet_Mass_g", "Dry_Mass_g", "Leaf_Area_cm2", "Leaf_Thickness_Ave_mm", "LDMC", "SLA_cm2_g", "Plant_Height_cm", "C_percent", "N_percent", "CN_ratio", "P_AVG", "dN15_percent", "dC13_percent"))) %>% 
    mutate(CG = paste(Country, Gradient, sep = "")) %>% 
    ggplot(aes(x = Site, y = meanMean, colour = CG)) +
    geom_point() +
    geom_smooth(method = "lm", se = FALSE) +
    labs(x = "Mean annual Temperature °C", y = "Community weighted trait mean") +
    facet_grid(Country ~ Trait, scales = "free_y")
  return(myplot)
}

MakeMeanFigure <- function(BootstrapMoments_Bio){
  myplot <- BootstrapMoments_Bio %>% 
    filter(Trait %in% c("Wet_Mass_g_log", "Dry_Mass_g_log", "Leaf_Area_cm2_log", "Leaf_Thickness_Ave_mm", "LDMC", "SLA_cm2_g", "Plant_Height_cm_log", "C_percent", "N_percent", "CN_ratio", "P_AVG", "dN15_percent", "dC13_percent")) %>% 
    mutate(Trait = factor(Trait, level = c("Wet_Mass_g_log", "Dry_Mass_g_log", "Leaf_Area_cm2_log", "Leaf_Thickness_Ave_mm", "LDMC", "SLA_cm2_g", "Plant_Height_cm_log", "C_percent", "N_percent", "CN_ratio", "P_AVG", "dN15_percent", "dC13_percent"))) %>% 
  mutate(CG = paste(Country, Gradient, sep = "")) %>% 
  ggplot(aes(x = MeanTempWarmQuart, y = meanMean, colour = CG)) +
  geom_point(alpha = 0.3) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(x = "Mean temperature of warmest quarter (°C)", y = "Community weighted trait mean") +
  facet_wrap(~ Trait, scales = "free_y")
  return(myplot)
}

MakeVarFigure <- function(CW_Means_Bootstrapped_Bio){
  myplot <- CW_Means_Bootstrapped_Bio %>% 
    filter(Trait %in% c("Wet_Mass_g", "Dry_Mass_g", "Leaf_Area_cm2", "Leaf_Thickness_Ave_mm", "LDMC", "SLA_cm2_g", "Plant_Height_cm", "C_percent", "N_percent", "CN_ratio", "P_AVG", "dN15_percent", "dC13_percent")) %>% 
    mutate(Trait = factor(Trait, level = c("Wet_Mass_g", "Dry_Mass_g", "Leaf_Area_cm2", "Leaf_Thickness_Ave_mm", "LDMC", "SLA_cm2_g", "Plant_Height_cm", "C_percent", "N_percent", "CN_ratio", "P_AVG", "dN15_percent", "dC13_percent"))) %>% 
    mutate(CG = paste(Country, Gradient, sep = "")) %>% 
    ggplot(aes(x = AnnMeanTemp, y = var, colour = CG)) +
    geom_point() +
    geom_smooth(method = "lm", se = FALSE) +
    labs(x = "Mean annual Temperature °C", y = "Community weighted trait variation") +
    facet_wrap(~ Trait, scales = "free_y")
  return(myplot)
}

MakeSkewFigure <- function(CW_Means_Bootstrapped_Bio){
  myplot <- CW_Means_Bootstrapped_Bio %>% 
    filter(Trait %in% c("Wet_Mass_g", "Dry_Mass_g", "Leaf_Area_cm2", "Leaf_Thickness_Ave_mm", "LDMC", "SLA_cm2_g", "Plant_Height_cm", "C_percent", "N_percent", "CN_ratio", "P_AVG", "dN15_percent", "dC13_percent")) %>% 
    mutate(Trait = factor(Trait, level = c("Wet_Mass_g", "Dry_Mass_g", "Leaf_Area_cm2", "Leaf_Thickness_Ave_mm", "LDMC", "SLA_cm2_g", "Plant_Height_cm", "C_percent", "N_percent", "CN_ratio", "P_AVG", "dN15_percent", "dC13_percent"))) %>% 
    mutate(CG = paste(Country, Gradient, sep = "")) %>% 
    ggplot(aes(x = AnnMeanTemp, y = skew, colour = CG)) +
    geom_point() +
    geom_smooth(method = "lm", se = FALSE) +
    labs(x = "Mean annual Temperature °C", y = "Community weighted trait skewness") +
    facet_wrap(~ Trait, scales = "free_y")
  return(myplot)
}


MakeKurtFigure <- function(CW_Means_Bootstrapped_Bio){
  myplot <- CW_Means_Bootstrapped_Bio %>% 
    filter(Trait %in% c("Wet_Mass_g", "Dry_Mass_g", "Leaf_Area_cm2", "Leaf_Thickness_Ave_mm", "LDMC", "SLA_cm2_g", "Plant_Height_cm", "C_percent", "N_percent", "CN_ratio", "P_AVG", "dN15_percent", "dC13_percent")) %>% 
    mutate(Trait = factor(Trait, level = c("Wet_Mass_g", "Dry_Mass_g", "Leaf_Area_cm2", "Leaf_Thickness_Ave_mm", "LDMC", "SLA_cm2_g", "Plant_Height_cm", "C_percent", "N_percent", "CN_ratio", "P_AVG", "dN15_percent", "dC13_percent"))) %>% 
    mutate(CG = paste(Country, Gradient, sep = "")) %>% 
    ggplot(aes(x = AnnMeanTemp, y = kurt, colour = CG)) +
    geom_point() +
    geom_smooth(method = "lm", se = FALSE) +
    labs(x = "Mean annual Temperature °C", y = "Community weighted trait kurtosis") +
    facet_wrap(~ Trait, scales = "free_y")
  return(myplot)
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
