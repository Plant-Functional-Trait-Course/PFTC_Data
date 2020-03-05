# TRANSFORMING AND SCALING THE TRAITS
LogTransformation <- function(Country){
  
  #Making a function to log transform some traits in a new column of the previous trait dataset
  fun <- . %>% 
    mutate(Value_trans = ifelse(Trait %in% c("Plant_Height_cm", "Wet_Mass_g", "Dry_Mass_g", "Leaf_Area_cm2"), suppressWarnings(log(Value)), Value),
           Trait_trans = recode(Trait, "Plant_Height_cm" = "Plant_Height_cm_log", "Wet_Mass_g" = "Wet_Mass_g_log", "Dry_Mass_g" = "Dry_Mass_g_log", "Leaf_Area_cm2" = "Leaf_Area_cm2_log"))
  
  #If the dataset is a fata frame it will run the function on that, if it is a list it iwll run the function on the trait list
  
  if(inherits(Country, "data.frame")){
    Country <- Country %>% fun()
  } else {
    Country$trait <- Country$trait %>% fun()
  }
  
  # Country$trait <- Country$trait %>%
  #   group_by(Trait_trans) %>% 
  #   mutate(Value_trans = scale(Value_trans)[1])
  
  return(Country)
}

