#### MAKE MAP ####

MakePrettyMap <- function(CountryList){
  
  # Get coordinates
  coords <- map_df(CountryList, ~ mutate(.x$meta, Gradient = as.character(Gradient))) %>% 
    group_by(Country) %>% 
    summarise(Elevation = mean(Elevation),
              Latitude = mean(Latitude),
              Longitude = mean(Longitude))

  # Get Wolrdclim elevation data
  elev <- raster::getData('worldclim', var='alt', res=2.5)
  
  #### WORLDMAP ####
  elev.spdf <- as(elev, "SpatialPixelsDataFrame")
  elev.df <- as_tibble(elev.spdf) %>% 
    # replace all values > 6000 with 6000 (everything above is not interesting)
    mutate(alt = ifelse(alt > 6000, 6000, alt)) %>% 
    filter(x > -130 & x < 130 & y > -25 & y < 85)
  
  # plot world map
  PFTCMap <- ggplot() + 
    geom_raster(data = elev.df, aes(x=x, y=y, fill = alt)) + 
    coord_equal() +
    labs(x="", y = "", fill = "") + 
    scale_y_continuous(limits = c(-25, 85), expand = c(0, 0)) +
    scale_x_continuous(expand = c(0, 0)) +
    scale_fill_viridis_c(breaks = seq(0, 6000, 2000), labels = paste(seq(0, 6000, 2000), "m")) +
    geom_point(data = coords, aes(x = Longitude, y = Latitude), colour  = "#D55E00", size = 3) +
    theme_bw() +
    theme(legend.position = c(0.005, 0.005), legend.justification = c(0, 0), legend.title = element_blank())
  
  return(PFTCMap)
}
