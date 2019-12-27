
library("raster")
library("viridis")
library("tidyverse")

#### Coord pop and site ####
GetCoords <- function(countrylist) {  
  dat <- countrylist$meta
  return(dat)
}

GetCoords(CountryList_WD)

CountryList_WD$Colorado$meta


### COORDINATES FIELD SITES
coords <- CountryList$China$meta %>% 
  rbind(CountryList$Colorado$meta) %>% 
  rbind(CountryList$Peru$meta) %>% 
  rbind(CountryList$Svalbard$meta) %>% 
  rbind(CountryList$Norway$meta) %>% 
  group_by(Country) %>% 
  summarise(Elevation = mean(Elevation),
            Latitude = mean(Latitude),
            Longitude = mean(Longitude))


#### Get Wolrdclim elevation data ####
elev <- getData('worldclim', var='alt', res=2.5)

#### WORLDMAP ####
elev.spdf <- as(elev, "SpatialPixelsDataFrame")
elev.df <- as_tibble(elev.spdf) %>% 
  # replace all values > 6000 with 6000 (everything above is not interesting)
  mutate(alt = ifelse(alt > 6000, 6000, alt)) %>% 
  filter(x > -130 & x < 130 & y > -25 & y < 85)

# plot world map
ggplot() + 
  geom_raster(data = elev.df, aes(x=x, y=y, fill = alt)) + 
  coord_equal() +
  labs(x="", y = "", fill = "Elevation\n") + 
  scale_y_continuous(limits = c(-25, 85)) +
  scale_fill_viridis(option = "inferno") +
  #scale_fill_gradient(low = "grey0", high = "grey100", limits=c(-416,4000)) + 
  geom_point(data = coords, aes(x = Longitude, y = Latitude), colour  = "#E69F00") + 
  theme_bw()
