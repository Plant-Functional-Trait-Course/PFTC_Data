#library(raster)
#library(sp)
library("tidyverse")

##Doesn't work, and I don't know why. metadata for Colorado is in dataframe not a tibble

MakeCoordinates <- function(countrylist){
  t <- as_tibble(countrylist$meta)
  
coor <- t %>% 
  select(Site, Latitude, Longitude) %>% 
  unique()
return(coor)
}

#Just added a new dataset

LatsLongs <- read_delim(file = "data/LatsLongs.csv", col_names = TRUE, delim= ";") 

coord <- LatsLongs %>% 
  dplyr::select(Latitude, Longitude)


library("raster")
WordlClim <- getData("worldclim",var="bio",res=10)

Clim <- WordlClim[[c(1,10, 12)]]
names(Clim) <- c("Temp", "Summer Temp", "Prec")

values <- extract(Clim, coord)



#plot(clim$bio10/10, main="Mean Temperature of Warmest Quarter")

#coords <- coor %>% select(Latitude, Longitude)

#points <- SpatialPoints(coords, proj4string = r@crs)

#values <- extract(r,points)

#df <- cbind.data.frame(coordinates(points),values)
#plot(r[[1]])
#plot(points,add=T)
