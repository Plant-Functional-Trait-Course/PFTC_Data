#library(raster)
#library(sp)
library(tidyverse)

#WordlClim <- getData("worldclim",var="bio",res=10)

#r <- WordlClim[[c(10,12)]]
#names(r) <- c("Temp","Prec")

MakeCoordinates <- function(countrylist){
coor <- countrylist$meta %>% 
  select(Site, Latitude, Longitude) %>% 
  unique()
return(coor)
}



#coords <- coor %>% select(Latitude, Longitude)

#points <- SpatialPoints(coords, proj4string = r@crs)

#values <- extract(r,points)

#df <- cbind.data.frame(coordinates(points),values)
#plot(r[[1]])
#plot(points,add=T)
