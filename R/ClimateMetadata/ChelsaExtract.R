library(raster)

#load global site data
load("MetaAllCountries.Rdata")
meta <- MetaAllCountries

#load climate data
setwd("chelsa")
layer_names <- dir()

#extract climate data by site location
site_locations <- SpatialPoints(meta[,4:5])

bioclim <- data.frame(matrix(nrow=nrow(meta)))
for(i in 1:length(layer_names))
{
	layer_raster <- raster(layer_names[i])
	layer_num = unlist(strsplit(substr(layer_names[i], 1, nchar(layer_names[i])-4), split='_'))[3]
	print(paste(paste('Processing layer ', layer_num), paste(' from file ', layer_names[i])))
	bioclim[paste("bio", layer_num, sep="")] <- extract(layer_raster, site_locations)/10
}

write.csv(bioclim, file="bio.csv")
meta <- cbind(meta, bioclim[,-1])
write.csv(meta, file="country_metadata_merged_climate.csv")
