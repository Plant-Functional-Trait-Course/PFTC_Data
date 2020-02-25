######################################
#### DOWNLOAD CHELSA BIOCLIM DATA ####
######################################


# Coordinates
meta <- MetaData %>% dplyr::select(Longitude, Latitude)

# make list of files
rastlist <- list.files(path = "ClimateData/", pattern = ".tif", 
                       all.files = TRUE, full.names = TRUE)

# Raster
allrasters <- map(rastlist, raster::raster)

# Extract
climate <- map(.x = allrasters, .f = raster::extract, meta) %>% 
  do.call("rbind", .) %>% 
  t() %>% as_tibble() %>% 
  rename("MeanTemp" = "V1", 
         "MeanDiurnalRange" = "V2", 
         "Isothermality" = "V3", 
         "TempSeasonal" = "V4", 
         "MaxTempWarmMonth" = "V5", 
         "MinTempColdMonth" = "V6", 
         "TmepAnnRange" = "V7", 
         "MeanTempWetQuart" = "V8", 
         "MeanTempDryQuart" = "V9", 
         "MeanTempWarmQuart" = "V10", 
         "MeanTempColdQuart" = "V11", 
         "AnnPrec" = "V12", 
         "PrecWetMonth" = "V13", 
         "PrecDryMonth" = "V14", 
         "PrecSeasonal" = "V15", 
         "PrecWetQuart" = "V16", 
         "PrecDryQuart" = "V17", 
         "PrecWarmQuart" = "V18", 
         "PrecColdQuart" = "V19") %>% 
  mutate(MeanTemp = MeanTemp/10,
         MaxTempWarmMonth = MaxTempWarmMonth/10,
         MinTempColdMonth = MinTempColdMonth/10,
         MeanTempWetQuart = MeanTempWetQuart/10,
         MeanTempDryQuart = MeanTempDryQuart/10,
         MeanTempWarmQuart = MeanTempWarmQuart/10,
         MeanTempColdQuart = MeanTempColdQuart/10)

MetaClimate <- MetaData %>% 
  bind_cols(climate)

write_csv(x = MetaClimate, path = "data/MetaClimate.csv", col_names = TRUE)
