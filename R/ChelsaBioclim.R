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



# Svalbard
metaSV <- Climate %>% 
  filter(Country == "SV") %>% 
  select(Gradient, Site, Elevation) %>% 
  arrange(Gradient, Elevation)

coords <- data.frame(Longitude = c(15.68663, 15.67255, 15.91093, 15.39917, 15.36402, 15.37586),
                     Latitude = c(78.21716, 78.19637, 78.16775, 78.24738, 78.23936, 78.20929))

# Elev
elev <- raster::getData('worldclim', var = "alt", res = 0.5, lon = 15, lat = 78)

# Mean Ann Temp
rastlist <- list.files(path = "/Volumes/fis003/Projects/PFTC/ClimateData/", pattern = ".tif", 
                       all.files = TRUE, full.names = TRUE)

allrasters <- map(rastlist[1], raster::raster)

TempElev <- map(.x = allrasters, .f = raster::extract, coords) %>% 
  do.call("rbind", .) %>% 
  t() %>% as_tibble() %>% 
  rename("MeanTemp" = "V1") %>% 
  mutate(MeanTemp = MeanTemp /10) %>% 
  bind_cols(Elev = raster::extract(elev, coords),
            Gradient = c(rep("C", 3), rep("B", 3)))

# Regression and predict new Temp
mod <- lm(MeanTemp ~ Elev, data = TempElev)
new <- data.frame(Elev = metaSV$Elevation)
metaSV$MeanTempSV <- predict(mod, new)
 
dd <- Climate %>% 
  left_join(metaSV, by = c("Gradient", "Site", "Elevation")) %>% 
  mutate(MeanTemp = if_else(Country == "SV", MeanTempSV, MeanTemp)) %>% 
  select(-MeanTempSV)

write_csv(dd, path = "data/MetaClimate.csv", col_names = TRUE)




