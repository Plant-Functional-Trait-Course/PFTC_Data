# Climate data

library("GSODRdata")
str(CHELSA)

cnames <- paste0("CHELSA_temp_", 1:12, "_1979-2013")

clim_temp <- CHELSA[CHELSA$STNID %in% pnts$STNID,
                    paste(c("STNID", cnames))]
clim_temp_df <- data.frame(STNID = rep(clim_temp$STNID, 12),
                           MONTHC = as.vector(sapply(1:12, rep,
                                                     times = nrow(clim_temp))), 
                           CHELSA_TEMP = as.vector(unlist(clim_temp[, cnames])))

pnts$MONTHC <- as.numeric(paste(pnts$MONTH))
temp <- left_join(pnts@data, clim_temp_df, by = c("STNID", "MONTHC"))


### WORLDCLIM
load(file = "data/metaCH.Rdata")
load(file = "data/metaPE.Rdata")
load(file = "data/metaSV.Rdata")
load(file = "data/metaNO.Rdata")
load(file = "data/metaCO.Rdata")

MetaAllCountries <- metaCH %>% 
  bind_rows(metaPE, metaSV, metaNO, metaCO) %>% 
  mutate(Gradient = ifelse(is.na(Gradient), 1, Gradient)) %>% 
  mutate(Gradient = ifelse(Country == "NO" & Site %in% c("Vik", "Hog", "Lav"), 2, Gradient)) %>% 
  mutate(Gradient = ifelse(Country == "NO" & Site %in% c("Arh", "Ram", "Gud"), 3, Gradient)) %>% 
  mutate(Gradient = ifelse(Country == "NO" & Site %in% c("Ovs", "Ves", "Skj"), 4, Gradient)) %>% 
  mutate(Country = ifelse(is.na(Country), "CO", Country))

coords <- MetaAllCountries %>% 
  select(Longitude, Latitude)

r <- raster::getData("worldclim", var = "bio",res = 2.5)
points <- sp::SpatialPoints(coords, proj4string = r@crs)
values <- raster::extract(r,points)
bioclim <- cbind.data.frame(sp::coordinates(points),values)

MetaBioclim <- MetaAllCountries %>% 
  bind_cols(bioclim) %>% 
  select(-Longitude1, -Latitude1) %>% 
  mutate(bio1 = bio1/10,
         bio2 = bio2/10,
         bio3 = bio3/10,
         bio5 = bio5/10,
         bio6 = bio6/10,
         bio7 = bio7/10,
         bio8 = bio8/10,
         bio9 = bio9/10,
         bio10 = bio10/10,
         bio11 = bio11/10
         ) %>% 
  rename(MeanAnnTemp = bio1, MeanDiurnalRange = bio2, Isothermality = bio3, TempSeasonality = bio4, MaxTempWarmestMonth = bio5, MinTempColdestMonth = bio6, TempAnnRange = bio7, MeanTempWettestQuarter = bio8, MeanTempDriestQuarter = bio9, MeanTempWarmestQuarter = bio10, MeanTempColdestQuarter = bio11, AnnPrec = bio12, PrecWettestMonth = bio13, PrecDriestMonth = bio14, PrecSeasonality = bio15, PrecWettestQuarter = bio16, PrecDriestQuarter = bio17, PrecWarmestQuarter = bio18, PrecColdestQuarter = bio19)

save(MetaBioclim, file = "MetaBioclim.Rdata")  

metaCH <- MetaBioclim %>% 
  filter(Country == "CH")
save(metaCH, file = "data/metaCH")

metaPE <- MetaBioclim %>% 
  filter(Country == "PE")
save(metaPE, file = "data/metaPE")

metaSV <- MetaBioclim %>% 
  filter(Country == "SV")
save(metaSV, file = "data/metaSV")

metaNO <- MetaBioclim %>% 
  filter(Country == "NO")
save(metaNO, file = "data/metaNO")

metaCO <- MetaBioclim %>% 
  filter(Country == "CO") %>%
save(metaCO, file = "data/metaCO")

MetaBioclim %>% 
  mutate(CountGrad = paste(Country, Gradient, sep = "_")) %>% 
  ggplot(aes(x = Elevation, y = MeanTempWarmestQuarter, colour = CountGrad)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)
