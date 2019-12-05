library(ncdf4)
load("MetaBioclimAllCountries_updated.RData")
MetaAllCountries <- meta
long <- round(MetaAllCountries$Longitude*2)+360
lat <- round(MetaAllCountries$Latitude*2)+180

#https://crudata.uea.ac.uk/cru/data/hrg/
vap <- nc_open("vap.nc")
print(vap)
vap_time <- ncvar_get(vap, varid="vap", start=c(1, 1, 1000), count=c(720, 360, 365))
vap_time_avg <- rowMeans(vap_time, na.rm=TRUE, dims=2)
vaps <- vap_time_avg[long, lat]

rm(vap, vap_time, vap_time_avg)

pet <- nc_open("pet.nc")
pet_time <- ncvar_get(pet, varid="pet", start=c(1, 1, 1000), count=c(720, 360, 365))
pet_time_avg <- rowMeans(pet_time, na.rm=TRUE, dims=2)
pets <- pet_time_avg[long, lat]

vapor_air_pressure <- c()
potential_evapotranspiration <- c()
for(i in seq(1, nrow(MetaAllCountries)))
{
	vapor_air_pressure <- c(vapor_air_pressure, vaps[i,i]) 
	potential_evapotranspiration <- c(potential_evapotranspiration, pets[i,i])
}

#Mean temperature of warmest quarter - BIOCLIM 10
TWQ = MetaAllCountries$bio10
SVP = 610.7*10^(7.5*TWQ/(237.3+TWQ))/1000 #kPA?

MetaAllCountries$VPD <- SVP - (vapor_air_pressure/10)	#kPA	
MetaAllCountries$PET <- potential_evapotranspiration	#mm/day
save(MetaAllCountries, file="MetaAllCountriesVPD_PET.RData")