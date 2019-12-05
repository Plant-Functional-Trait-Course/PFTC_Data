setwd("fluxes")
#get flux data for dates/times
flux_data <- dir()
fluxes <- read.csv(flux_data[1])
fluxes <- fluxes[,c("Plot", "Date", "Time")]
#for(i in seq(2, length(flux_data)))
#{
#	new_fluxes <- read.csv(flux_data[i])
#	new_fluxes <- new_fluxes[,c("Site", "Plot", "Date")]
#	fluxes <- rbind(fluxes, new_fluxes)
#}

fluxdates <- strsplit(as.character(fluxes$Date), split="-")
days <- as.numeric(sapply(fluxdates, FUN="[[", 3))
months <- as.numeric(sapply(fluxdates, FUN="[[", 2))
years <- sapply(fluxdates, FUN="[[", 1)

hours <- 

#Assemble solar radiation data
setwd("../weather")
weather_data <- dir()
weather_data <- weather_data[-length(weather_data)]
par <- read.csv(weather_data[1], skip=2)
par <- par[,c("Measurement.Time", "Solar.W.m.")]
for(i in seq(2, length(weather_data)))
{
	new_par <- read.csv(weather_data[i], skip=2)
	new_par <- new_par[,c("Measurement.Time", "Solar.W.m.")]
	par <- rbind(par, new_par)
}
colnames(par) <- c("datetime", "radiation")
par$datetime <- as.character(par$datetime)

datestimes <- strsplit(par$datetime, split = " ")
par$date <- sapply(datestimes, FUN="[[", 1)
par$time <- sapply(datestimes, FUN="[[", 2)
par$daynight <- sapply(datestimes, FUN="[[", 3)

pardays <- strsplit(par$date, split="/")
par$month <- sapply(pardays, FUN="[[", 1)
par$day <- sapply(pardays, FUN="[[", 2)
par$year <- sapply(pardays, FUN="[[", 3)

par$hour <- sapply(strsplit(par$time, split=":"), FUN="[[", 1)
morning <- ((par$hour == "10" | par$hour == "11") & par$daynight == 'AM') | (par$hour == "12" & par$daynight == 'PM') 

morning_par <- par[which(morning),]
flux_par <- rep(0, nrow(fluxes))

for(i in seq(1, nrow(fluxes)))
{
	ind <- which(morning_par$day == days[i] & morning_par$month == months[i] & morning_par$year == years[i])
	if(length(ind) == 0)
	{
		flux_par[i] = -1
	}
	else
	{
		#flux_par[i] = mean(morning_par[ind,]$radiation)
		flux_par[i] = -1
	}	
}

missing_vals <- which(flux_par == -1)
#missing_dates <- c("2013-05-17", "2013-05-18", "2013-05-24", "2013-05-23", "2013-05-27", "2013-05-28",
#			"2013-06-12", "2013-06-11", "2013-06-21", "2013-07-01", "2013-07-08", "2013-07-22",
#			"2013-07-23", "2013-07-31", "2013-07-30", "2014-06-03", "2014-06-09", "2014-06-10",
#			"2014-06-16", "2014-06-17", "2014-06-23", "2014-06-24", "2014-06-30", "2014-07-01",
#			"2014-07-07", "2014-07-08", "2014-07-14", "2014-07-15", "2014-07-17", "2014-07-21",
#			"2014-07-23", "2014-08-07", "2015-05-20", "2015-05-27", "2015-05-29", "2015-06-22",
#			"2015-06-23", "2015-06-25", "2015-06-26", "2015-06-29", "2015-07-01", "2015-07-03",
#			"2015-07-15", "2015-07-16", "2015-07-22", "2015-07-23", "2015-08-05", "2015-08-06")
#Get missing weather data from local station
missing_dates <- c("2014-07-07", "2014-07-15", "2014-07-17", "2014-07-21", "2014-07-23", "2014-08-07",
			"2015-06-23", "2015-06-26", "2015-07-03", "2015-07-16", "2015-07-23", "2015-08-06")
library(weatherData)
site <- "KCOMTCRE2"
setwd(paste("../",site,sep=""))
if(length(dir()) == 0)	#Download weather data
{
	gold_link <- getDetailedWeather(site, missing_dates[1], station_type="id", opt_custom_columns=TRUE, custom_columns=c(14))
	colnames(gold_link) <- c("datetime", "radiation")
	for(i in seq(2, length(missing_dates)))
	{
		new_rad <- getDetailedWeather(site, missing_dates[i], station_type="id", opt_custom_columns=TRUE, custom_columns=c(14))
		colnames(new_rad) <- c("datetime", "radiation")
		print(paste("New data for date: ", missing_dates[i]))
		write.csv(new_rad, file = paste(missing_dates[i], ".csv"))
		#gold_link <- rbind(gold_link, new_rad)
	}
}
station_data <- dir()
gold_link <- read.csv(station_data[1])
for(i in seq(2, length(station_data)))
{
	new_rad <- read.csv(station_data[i])
	gold_link <- rbind(gold_link, new_rad)
}

station_dates_times <- strsplit(as.character(gold_link$datetime), split = " ")
gold_link$date <- sapply(station_dates_times, FUN="[[", 1)
gold_link$time <- sapply(station_dates_times, FUN="[[", 2)

stationdays <- strsplit(gold_link$date, split="-")
gold_link$month <- as.numeric(sapply(stationdays, FUN="[[", 2))
gold_link$day <- as.numeric(sapply(stationdays, FUN="[[", 3))
gold_link$year <- sapply(stationdays, FUN="[[", 1)

gold_link$hour <- sapply(strsplit(gold_link$time, split=":"), FUN="[[", 1)
gold_link <- gold_link[which(gold_link$hour == "10" | gold_link$hour == "11" | gold_link$hour == "12"),]

for(i in seq(1, length(missing_vals)))
{
	i_m <- missing_vals[i]
	ind <- which(gold_link$day == days[i_m] & gold_link$month == months[i_m] & gold_link$year == years[i_m])
	if(length(ind) == 0)
	{
		print(days[i_m])
		print(months[i_m])
		print(years[i_m])
		flux_par[i_m] = 0
	}
	else
		flux_par[i_m] = mean(gold_link[ind,]$radiation)
}

#Super rough conversion from W/m^2 to PPFD for solar spectra, coefficient umol/J^-1
#https://www.apogeeinstruments.com/conversion-ppfd-to-watts/
flux_par <- flux_par * 3.05

setwd("../..")
merged <- data.frame(Plot=fluxes$Plot, Date=fluxes$Date, PAR=flux_par)
write.csv(merged, file="ColoradoPAR.csv")