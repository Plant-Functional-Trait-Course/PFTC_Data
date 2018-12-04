iButt <- read.csv("Celevation_TEMPfile.txt")
fluxes <- read.csv("Cflux_SV_Gradient_2018.csv")

iButt$Datetime <- as.character(iButt$Datetime)
iButt$Temperature <- iButt$Temp + (iButt$Decimal/1000)
datestimes <- strsplit(iButt$Datetime, split = " ")

iButt$Date <- sapply(datestimes, FUN="[[", 1)
iButt$Time <- sapply(datestimes, FUN="[[", 2)

times <- strsplit(iButt$Time, split=":")
iButt$Hour <- as.numeric(sapply(times, FUN="[[", 1))
iButt$Minute <- as.numeric(sapply(times, FUN="[[", 2))
iButt$Second <- sapply(times, FUN="[[", 3)

#Use the first iButton measurement if fluxes are too early
base_temp = 13.098
flux_temp <- rep(0, nrow(fluxes))

fluxtime <- strsplit(as.character(fluxes$StartTime), split=":")
fluxtime[[27]] <- c(0, 0, 0)	#yea
hours <- as.numeric(sapply(fluxtime, FUN="[[", 1))
minutes <- as.numeric(sapply(fluxtime, FUN="[[", 2))

for(i in seq(1, nrow(fluxes)))
{
	if(hours[i] <= 11)
	{
		if((hours[i] == 11 && minutes[i] < 14) || hours[i] < 11)
		{
			flux_temp[i] = base_temp
			next
		}
		
	}
	ind <- which(iButt$Hour == hours[i] & iButt$Minute == minutes[i])
	if(length(ind) == 0)
		print(i)
	flux_temp[i] = mean(iButt[ind,]$Temperature)	
}
print(flux_temp)

##Write the final merged output file using temperature data and site metadata
#write.csv(flux_temp, file="Svalbard_Gradient_iButtonTemps.csv")
