bio1_7 <- read.csv("bio1_7.csv")
bio8_13 <- read.csv("bio8_13.csv")
bio14_19 <- read.csv("bio14_19.csv")

load("MetaAllCountries.Rdata")
meta <- MetaAllCountries

meta <- cbind(meta, bio1_7)
meta <- cbind(meta, bio8_13)
meta <- cbind(meta, bio14_19)
meta <- meta[,-c(7, 15, 22)]
print(meta)
write.csv(meta, file="country_metadata_merged_climate.csv")