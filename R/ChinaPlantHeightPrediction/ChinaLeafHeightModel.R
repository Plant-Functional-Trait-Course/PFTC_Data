library(reshape2)
library(ggplot2)
library(tidyr)

load("CWTraitMeans_Gradient.Rdata")
wide_traits <- spread(CWTraitMeans, Trait, CWTraitMean)
#ggplot(wide_traits, aes(x=log(Leaf_Area_cm2), y=log(Plant_Height_cm))) + geom_point(aes(color=Country))

yucky <- lm(Plant_Height_cm~Leaf_Area_cm2, data=wide_traits)
yucky_log <- lm(log(Plant_Height_cm)~log(Leaf_Area_cm2), data=wide_traits)

china <- which(wide_traits$Country == "CH")
china_areas <- wide_traits[china,]$Leaf_Area_cm2
china_data <- data.frame(Leaf_Area_cm2=china_areas)
china_heights <- predict(yucky_log, china_data) 
china_data$Plant_Height_cm <- china_heights
#exp_china_data <- exp(china_data)
#exp_china_data[24,]$Plant_Height_cm = NA

new_wide_traits <- wide_traits
new_wide_traits[china,c("Leaf_Area_cm2", "Plant_Height_cm")] <- china_data
#ggplot(new_wide_traits, aes(x=log(Leaf_Area_cm2), y=log(Plant_Height_cm))) + geom_point(aes(color=Country))
#ggplot(new_wide_traits, aes(x=Leaf_Area_cm2, y=Plant_Height_cm)) + geom_point(aes(color=Country))

new_long_traits <- gather(new_wide_traits, key="Trait", value="CWTraitMean", 29:45)
save(new_long_traits, file="CWTraitMeans_Gradient_ChinaHeights.RData")