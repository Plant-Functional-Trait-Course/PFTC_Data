#**********************************************************************
library(drake)

r_make(source = "R/AudsDrakePlan.R")
loadd()
failed()

#view dependency graph
r_vis_drake_graph(source = "R/AudsDrakePlan.R", targets_only = TRUE)

#**********************************************************************


Bmoments <- BootstrapMoments_All %>% 
  group_by(Country, Year, Site, Gradient, BlockID, PlotID, Trait_trans) %>% 
  summarise(n = n(),
            meanMean = mean(Mean), CIlow.Mean = meanMean - sd(Mean), CIhigh.Mean = meanMean + sd(Mean),
            meanVar = mean(Variance), CIlow.Var = meanVar - sd(Variance), CIhigh.Var = meanVar + sd(Variance),
            meanSkew = mean(Skewness), CIlow.Skew = meanSkew - sd(Skewness), CIhigh.Skew = meanSkew + sd(Skewness),
            meanKurt = mean(Kurtosis), CIlow.Kurt = meanKurt - sd(Kurtosis), CIhigh.Kurt = meanKurt + sd(Kurtosis))


Bmoments %>%
  left_join(meta) %>% 
  filter(!is.na(Trait_trans), Trait_trans %in% c("Wet_Mass_g", "Dry_Mass_g", "Leaf_Area_cm2", "Leaf_Thickness_Ave_mm", "LDMC", "SLA_cm2_g", "Plant_Height_cm", "C_percent", "N_percent")) %>% 
  ungroup() %>% 
  mutate(Trait_trans = factor(Trait_trans, level = c("Wet_Mass_g", "Dry_Mass_g", "Leaf_Area_cm2", "Leaf_Thickness_Ave_mm", "LDMC", "SLA_cm2_g", "Plant_Height_cm", "C_percent", "N_percent"))) %>% 
  mutate(CG = paste(Country, Gradient, sep = "")) %>% 
  ggplot(aes(x = Elevation, y = meanMean, colour = CG)) +
  geom_point(aes()) +
  geom_smooth(method = "lm", se = FALSE) +
  facet_wrap( ~ Trait_trans, scales = "free_y")


Bmoments %>%
  left_join(meta) %>% 
  filter(!is.na(Trait_trans), Trait_trans %in% c("Wet_Mass_g", "Dry_Mass_g", "Leaf_Area_cm2", "Leaf_Thickness_Ave_mm", "LDMC", "SLA_cm2_g", "Plant_Height_cm", "C_percent", "N_percent")) %>% 
  ungroup() %>% 
  mutate(Trait_trans = factor(Trait_trans, level = c("Wet_Mass_g", "Dry_Mass_g", "Leaf_Area_cm2", "Leaf_Thickness_Ave_mm", "LDMC", "SLA_cm2_g", "Plant_Height_cm", "C_percent", "N_percent"))) %>% 
  mutate(CG = paste(Country, Gradient, sep = "")) %>% 
  filter(meanVar < 10000) %>% 
  ggplot(aes(x = Elevation, y = meanVar, colour = CG)) +
  geom_point(aes()) +
  geom_smooth(method = "lm", se = FALSE)

### TODO
# !!! SV: NA       SV           NA       NA   NANA     NANANA

# Country  Year Site  Gradient BlockID PlotID Taxon     Cover
# <chr>   <dbl> <chr> <chr>    <chr>   <chr>  <chr>     <dbl>
#   1 SV       2018 3C    C        1       3CA    Trisetum…     0

loadd(CountryList)
loadd(CountryList_WD)
loadd(TraitMeans)
loadd(CWTMeans)
loadd(BootstrapMoments_All)
library(ggplot2)
library(vegan)
library("broom")



dat <- map_df(CountryList[-6], CalculateDiversity) %>% 
  filter(!is.na(Gradient)) %>% 
  mutate(PercentRichness = Richness / spPool)
dat

dat %>% 
  ggplot(aes(x = Elevation, y = PercentRichness, colour = Country)) +
  geom_point() + 
  geom_smooth(method = "lm")

res <- dat %>%
  group_by(Country, Gradient) %>% 
  do(fit = lm(PercentRichness ~ Elevation, .))
tidy(res, fit)

dat %>% 
  ggplot(aes(x = Latitude, y = PercentRichness)) +
  geom_point(aes(colour = Country)) + 
  geom_smooth(method = "lm")

summary(lm(PercentRichness ~ Latitude, dat))

meta <- dat %>% 
  distinct(Country, Gradient, Site, PlotID, Elevation, Latitude)

CWTMeans %>% 
  filter(TraitLevel == "TraitMean_site") %>% 
  left_join(meta, by = c("Country", "Site", "Gradient", "PlotID")) %>% 
  ggplot(aes(x = Elevation, y = CWTraitMean, colour = Country)) +
    geom_point(aes()) +
    geom_smooth(method = "lm") +
    facet_wrap(~ Trait_trans, scales = "free_y")

CWTMeans %>% 
  filter(TraitLevel == "TraitMean_site") %>% 
  left_join(meta, by = c("Country", "Site", "Gradient", "PlotID")) %>% 
  ggplot(aes(x = Latitude, y = CWTraitMean)) +
  geom_point(aes(colour = Country)) +
  geom_smooth(method = "lm") +
  facet_wrap(~ Trait_trans, scales = "free_y")
  