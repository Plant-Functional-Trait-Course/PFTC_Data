library(drake)

r_make(source = "R/Gradient-analysis-with-drake.R")

failed()

#view dependency graph
r_vis_drake_graph(source = "R/Gradient-analysis-with-drake.R", targets_only = TRUE)




# !!! SV: NA       SV           NA       NA   NANA     NANANA

loadd(CountryList)
loadd(TraitMeans)
loadd(CWTMeans)
library(ggplot2)
library(vegan)
library("broom")

CalculateDiversity <- function(CountryList){
  diversity <- CountryList$community %>% 
    left_join(CountryList$meta, by = c("Country", "Site", "Gradient")) %>% 
    group_by(Country, Gradient, Elevation, Latitude) %>% 
    mutate(spPool = n()) %>% 
    group_by(Country, Gradient, Elevation, Latitude, Site, PlotID, spPool) %>%  
    summarise(Richness = n(),
              Diversity = diversity(Cover), 
              N1 = exp(Diversity),
              Evenness = Diversity/log(Richness))
  return(diversity)
}

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
  