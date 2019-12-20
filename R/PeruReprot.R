library("tidyverse")
library("broom")
library("vegan")
library("e1071")

get_file(node = "7mzjk",
         file = "CommunityCover_2018_Peru.csv", 
         path = "data")

get_file(node = "7mzjk", 
         file = "traits_2018_Peru_cleaned.csv", 
         path = "data")

trait.raw <- read_csv(file = "data/traits_2018_Peru_cleaned.csv", col_names = TRUE)
comm.raw <- read_csv(file = "data/CommunityCover_2018_Peru.csv", col_names = TRUE)

CleanPeruCommunity <- function(communityPE_raw){
  communityPE <- communityPE_raw %>% 
    mutate(Country = "PE", 
           BlockID = as.character(1),
           PlotID = paste(Site, PlotID, Treatment, sep="_"),
           Gradient = as.character(1)) %>% 
    select(Country, Year, Site, Gradient, BlockID, PlotID, Taxon, Cover) %>% 
    filter(!is.na(Cover))
  return(communityPE)
}
comm <- CleanPeruCommunity(comm.raw)

trait <- trait.raw %>% 
  mutate(BlockID = as.character(1),
         PlotID = paste(Site, PlotID, Treatment, sep="_"),
         Gradient = as.character(1)) %>%
  select(Country, Year, Site, Gradient, Treatment, BlockID, PlotID, Taxon, Plant_Height_cm, Wet_Mass_g, Dry_Mass_g, Leaf_Thickness_Ave_mm, Leaf_Area_cm2, SLA_cm2_g, LDMC) %>% 
  gather(key = Trait, value = Value, -Country, -Year, -Site, -BlockID, -PlotID, -Gradient, -Treatment, -Taxon) %>% 
  mutate(Taxon = recode(Taxon, "Agrostis 2" = "Agrostis sp2")) %>% 
  filter(!is.na(Value))

load("data/metaPE.Rdata")
metaPE <- metaPE %>% 
  filter(!c(Elevation == 3675 & Site == "ACJ"))
load(file = "data/standardControlFluxPE_2016.Rdata")
flux <- standardControlFluxPE_2016

TraitWeights_plot <- comm %>% 
  left_join(trait %>% select(-Year), by = c("Country", "Site", "Gradient", "BlockID", "PlotID", "Taxon")) %>% 
  group_by(Country, Year, Site, Gradient, Treatment, BlockID, PlotID, Taxon, Trait) %>% 
  mutate(weight = Cover/n()) %>% 
  group_by(Country, Year, Site, Gradient, Treatment, BlockID, PlotID, Trait) %>% 
  filter(!is.na(Trait))


BootstrapMoments_All <- rerun(.n = 100, TraitWeights_plot %>% 
                                group_by(Country, Year, Site, Gradient, Treatment, BlockID, PlotID, Trait) %>% 
                                sample_n(size = 100, replace = TRUE, weight = weight)) %>% 
  bind_rows(.id = "n") %>%
  group_by(n, add = TRUE) %>%
  # get all the happy moments
  summarise(Mean = mean(Value), Variance = var(Value), Skewness = skewness(Value), Kurtosis = kurtosis(Value))


#***********************************************************************
# Plots SERFOR (WAY, PIL)
trait2 <- BootstrapMoments_All %>% 
  left_join(metaPE, by = c("Site", "Country")) %>% 
  filter(Site %in% c("WAY", "PIL"),
         Treatment != "BB",
         !is.na(Mean),
         Mean != Inf,
         !Trait %in% c("Dry_Mass_g", "Wet_Mass_g")) %>% 
  filter(!c(Trait == "SLA_cm2_g" & Mean > 200)) %>% 
  ungroup() %>% 
  mutate(Site = factor(Site, levels = c("WAY", "PIL")))

# dviersity
comm %>% 
  group_by(Site) %>% 
  summarise(richness = n(),
            diversity = diversity(Cover, index = "shannon"),
            evenness = diversity/log(richness))

trait1_names <- c("Leaf_Thickness_Ave_mm" = "Grosor medio de la hoja en mm",
                 "SLA_cm2_g" = "SLA en cm2 por gramo",
                 "LDMC" = "LDMC",
                 "Leaf_Area_cm2" = "Área de la hoja en cm",
                 "Plant_Height_cm" = "Altura de la planta en cm")

ggplot(trait2, aes(x = Site, y = Mean)) +
  geom_boxplot() +
  labs(x = "", y = "Valor medio del rasgo") +
  facet_wrap(~ Trait, scales = "free_y", labeller = as_labeller(trait1_names))

res <- trait2 %>% 
  group_by(Trait) %>% 
  do(fit = lm(Mean ~ Site, data = .))
res <- tidy(res, fit)
write_csv(res, path = "res.csv")

ggplot(trait2, aes(x = Site, y = Mean, fill = Treatment)) +
  geom_boxplot() +
  labs(x = "", y = "Valor medio del rasgo") +
  facet_wrap(~ Trait, scales = "free_y", labeller = as_labeller(trait1_names))
res <- trait2 %>% 
  group_by(Trait) %>% 
  do(fit2 = lm(Mean ~ Site*Treatment, data = .))
res <- tidy(res, fit2)
write_csv(res, path = "res.csv")

plot_names <- c("GPP1200" = "GGP",
                "Reco15" = "Respiration")
flux %>% 
  gather(key = Variable, value = Value, Reco15, GPP1200) %>% 
  filter(Site %in% c("WAY", "PIL")) %>% 
  ungroup() %>% 
  mutate(Site = factor(Site, levels = c("WAY", "PIL"))) %>% 
  ggplot(aes(x = Site, y = Value)) +
  geom_boxplot() +
  labs(x = "", y = "Mean value") +
  facet_wrap(~ Variable, scales = "free_y", labeller = as_labeller(plot_names))

res <- flux %>% 
  gather(key = Variable, value = Value, Reco15, GPP1200) %>% 
  filter(Site %in% c("WAY", "PIL")) %>% 
  group_by(Variable) %>% 
  do(fit3 = lm(Value ~ Site, data = .))
res <- tidy(res, fit3)

flux2 <- flux %>% 
  filter(Site %in% c("WAY", "PIL")) %>% 
  select(Site, PlotID, Treatment, GPP1200, Reco15) %>% 
  gather(key = Flux, value = value, GPP1200, Reco15) %>% 
  left_join(trait2, by = c("Site", "PlotID", "Treatment"))

trait_names <- c("Leaf_Thickness_Ave_mm" = "Grosor medio de la hoja en mm",
                "SLA_cm2_g" = "SLA en cm2 por gramo",
                "LDMC" = "LDMC",
                "Leaf_Area_cm2" = "Área de la hoja en cm",
                "Plant_Height_cm" = "Altura de la planta en cm",
                "GPP1200" = "Tasa fotosintética",
                "Reco15" = "Respiration")
ggplot(flux2, aes(x = Mean, y = value)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(x = "Valor medio del rasgo", y = "") +
  facet_grid(Flux ~ Trait, scales = "free", labeller = as_labeller(trait_names))
  
res <- flux2 %>% 
  group_by(Trait, Flux) %>% 
  do(fit4 = lm(value ~ Mean, data = .))
tidy(res, fit4)


#***********************************************************************
# Plots SERNAP (TRE, ACJ)
trait3 <- BootstrapMoments_All %>% 
  left_join(metaPE, by = c("Site", "Country")) %>% 
  filter(Site %in% c("TRE", "ACJ"),
         Treatment != "BB",
         !is.na(Mean),
         Mean != Inf,
         !Trait %in% c("Dry_Mass_g", "Wet_Mass_g")) %>% 
  filter(!c(Trait == "SLA_cm2_g" & Mean > 200)) %>% 
  ungroup() %>% 
  mutate(Site = factor(Site, levels = c("ACJ", "TRE")))

# dviersity
comm %>% 
  group_by(Site) %>% 
  summarise(richness = n(),
            diversity = diversity(Cover, index = "shannon"),
            evenness = diversity/log(richness))

trait1_names <- c("Leaf_Thickness_Ave_mm" = "Grosor medio de la hoja en mm",
                  "SLA_cm2_g" = "SLA en cm2 por gramo",
                  "LDMC" = "LDMC",
                  "Leaf_Area_cm2" = "Área de la hoja en cm2",
                  "Plant_Height_cm" = "Altura de la planta en cm")

ggplot(trait3, aes(x = Site, y = Mean)) +
  geom_boxplot() +
  labs(x = "", y = "Valor medio del rasgo") +
  facet_wrap(~ Trait, scales = "free_y", labeller = as_labeller(trait1_names))

res <- trait3 %>% 
  group_by(Trait) %>% 
  do(fit = lm(Mean ~ Site, data = .))
res <- tidy(res, fit)
write_csv(res, path = "res.csv")

trait3 %>% 
  filter(Site == "ACJ") %>% 
  ggplot(aes(x = Treatment, y = Mean, fill = Treatment)) +
  geom_boxplot() +
  labs(x = "", y = "Valor medio del rasgo") +
  facet_wrap(~ Trait, scales = "free_y", labeller = as_labeller(trait1_names))
res <- trait3 %>% 
  filter(Site == "ACJ") %>% 
  group_by(Trait) %>% 
  do(fit2 = lm(Mean ~ Treatment, data = .))
res <- tidy(res, fit2)
write_csv(res, path = "res.csv")

# Flux
flux3 <- flux %>% 
  filter(Site %in% c("ACJ", "TRE")) %>% 
  select(Site, PlotID, Treatment, GPP1200, Reco15) %>% 
  gather(key = Flux, value = value, GPP1200, Reco15) %>% 
  left_join(trait3, by = c("Site", "PlotID", "Treatment"))

trait_names <- c("Leaf_Thickness_Ave_mm" = "Grosor medio de la hoja en mm",
                 "SLA_cm2_g" = "SLA en cm2 por gramo",
                 "LDMC" = "LDMC",
                 "Leaf_Area_cm2" = "Área de la hoja en cm",
                 "Plant_Height_cm" = "Altura de la planta en cm",
                 "GPP1200" = "Tasa fotosintética",
                 "Reco15" = "Respiration")
ggplot(flux3, aes(x = Mean, y = value)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(x = "Valor medio del rasgo", y = "") +
  facet_grid(Flux ~ Trait, scales = "free", labeller = as_labeller(trait_names))

res <- flux3 %>% 
  group_by(Trait, Flux) %>% 
  do(fit4 = lm(value ~ Mean, data = .))
tidy(res, fit4)

# Plots SERNAP (TRE, ACJ)


