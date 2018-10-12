####################################
### Community composition change ###
####################################

library("vegan")
library("ggvegan")

CommunityOrdination <- function(dat){
  set.seed(42)
  
  com_fat <- dat$community %>% 
  spread(key = Taxon, value = Cover, fill = 0) 

  com_fat_spp <- com_fat %>% select(-(Country:Gradient))

  NMDS <- metaMDS(com_fat_spp, noshare = TRUE, try = 3)

  fNMDS <- fortify(NMDS) %>% 
    filter(Score == "sites") %>% 
    bind_cols(com_fat %>% select(Country:PlotID))
  
  return(fNMDS)
}

CommunityOrdinationPlot <- ggplot(fNMDS, aes(x = NMDS1, y = NMDS2, group = Site, shape = Site))+
  geom_point() +
  coord_equal() +
  labs(x = "NMDS axis 1", y = "NMDS axis 2") +
  theme_bw()
