####################################
   ### MULTIVARIATE ANALYSIS ###
####################################

# NMDS ordination
NMDSOrdination <- function(CountryList){
  
  comm_wide <- CountryList$community %>% 
    pivot_wider(names_from = Taxon, values_from = Cover, values_fill = list(Cover = 0)) 
  comm_wide_spp <- comm_wide %>% select(-(Country:PlotID))

  NMDS <- metaMDS(comm_wide_spp, noshare = TRUE, try = 30)

  fNMDS <- fortify(NMDS) %>% 
    filter(Score == "sites") %>% 
    bind_cols(comm_wide %>% select(Country:PlotID))
  
  return(fNMDS)
}




