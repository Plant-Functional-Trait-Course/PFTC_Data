####################################
   ### MULTIVARIATE ANALYSIS ###
####################################

# NMDS ordination
NMDSOrdination <- function(CountryList){
  set.seed(42)
  cols <- c(PlotID = "1")
  
  comm_wide <- CountryList$community %>% 
    # add plotID in colorado
    add_column(!!!cols[!names(cols) %in% names(.)]) %>% 
    pivot_wider(names_from = Taxon, values_from = Cover, values_fill = list(Cover = 0)) 
  comm_wide_spp <- comm_wide %>% select(-(Country:PlotID))

  NMDS <- metaMDS(comm_wide_spp, noshare = TRUE, try = 30)

  fNMDS <- fortify(NMDS) %>% 
    filter(Score == "sites") %>% 
    as.tibble() %>% 
    bind_cols(comm_wide) %>% 
    select(Country:PlotID, NMDS1, NMDS2)
  
  return(fNMDS)
}


