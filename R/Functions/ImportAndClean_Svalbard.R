##################
### SVALBARD  ###
##################


#### CLEANING DATA ####
# Cleaning Svalbard meta community
CleanSvalbardMetaCommunity <- function(metaCommunitySV_raw){
  metaCommunitySV <- metaCommunitySV_raw %>%
    mutate(Lichen_rock = gsub("_", ".", Lichen_rock),
           Lichen_rock = as.numeric(Lichen_rock)) %>% 
    mutate(Lichen = rowSums(select(., Lichen_soil, Lichen_rock), na.rm = TRUE),
           Site = paste(Site, PlotID, sep="")) %>% 
    rename(Bryophyte = Bryophytes) %>% 
    select(Gradient, Site, PlotID, MedianHeight_cm, Vascular, Bryophyte, Lichen, Rock, BareGround, BioCrust, Litter, Country, Year, Project)
  return(metaCommunitySV)
}


# Cleaning Svalbard community
CleanSvalbardCommunity <- function(communitySV_raw){
  communitySV <- communitySV_raw %>% 
    rename(Latitude = Latitude_N, Longitude = Longitude_E, Elevation = Elevation_m) %>%
    mutate(PlotID = paste(Site, Gradient, PlotID, sep=""),
           Site = paste(Site, Gradient, sep =""),
           BlockID = as.character(1),
           Cover = as.numeric(Cover)) %>% 
    select(Country, Year, Site, Gradient, BlockID, PlotID, Taxon, Cover) %>% 
    mutate(Taxon = recode(Taxon, "micranthes hieracifolia" = "micranthes hieraciifolia"),
           Taxon = stringi::stri_trans_totitle(
             Taxon, 
             opts_brkiter = stringi::stri_opts_brkiter(type = "sentence"))) %>% 
    filter(Cover != 0) #%>% 
    #filter(Site != "NANA")

  return(communitySV)
}
#communitySV_raw %>% filter(Project == "T" & Site == "2" & Gradient == "C" & PlotID == "B" & Taxon == "oxyria digyna") %>% data.frame()


# Cleaning Svalbard trait
CleanSvalbardTrait <- function(traitSV_raw){
  traitSV <- traitSV_raw %>% 
    rename(Latitude = Latitude_N, Longitude = Longitude_E, Elevation = Elevation_m)%>%
    mutate(PlotID = paste(Site, Gradient, PlotID, sep=""),
           BlockID = as.character(1),
           Site = paste(Site, Gradient, sep ="")
    ) %>%
    select(Country, Year, Site, Gradient, BlockID, PlotID, Taxon, Plant_Height_cm, Wet_Mass_g, Dry_Mass_g, Leaf_Thickness_Ave_mm, Leaf_Area_cm2, SLA_cm2_g, LDMC) %>% 
    gather(key = Trait, value = Value, -Country, -Year, -Site, -Gradient, -BlockID, -PlotID, -Taxon) %>% 
    filter(!is.na(Value)) %>% 
    mutate(Taxon = stringi::stri_trans_totitle(
      Taxon, 
      opts_brkiter = stringi::stri_opts_brkiter(type = "sentence")))
  
  return(traitSV)
}


#### IMPORT, CLEAN AND MAKE LIST #### 
ImportClean_Svalbard <- function(){
  

  ## DOWNLOAD DATA FROM OSF
  # meta
  get_file(node = "7mzjk",
           file = "metaSV.csv",
           path = "data_cleaned",
           remote_path = "Svalbard")
  # community
  get_file(node = "7mzjk",
           file = "communitySV_2018.Rdata",
           path = "data_cleaned",
           remote_path = "Svalbard")
  # metaCommunity
  get_file(node = "7mzjk",
           file = "metaCommunitySV_2018.Rdata",
           path = "data_cleaned",
           remote_path = "Svalbard")
  # traits
  get_file(node = "7mzjk",
           file = "traitsGradients_SV_2018.Rdata",
           path = "data_cleaned",
           remote_path = "Svalbard")
  # flux
  get_file(node = "7mzjk",
           file = "standardControlFluxSV_2016.Rdata",
           path = "data_cleaned",
           remote_path = "Svalbard")

  
  ### IMPORT DATA
  # meta data
  metaSV = read_csv(file_in("data_cleaned/metaSV.csv"))
  # meta community
  metaCommunitySV_raw = get(load(file = file_in("data/metaCommunitySV_2018.Rdata")))
  # community
  communitySV_raw <- get(load(file = file_in("data/communitySV_2018.Rdata")))
  # trait
  traitSV_raw <- get(load(file = file_in("data/traitsGradients_SV_2018.Rdata")))
  # flux
  fluxSV <- get(load("data/standardControlFluxSV_2016.Rdata"))
  hierarchySV = c("Country", "Site", "BlockID", "PlotID")

  
  ### CLEAN DATA SETS
  ## CN_Gongga
  metaCommunitySV = CleanSvalbardMetaCommunity(metaCommunitySV_raw)
  communitySV = CleanSvalbardCommunity(communitySV_raw)
  traitSV = CleanSvalbardTrait(traitSV_raw)
  
  # Make list
  Data_SV = list(meta = metaSV,
                 metaCommunity = metaCommunitySV,
                 community = communitySV,
                 trait = traitSV,
                 flux = fluxSV,
                 trait_hierarchy = hierarchySV)
  
  return(Data_SV)
}
