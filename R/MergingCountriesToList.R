#### MAKE A LIST FOR ALL COUNTRIES ####

MakeCountryList <- function(traitCH, communityCH, traitPE, communityPE, traitSV, communitySV){
  
  CountryList <- list(China = list(meta = metaCH,
                                   community = communityCH,
                                   trait = traitCH),
                      Peru = list(community = communityPE,
                                  trait = traitPE),
                      Svalbard = list(community = communitySV,
                                      trait = traitSV)
                      )
  
}

