#### MAKE A LIST FOR ALL COUNTRIES ####

MakeCountryList <- function(metaCH, traitCH, communityCH, 
                            metaPE, traitPE, communityPE, 
                            traitSV, communitySV,
                            metaNO){
  
  CountryList <- list(China = list(meta = metaCH,
                                   community = communityCH,
                                   trait = traitCH),
                      
                      Peru = list(meta = metaPE,
                                  community = communityPE,
                                  trait = traitPE),
                      
                      Svalbard = list(community = communitySV,
                                      trait = traitSV),
                      
                      Norway = list(meta = metaNO)
                      )
  
}

