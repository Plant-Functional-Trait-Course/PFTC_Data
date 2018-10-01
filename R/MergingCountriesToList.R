#### MAKE A LIST FOR ALL COUNTRIES ####

MakeCountryList <- function(metaCH, metaCommunityCH, communityCH, traitCH, 
                            metaPE, metaCommunityPE, communityPE, traitPE, 
                            metaCommunitySV, communitySV, traitSV, 
                            metaNO, metaCommunityNO, communityNO, traitNO
                            ){
  
  CountryList <- list(China = list(meta = metaCH,
                                   metaCommunity = metaCommunityCH,
                                   community = communityCH,
                                   trait = traitCH),
                      
                      Peru = list(meta = metaPE,
                                  metaCommunity = metaCommunityPE,
                                  community = communityPE,
                                  trait = traitPE),
                      
                      Svalbard = list(metaCommunity = metaCommunitySV,
                                      community = communitySV,
                                      trait = traitSV),
                      
                      Norway = list(meta = metaNO,
                                    metaCommunity = metaCommunityNO,
                                    community = communityNO,
                                    trait = traitNO)
                      )
  
}

