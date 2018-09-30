#### MAKE A LIST FOR ALL COUNTRIES ####

MakeCountryList <- function(metaCH, communityCH, metaCommunityCH, traitCH, 
                            metaPE, communityPE, traitPE, 
                            communitySV, metaCommunitySV, traitSV, 
                            metaNO, communityNO, metaCommunityNO, traitNO
                            ){
  
  CountryList <- list(China = list(meta = metaCH,
                                   community = communityCH,
                                   metaCommunity = metaCommunityCH,
                                   trait = traitCH),
                      
                      Peru = list(meta = metaPE,
                                  community = communityPE,
                                  metaCommunity = NA,
                                  trait = traitPE),
                      
                      Svalbard = list(meta = NA,
                                      community = communitySV,
                                      metaCommunity = metaCommunitySV,
                                      trait = traitSV),
                      
                      Norway = list(meta = metaNO,
                                    community = communityNO,
                                    metaCommunity = metaCommunityNO,
                                    trait = traitNO),
                      
                      Colorado = list(meta = NA,
                                    community = NA,
                                    metaCommunity = NA,
                                    trait = NA)
                      )
  
}

