#### MAKE A LIST FOR ALL COUNTRIES ####

MakeCountryList <- function(metaCH, metaCommunityCH, communityCH, traitCH, fluxCH,
                            metaPE, metaCommunityPE, communityPE, traitPE, fluxPE,
                            metaSV, metaCommunitySV, communitySV, traitSV, fluxSV,
                            metaNO, metaCommunityNO, communityNO, traitNO, fluxNO,
                            metaCO, metaCommunityCO, communityCO, traitCO, fluxCO
                            ){
  
  CountryList <- list(China = list(meta = metaCH,
                                   metaCommunity = metaCommunityCH,
                                   community = communityCH,
                                   trait = traitCH,
                                   flux = fluxCH),
                      
                      Peru = list(meta = metaPE,
                                  metaCommunity = metaCommunityPE,
                                  community = communityPE,
                                  trait = traitPE,
                                  flux = fluxPE),
                      
                      Svalbard = list(meta = metaSV,
                                      metaCommunity = metaCommunitySV,
                                      community = communitySV,
                                      trait = traitSV,
                                      flux = fluxSV),
                      
                      Norway = list(meta = metaNO,
                                    metaCommunity = metaCommunityNO,
                                    community = communityNO,
                                    trait = traitNO,
                                    flux = fluxNO),
                      
                      Colorado = list(meta = metaCO,
                                      metaCommunity = metaCommunityCO,
                                      community = communityCO,
                                      trait = traitCO,
                                      flux = fluxCO)
                      )
  return(CountryList)
  
}

