loadd("CountryList")
source("trait_distributions/r_functions/misc.R")

##############################
#Combining community data

#Add china data

CountryList$China$community$globalplotID<-paste(CountryList$China$community$Country,"_",CountryList$China$community$PlotID,sep="")
#chinaco<-CountryList$China$community
combined_community<-CountryList$China$community[c("globalplotID","PlotID","Country","Year","Site","Treatment","Taxon","Cover")]

#Add peru data
CountryList$Peru$community$globalplotID<-paste(CountryList$Peru$community$Country,"_",CountryList$Peru$community$PlotID,"_",CountryList$Peru$community$Treatment,sep="")
#peruco<-CountryList$Peru$community
combined_community<-
  rbind(combined_community,
        CountryList$Peru$community[c( "globalplotID","PlotID","Country","Year","Site","Treatment","Taxon","Cover")]
  )

#Add Svalbard data
CountryList$Svalbard$community$globalplotID<-paste(CountryList$Svalbard$community$Country,"_",CountryList$Svalbard$community$Gradient,"_",CountryList$Svalbard$community$Site,"_",CountryList$Svalbard$community$PlotID,sep="")
CountryList$Svalbard$community$Treatment<-"C"
CountryList$Svalbard$community$Site<-paste(CountryList$Svalbard$community$Gradient,"_",CountryList$Svalbard$community$Site,sep="")
#svco<-CountryList$Svalbard$community
combined_community<-
  rbind(combined_community,
        CountryList$Svalbard$community[c( "globalplotID","PlotID","Country","Year","Site","Treatment","Taxon","Cover")]
  )

#Add Norway data
CountryList$Norway$community$globalplotID<-paste(CountryList$Norway$community$Country,"_",CountryList$Norway$community$PlotID,sep="")
if(is.null(CountryList$Norway$community$Treatment)){CountryList$Norway$community$Treatment<-"C"}
#nocom<-CountryList$Norway$community

norway_translator<-CountryList$Norway$trait[c('Taxon',"ID")]
norway_translator<-na.omit(norway_translator)
norway_translator$ID<-unlist(lapply(X = norway_translator$ID,FUN = function(x){paste(strsplit(x,split = "_")[[1]][2:3],collapse = " "  )}))
norway_translator<-unique(norway_translator)
CountryList$Norway$community$Taxon<-unlist(lapply(X = CountryList$Norway$community$Taxon,FUN = function(x){if(length(which(norway_translator$ID==x))>0){norway_translator$Taxon[which(norway_translator$ID==x)]  }else(NA)}))
rm(norway_translator)

combined_community<-
  rbind(combined_community,
        CountryList$Norway$community[c( "globalplotID","PlotID","Country","Year","Site","Treatment","Taxon","Cover")]
  )



#Add Colorado data
CountryList$Colorado$community$globalplotID<-paste(CountryList$Colorado$community$Country,"_",CountryList$Colorado$community$Site,"_",CountryList$Colorado$community$PlotID,sep="")
if(is.null(CountryList$Colorado$community$Treatment)){CountryList$Colorado$community$Treatment<-"C"}
#cocom<-CountryList$Colorado$community
combined_community<-
  rbind(combined_community,
        CountryList$Colorado$community[c( "globalplotID","PlotID","Country","Year","Site","Treatment","Taxon","Cover")]
  )


combined_community$Taxon<-firstup(combined_community$Taxon)

combined_community$Taxon<-gsub(pattern = "\t",replacement = "",x = combined_community$Taxon)

#####################################

#Combining trait data


#Add china
#ch_trait<-CountryList$China$trait
combined_trait <- 
  CountryList$China$trait[c("ID","Country","Year","Site","Treatment","Taxon",
                            "Wet_Mass_g","Dry_Mass_g","Leaf_Thickness_Ave_mm","Leaf_Area_cm2","SLA_cm2_g","LDMC",
                            "C_percent","N_percent","CN_ratio","dN15_percent","dC13_percent","P_AVG")]

#Add peru
#pe_trait<-CountryList$Peru$trait

if(is.null(CountryList$Peru$trait$C_percent)){CountryList$Peru$trait$C_percent<-NA}
if(is.null(CountryList$Peru$trait$N_percent)){CountryList$Peru$trait$N_percent<-NA}
if(is.null(CountryList$Peru$trait$CN_ratio)){CountryList$Peru$trait$CN_ratio<-NA}
if(is.null(CountryList$Peru$trait$dN15_percent)){CountryList$Peru$trait$dN15_percent<-NA}
if(is.null(CountryList$Peru$trait$dC13_percent)){CountryList$Peru$trait$dC13_percent<-NA}
if(is.null(CountryList$Peru$trait$P_AVG)){CountryList$Peru$trait$P_AVG<-NA}

combined_trait <- 
  rbind(combined_trait,CountryList$Peru$trait[c("ID","Country","Year","Site","Treatment","Taxon",
                                                "Wet_Mass_g","Dry_Mass_g","Leaf_Thickness_Ave_mm","Leaf_Area_cm2","SLA_cm2_g","LDMC",
                                                "C_percent","N_percent","CN_ratio","dN15_percent","dC13_percent","P_AVG")])



#Add Svalbard
#svtrait<-CountryList$Svalbard$trait
CountryList$Svalbard$trait$Site<-paste(CountryList$Svalbard$trait$Gradient,"_",CountryList$Svalbard$trait$Site,sep="")
if(is.null(CountryList$Svalbard$trait$C_percent)){CountryList$Svalbard$trait$C_percent<-NA}
if(is.null(CountryList$Svalbard$trait$N_percent)){CountryList$Svalbard$trait$N_percent<-NA}
if(is.null(CountryList$Svalbard$trait$CN_ratio)){CountryList$Svalbard$trait$CN_ratio<-NA}
if(is.null(CountryList$Svalbard$trait$dN15_percent)){CountryList$Svalbard$trait$dN15_percent<-NA}
if(is.null(CountryList$Svalbard$trait$dC13_percent)){CountryList$Svalbard$trait$dC13_percent<-NA}
if(is.null(CountryList$Svalbard$trait$P_AVG)){CountryList$Svalbard$trait$P_AVG<-NA}
CountryList$Svalbard$trait$Taxon<-firstup(CountryList$Svalbard$trait$Taxon)



combined_trait <- 
  rbind(combined_trait,CountryList$Svalbard$trait[c("ID","Country","Year","Site","Treatment","Taxon",
                                                    "Wet_Mass_g","Dry_Mass_g","Leaf_Thickness_Ave_mm","Leaf_Area_cm2","SLA_cm2_g","LDMC",
                                                    "C_percent","N_percent","CN_ratio","dN15_percent","dC13_percent","P_AVG")])



#Add Norway data

#notrait<-CountryList$Norway$trait
if(is.null(CountryList$Norway$trait$Treatment)){CountryList$Norway$trait$Treatment<-"C"}
if(is.null(CountryList$Norway$trait$dN15_percent)){CountryList$Norway$trait$dN15_percent<-NA}
if(is.null(CountryList$Norway$trait$dC13_percent)){CountryList$Norway$trait$dC13_percent<-NA}
if(is.null(CountryList$Norway$trait$P_AVG)){CountryList$Norway$trait$P_AVG<-NA}
combined_trait <- 
  rbind(combined_trait,CountryList$Norway$trait[c("ID","Country","Year","Site","Treatment","Taxon",
                                                  "Wet_Mass_g","Dry_Mass_g","Leaf_Thickness_Ave_mm","Leaf_Area_cm2","SLA_cm2_g","LDMC",
                                                  "C_percent","N_percent","CN_ratio","dN15_percent","dC13_percent","P_AVG")])


#Add Colorado data

#cotrait<-CountryList$Colorado$trait
if(is.null(CountryList$Colorado$trait$ID)){CountryList$Colorado$trait$ID<-paste("Co_",1:nrow(CountryList$Colorado$trait),sep="")}
if(is.null(CountryList$Colorado$trait$Treatment)){CountryList$Colorado$trait$Treatment<-"C"}
colnames(CountryList$Colorado$trait)[which(colnames(CountryList$Colorado$trait)=="Wet_mass_g")]<-"Wet_Mass_g"
combined_trait <- 
  rbind(combined_trait,CountryList$Colorado$trait[c("ID","Country","Year","Site","Treatment","Taxon",
                                                    "Wet_Mass_g","Dry_Mass_g","Leaf_Thickness_Ave_mm","Leaf_Area_cm2","SLA_cm2_g","LDMC",
                                                    "C_percent","N_percent","CN_ratio","dN15_percent","dC13_percent","P_AVG")])

combined_trait$Taxon<-firstup(combined_trait$Taxon)
combined_trait$Taxon<-gsub(pattern = "\t",replacement = "",x = combined_trait$Taxon)
