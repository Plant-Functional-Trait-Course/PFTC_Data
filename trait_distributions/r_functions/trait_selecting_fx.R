#Function to generate sets of traits for trait_distribution function to use


#Initial stab: if plastic = TRUE, prioritize destination sites
#1) species in site
#2) species in country
#3) species anywhere

#Inputs:
  #species = species vector
  #site = character
  #country = character
  #traits dataframe

#Output: dataframe with
  #row = individuals
  #columns = species, traits

#For plastic, set site to destination site, if fixed, set site to origin

select_traits<-function(species,site,country,traits_dataframe){

traits_output<-NULL    


for(i in 1:length(unique(species))){
  
species_i<-unique(species)[i]  
#genus_i<-strsplit(x = species_i,split = " ")[[1]][1]

#1) species in specified site

out_i<-traits_dataframe[which(traits_dataframe$Taxon==species_i & 
                                        traits_dataframe$Site == site &
                                        traits_dataframe$Country == country   ),]




#2) species in country
if(nrow(out_i)==0){
  out_i<-traits_dataframe[which(traits_dataframe$Taxon==species_i & 
                                  traits_dataframe$Country ==country),]
}

#3) species anywhere
if(nrow(out_i)==0){
  out_i<-traits_dataframe[which(traits_dataframe$Taxon==species_i ),]
  
}


if(nrow(out_i)!=0){
#out_i$assigned_species<-species_i
traits_output<-rbind(traits_output,out_i)
}
  
} #for i species loop 
  
  
return(traits_output)  

  
}
