


source("Gradient-analysis-with-drake.R")
loadd("CountryList")
source("trait_distributions/r_functions/misc.R")
source("trait_distributions/r_functions/prep_trait_and_comm_objects.R")
source("trait_distributions/r_functions/trait_distribution_fx.R")
source("trait_distributions/r_functions/trait_selecting_fx.R")

#Minor fixes
combined_community$Cover<-gsub(pattern = " ",replacement = "",x = combined_community$Cover)
combined_community<-combined_community[which(!is.na(combined_community$PlotID)),]


n_replicates=200

#Inputs:
  #Number of replicated outputs
  # Species abundance dataframe (2 columns, first column: species name, second column: abundance)
  # Trait data frame (2 or more columns: first column: species name, columns 2+ : traits)

#Output
  #Matrix with nrows = number of replicates, ncols = total abundance


trait_means<-species_trait_means(trait_data_frame = as.data.frame(combined_trait[,c("Taxon","Wet_Mass_g","Dry_Mass_g","Leaf_Thickness_Ave_mm","Leaf_Area_cm2","SLA_cm2_g",
                                                                                    "LDMC","C_percent","N_percent","CN_ratio","dN15_percent","dC13_percent","P_AVG" )]))

for( i in 321:nrow(unique(combined_community[c('globalplotID',"Year")]))){

plot_i<-as.character(unique(combined_community[c('globalplotID',"Year")])[i,1])
year_i<-as.character(unique(combined_community[c('globalplotID',"Year")])[i,2])
data_i<-combined_community[which(combined_community$globalplotID==plot_i & combined_community$Year==year_i),]
data_i<-data_i[which(!is.na(data_i$Cover)),]

species_i<-unique(data_i$Taxon)    
site_i<-as.character(unique(data_i$Site))
country_i<-as.character(unique(data_i$Country))

abd_i<-as.data.frame(data_i[c('Taxon',"Cover")])

dist_i<-trait_distributions(number_replicates = n_replicates,
                    abundance_data = abd_i,
                    trait_data = trait_means)

#write output

for(f in 1:length(dist_i)){
  
  
  trait_f<-names(dist_i)[[f]]    
  dist_f<-dist_i[[f]]  
  
  if(!is.null(dist_f)){#if there is distribution data, write it
  if(nrow(dist_f)!=n_replicates){stop("Number of rows in distribution does not equal number of replicates")}  
  
  write.csv(x = dist_f,file = paste("trait_distributions/output_distributions_no_itv/",plot_i,".",year_i,".",trait_f,".csv",sep = ""),row.names = F  )
  }#if there is distribution data
  rm(trait_f,dist_f)  
  
}


print(paste(round(i/nrow(unique(combined_community[c('globalplotID',"Year")]))*100,digits = 2)," percent done" ,sep = ""))
  
  
  
}#end i loop

rm(country_i,f,i,n_replicates,plot_i,site_i,species_i)
