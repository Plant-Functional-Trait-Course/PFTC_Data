source("R/CountryListAndTraitMeanDrakePlan.R")

# CWTrait Means
CWTraitMeanDrakePlan <- drake_plan(
  #Making local, site, regional and global level trait means
  Full_TraitMeans = Community_TraitMeans(CountryList, TraitMeans),
  #Deciding what level you want to filter for 80% of the community, here I chose the global level
  Community_Trait = Threshold_filter(Full_TraitMeans, TraitMean_global),
  #Calculating community weighted means
  CW_Traits = CommunityW_TraitMeans(Community_Trait)
  #CWTMeans = CommunityW_Means(Full_CWTraitMeans)
)

# Bootstrapped CWM
#BootstrapMoments_All = CountryList_trans %>% 
# map_df(CWM_Bootstrapping),

# Summarize Bootstrap Moments
#BootstrapMoments = SummarizeBootMoments(BootstrapMoments_All),


#BootstrapMoments_Bio = BootstrapMoments %>% 
# left_join(metaBioclim, by = c("Country", "Site")),

#GradientPlot = MakeFigure(BootstrapMoments),
#GradientMeanPlot = MakeMeanFigure(CW_Means_Bootstrapped_Bio),
#GradientVarPlot = MakeVarFigure(CW_Means_Bootstrapped_Bio),
#GradientSkewPlot = MakeSkewFigure(CW_Means_Bootstrapped_Bio),
#GradientKurtPlot = MakeKurtFigure(CW_Means_Bootstrapped_Bio)

#)


# combine import and analysis to master drake plan
MasterDrakePlan <- ImportDrakePlan %>% 
  bind_rows(CountryListAndTraitMeanDrakePlan) %>% 
  bind_rows(CWTraitMeanDrakePlan)

#configure and make drake plan
config <- drake_config(MasterDrakePlan)

config
