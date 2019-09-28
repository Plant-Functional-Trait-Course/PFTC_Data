source("R/CountryListAndTraitMeanDrakePlan.R")

# CWTrait Means
CWTraitMeanDrakePlan <- drake_plan(
  #Calculating community weighted trait means
  Full_CWTraitMeans = CommunityW_TraitMeans(CountryList, TraitMeans),
  CWTMeans = CommunityW_Means(Full_CWTraitMeans)
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