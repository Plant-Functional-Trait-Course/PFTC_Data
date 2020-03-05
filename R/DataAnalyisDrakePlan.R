#### DATA ANALYSIS PLAN ####


DataAnalysisDrakePlan <- drake_plan(
  
  # Test realationship of Diversity along gradients
  ResultsDiversity = RegressionDiversityIndeces(Diversity),
  
  # NMDS Ordination
  fNMDS = map_df(CountryList, NMDSOrdination),

  # Regression on Moments
  RegMoment = MomentRegression(HappyMoments_Site)
) 
