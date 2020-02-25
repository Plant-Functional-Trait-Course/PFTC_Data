#### MAKE PRETTY FIGURE PLAN ####

MakePrettyFiguresPlan <- drake_plan(
  
  # Make a map
  PFTCMap = MakePrettyMap(CountryList),
  
  # Coverage Plot
  CoverageFigure <- CoverageFigure(ImputetTraits)
  
)