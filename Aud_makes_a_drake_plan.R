#**********************************************************************
library(drake)

r_make(source = "R/AudsDrakePlan.R")
loadd()
failed()

#view dependency graph
r_vis_drake_graph(source = "R/AudsDrakePlan.R", targets_only = TRUE)

#**********************************************************************


# Save figures
ggsave(file = "CoveragePlot.png", device = "png", width = 7, height = 4, dpi = 300)
ggsave(file = "PFTCMap.png", device = "png", width = 8, height = 6, dpi = 300)
ggsave(GradientMeanPlot, file = "GradientMeanPlot.png", device = "png", width = 7, height = 4, dpi = 300)
ggsave(HigherMomentPlot, file = "HigherMomentPlot.png", device = "png", width = 8, height = 4, dpi = 300)
ggsave(LatPlot, file = "LatitudePlot.png", device = "png", width = 6, height = 3, dpi = 300)
