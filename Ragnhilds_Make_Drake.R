library(drake)

r_make(source = "R/RagnhildsDrakePlan.R")

failed()

#view dependency graph
r_vis_drake_graph(source = "R/Gradient-analysis-with-drake.R", targets_only = TRUE)
