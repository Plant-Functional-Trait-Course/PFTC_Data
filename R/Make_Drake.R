library(drake)

r_make(source = "Gradient-analysis-with-drake.R")

failed()

#view dependency graph
r_vis_drake_graph(source = "Gradient-analysis-with-drake.R", targets_only = TRUE)
