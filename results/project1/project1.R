# Project 1
#-----------

# Exploring the effect of epistasis

rm(list = ls())
library(egstools)

# Load additive and epistatic model data
da <- collect_data("/media/raphael/bigass/simulations/EGS/additive/300 genes")
de <- collect_data("/media/raphael/bigass/simulations/EGS/epistatic/300 genes")
data <- combine_data(d = list(da, de), levs = list(Epistasis = c(0, 1)))

# Heatmaps of speciation
pe <- plot_heatmap(data, labs = c("Habitat symmetry", "Ecological selection"), zname = "EI", splitvar = "epistasis", colors = c("black", "lightgreen"), collab = "EI")
pr <- plot_heatmap(data, labs = c("Habitat symmetry", "Ecological selection"), zname = "RI", splitvar = "epistasis", colors = c("black", "lightblue"), collab = "RI")
ps <- plot_heatmap(data, labs = c("Habitat symmetry", "Ecological selection"), zname = "SI", splitvar = "epistasis", colors = c("black", "coral"), collab = "SI")

# Combine in one figure
library(ggplot2)
library(gridExtra)
ggsave("results/project1/heatmaps.png", grid.arrange(pe, pr, ps), width = 6, height = 10, dpi = 300)

# Show the simulations
plot_phase(data, tname = "t", yname = "EI", labs = c("Time", "Ecological divergence"), colvar = "Epistasis", splitvar = "ecosel", splitvar2 = "hsymmetry")
