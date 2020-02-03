# Script to explore preliminary results
# 28-01-2020

# This script was used to make plots of speciation outcomes across parameter space
# Those plots serve as preliminary results to show Sander and Rampal

# We are here exploring the course of three batches of simulations
# Change the tag to switch from one to the other
# The data for those simulations are in /media/raphael/bigass and have the label "biased" at the end
# Because they were generated with a version of the model that overrepresented highly connected nodes
# on the first chromosome

# The tags refer to three sets of data:
# large: large genome (300 genes), high dispersal (0.01)
# large_lowdispersal: large genome (300 genes), low dispersal (0.001)
# small: small genome (30 genes), high dispersal (0.01)
# large_unbiased: large genome (300 genes), high dispersal (0.01), uniform distribution of node degrees throughout the genome

# We are plotting heatmaps of reproductive and ecological isolation at the end of the simulations
# And compare additive to epistatic maps
# We are also plotting reproductive, ecological and spatial isolation through time and against each other
# For various values of habitat symmetry and ecological selection coefficient

rm(list = ls())

library(egstools)

# Useful function
mrep <- function(vecx, vecy) do.call(c, mapply(function(x, y) rep(x, y), vecx, vecy, SIMPLIFY = FALSE))

# Change here
tag <- "_large_lowdispersal_biased" # large, large_lowdisp, small, large_unbiased

#### Extract the data of interest ####

gentypes <- c("additive", "epistatic")
sourcedir <- "/media/raphael/bigass/simulations_"
files <- c(
  "EI.dat",
  "SI.dat",
  "RI.dat",
  "varA_x.dat",
  "varA_y.dat",
  "varA_z.dat",
  "varN_x.dat",
  "varN_y.dat",
  "varN_z.dat",
  "Fst_x.dat",
  "Fst_y.dat",
  "Fst_z.dat",
  "Qst_x.dat",
  "Qst_y.dat",
  "Qst_z.dat",
  "Cst_x.dat",
  "Cst_y.dat",
  "Cst_z.dat"
)

dirs <- paste0(sourcedir, gentypes, tag)
d <- lapply(dirs, function(curr_dir) { egstools::collect(curr_dir, files) })
epistasis <- mrep(c(0, 1), sapply(d, nrow))
d <- do.call(rbind, d)
d$epistasis <- epistasis

#### Heatmaps of speciation outcome acoss parameter space ####

collabs <- c("Number of ecological\ndivergence events", "Number of reproductive\nisolation events", "Mean ecological\ndivergence", "Mean reproductive\nisolation")
colors <- c("lightgreen", "lightblue", "lightgreen", "lightblue")
znames <- c("EI", "RI", "EI", "RI")
aggregates <- c("number", "average", "number", "average")
plotnames <- c("heatmap_number_EI", "heatmap_number_RI", "heatmap_mean_EI", "heatmap_mean_RI")
plotnames <- paste0(plotnames, tag, ".pdf")

for (i in 1:4) {

  h <- plot_heatmap(
    d,
    labs = c("Habitat symmetry", "Ecological selection"),
    xname = "hsymmetry",
    yname = "ecosel",
    zname = znames[i],
    tname = "t",
    summary = "value",
    aggregate = aggregates[i],
    threshold = 0.9,
    collab = collabs[i],
    colors = c("black", colors[i]),
    splitvar = "epistasis"
  )

  ggsave(plotnames[i], h, width = 8, height = 4)

}

#### Simulation trajectories ####

xnames <- c("EI", "EI", "RI", NULL, NULL, NULL, NULL, NULL,  NULL, NULL, NULL, NULL, NULL, NULL, NULL)
ynames <- c("RI", "SI", "SI", "RI", "EI", "SI", "varA_x", "varA_y", "varA_z", "varN_x", "varN_y", "varN_z", "Fst_x", "Fst_y", "Fst_z", "Qst_x", "Qst_y", "Qst_z", "Cst_x", "Cst_y", "Cst_z")
labx <- c("Ecological divergence", "Ecological divergence", "Reproductive isolation", "Time", "Time", "Time", "Time", "Time", "Time", "Time", "Time", "Time", "Time", "Time", "Time", "Time", "Time", "Time", "Time", "Time", "Time")
laby <- c("Reproductive isolation", "Spatial isolation", "Spatial isolation", "Reproductive isolation", "Ecological isolation", "Spatial isolation", "Additive variance in ecological trait", "Additive variance in mating trait", "Additive variance in neutral trait", "Non-additive variance in ecological trait", "Non-additive variance in mating trait", "Non-additive variance in neutral trait", "Fst of the ecological trait", "Fst of the mating trait", "Fst of the neutral trait", "Qst of the ecological trait", "Qst of the mating trait", "Qst of the neutral trait", "Cst of the ecological trait", "Cst of the mating trait", "Cst of the neutral trait")
plotnames <- c("phaseplot_RI_vs_EI", "phaseplot_SI_vs_EI", "phaseplot_SI_vs_RI", "plot_RI_through_time", "plot_EI_through_time", "plot_SI_through_time", "plot_varAx_through_time", "plot_varAy_through_time", "plot_varAz_through_time", "plot_varNx_through_time", "plot_varNy_through_time", "plot_varNz_through_time", "plot_Fstx_through_time", "plot_Fsty_through_time", "plot_Fstz_through_time", "plot_Qstx_through_time", "plot_Qsty_through_time", "plot_Qstz_through_time", "plot_Cstx_through_time", "plot_Csty_through_time", "plot_Cstz_through_time")
plotnames <- paste0(plotnames, tag, ".pdf")

for (i in 1:21) {

  print(i)

  if (i <= 3) tname <- NULL else tname <- "t"
  if (i <= 3) xname <- xnames[i] else xname <- NULL

  p <- plot_phase(
    d,
    xname = xname,
    yname = ynames[i],
    tname = tname,
    labs = c(labx[i], laby[i]),
    colvar = "epistasis",
    collab = "Epistasis",
    splitvar = "ecosel",
    splitvar2 = "hsymmetry"
  )

  ggsave(plotnames[i], p, width = 7, height = 8)

}
