rm(list = ls())

library(egstools)

# Collect simulation data and parameters

# If I want to merge data from different batches of simulations
d1 <- egstools::collect(
  dir = "/home/raphael/simulations_add",
  files = c("EI.dat", "varA_x.dat")
)
d2 <- egstools::collect(
  dir = "/home/raphael/simulations_epi",
  files = c("EI.dat", "varA_x.dat")
)

d1$epistasis <- 0
d2$epistasis <- 1

# Merge the data frames
d <- rbind(d1, d2)

# Phase plane
plot_phase(
  d,
  yname = "varA_x",
  tname = "t",
  labs = c("Time", "Ecological isolation"),
  colvar = "epistasis",
  collab = "Epistasis",
  splitvar = "ecosel",
  splitvar2 = "hsymmetry"
)

plot_heatmap(
  d,
  labs = c("Habitat symmetry", "Ecological selection"),
  xname = "hsymmetry",
  yname = "ecosel",
  zname = "RI",
  tname = "t",
  summary = "value",
  aggregate = "average",
  collab = "Reproductive isolation",
  colors = c("black", "yellow"),
  splitvar = "epistasis"
)


############

# Speciation cube
#cube <- plot_cube(d, labs, phi = 30, theta = 300)
