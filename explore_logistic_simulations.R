# Here we explore the results of the simulations across parameter space
# For a model implementation where the resource dynamics is logistic
# as per Ripa et al. instead of chemostat-like
# We compare both
# 03-02-2020
# Raphaël Scherrer

rm(list = ls())

library(egstools)

dir <- "/media/raphael/bigass/simulations_logistic"
files <- c("EI.dat", "SI.dat", "RI.dat")

d <- egstools::collect(dir, files)

plot_phase(
  d,
  #xname = "RI",
  yname = "RI",
  tname = "t",
  splitvar = "ecosel",
  splitvar2 = "hsymmetry",
  labs = c("Time", "Reproductive isolation")
)

plot_heatmap(
  d,
  labs = c("Habitat symmetry", "Ecological selection"),
  xname = "hsymmetry",
  yname = "ecosel",
  zname = "EI",
  tname = "t",
  summary = "value",
  aggregate = "average",
  colors = c("black", "lightgreen"),
  collab = "Ecological isolation"
)

