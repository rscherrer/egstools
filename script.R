rm(list = ls())

setwd("/home/raphael/EGS_sim")

d <- collect()
params <- collect_params()

folders <- unique(d$id)
nsim <- length(folders)
ii <- sample.int(nsim, 10)

d <- droplevels(subset(d, id %in% folders[ii]))
params <- droplevels(params[sort(ii), ])

ntimesteps <- table(d$id)[1]
d$hsymmetry <- rep(params$hsymmetry, each = ntimesteps)

labs <- c("Ecological isolation", "Spatial isolation", "Reproductive isolation")

# cube <- plot_cube(d, labs, phi = 30, theta = 300)

plot_plane(d, labs = labs[1:2], colvar = "hsymmetry")

# Make it split by values of some parameter?
