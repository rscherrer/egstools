rm(list = ls())

setwd("/home/raphael/EGS_sim")

# Collect simulation data and parameters
d <- collect()
params <- collect_params(c("hsymmetry", "ecosel"))

# Downsample
folders <- unique(d$id)
nsim <- length(folders)
ii <- sample.int(nsim, 10)
d <- droplevels(subset(d, id %in% folders[ii]))
params <- droplevels(as.data.frame(params[sort(ii), ]))

# Count the number of time steps
ntimesteps <- table(d$id)[1]

# Append parameter values to the data
d <- cbind(d, params[rep(seq_len(nrow(params)), each = ntimesteps), ])
colnames(d)[5] <- "hsymmetry"
colnames(d)[6] <- "ecosel"

# Axis labels
labs <- c("Ecological isolation", "Spatial isolation", "Reproductive isolation")

# Phase plane
plot_plane(d, labs = labs[1:2], colvar = "hsymmetry", splitvar = "hsymmetry")

# Speciation cube
cube <- plot_cube(d, labs, phi = 30, theta = 300)



