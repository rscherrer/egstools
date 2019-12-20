rm(list = ls())

setwd("/home/raphael/EGS_sim")

# Collect simulation data and parameters
d <- collect()
params <- collect_params(c("hsymmetry", "ecosel", "tsave"))

# Downsample
folders <- unique(d$id)
nsim <- length(folders)
ii <- sample.int(nsim, 411)
d <- droplevels(subset(d, id %in% folders[ii]))
params <- droplevels(as.data.frame(params[sort(ii), ]))

# Count the number of time steps
ntimesteps <- table(d$id)[1]
step <- as.numeric(as.character(params$tsave[1]))
timesteps <- step * (seq_len(ntimesteps) - 1)
d$t <- rep(timesteps, nlevels(d$id))

# Append parameter values to the data
d <- cbind(d, params[rep(seq_len(nrow(params)), each = ntimesteps), ])
colnames(d) <- c("x", "y", "z", "id", "t", "hsymmetry", "ecosel")

# Axis labels
labs <- c("Ecological isolation", "Spatial isolation", "Reproductive isolation")

# Phase plane
plot_plane(d, labs = c("Time", "Ecological isolation"), yname = "x", tname = "t", splitvar = "hsymmetry", splitvar2 = "ecosel", colvar = "ecosel")

# Speciation cube
cube <- plot_cube(d, labs, phi = 30, theta = 300)
