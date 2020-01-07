rm(list = ls())

library(egstools)
setwd("/home/raphael/sim_epi")

# Collect simulation data and parameters
d <- collect()
params <- collect_params(c("hsymmetry", "ecosel", "tsave"))

# Downsample
# folders <- unique(d$id)
# nsim <- length(folders)
# ii <- sample.int(nsim, 411)
# d <- droplevels(subset(d, id %in% folders[ii]))
# params <- droplevels(as.data.frame(params[sort(ii), ]))

# Count the number of time steps
ntimesteps <- table(d$id)
stepsizes <- as.numeric(as.character(params$tsave))
d$t <- do.call(c, mapply(function(n, stepsize) {
  seq(0, n * stepsize - 1, stepsize)
}, ntimesteps, stepsizes, SIMPLIFY = FALSE))

# Append parameter values to the data
d <- cbind(d, params[rep(seq_len(nrow(params)), each = ntimesteps), ])
colnames(d) <- c("x", "y", "z", "id", "t", "hsymmetry", "ecosel")

# Axis labels
labs <- c("Ecological isolation", "Spatial isolation", "Reproductive isolation")

# Phase plane
plot_plane(d, labs = c("Time", "Ecological isolation"), yname = "x", tname = "t", splitvar = "hsymmetry", splitvar2 = "ecosel", colvar = "ecosel")




# Speciation cube
#cube <- plot_cube(d, labs, phi = 30, theta = 300)

# Heatmap
# First summarize each simulation by one value

# Possible summaries:
# Value at time t
# Average value from t1 to t2
# Time at which a threshold is passed (assuming increase)
# Rate of increase (inverse of threshold passing time)



# Then aggregate for certain parameter values
# Then plot

ds <- summarize(d)s
plot_heatmap(ds, labs = c("Habitat symmetry", "Disruptive selection"), zname = "z", xname = "hsymmetry", yname = "ecosel")
