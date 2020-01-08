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



# We want to summarize each simulation by one value



summ_average(d, trange = c(200, 2000))
summ_value(d, tval = 2000)

s <- summ_threshold(d)
head(cbind(s, params))

# Now we want to make heatmaps from those reduced data sets
# We need three dimensions: x, y and z
# And we need to choose a function to aggregate replicates

# The agggregation function could be:
# Average across replicates
# Variance across replicates
# Some quantile across replicates
# The number of replicates above a certain threshold

# Summarized data
dsum <- summ_value(d, sname = "z", tval = 19900)
dsum <- cbind(dsum, params)

# Aggregate across replicates
dred <- aggregate_number(dsum)



# Plot the heatmap
p <- ggplot(data = dred, aes(x = hsymmetry, y = ecosel, fill = Z)) +
  geom_tile() +
  xlab(xlab) +
  ylab(ylab)
p

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

ds <- summarize(d)
plot_heatmap(ds, labs = c("Habitat symmetry", "Disruptive selection"), zname = "z", xname = "hsymmetry", yname = "ecosel")
