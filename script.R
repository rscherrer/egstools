rm(list = ls())

library(egstools)
setwd("/home/raphael/sim_epi")

# Collect simulation data and parameters
d <- collect()

# Downsample
# folders <- unique(d$id)
# nsim <- length(folders)
# ii <- sample.int(nsim, 411)
# d <- droplevels(subset(d, id %in% folders[ii]))
# params <- droplevels(as.data.frame(params[sort(ii), ]))



# Phase plane
plot_plane(
  d,
  yname = "x",
  tname = "t",
  labs = c("Time", "Ecological isolation"),
  colvar = "ecosel",
  splitvar = "ecosel",
  splitvar2 = "hsymmetry",
)



plot_heatmap(
  d,
  labs = c("Habitat symmetry", "Ecological selection"),
  xname = "hsymmetry",
  yname = "ecosel",
  zname = "z",
  tname = "t",
  summary = c("value", "average", "threshold"),
  tval = NULL,
  trange = NULL,
  theta = 0.9,
  colname = NULL,
  aggregate = "average",
  prob = 0.95,
  threshold = 0.9
)




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
