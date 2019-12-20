rm(list = ls())

setwd("/home/raphael/EGS_sim")

d <- collect()

folders <- unique(d$id)
nsim <- length(folders)
ii <- sample.int(nsim, 10)
d <- droplevels(subset(d, id %in% folders[ii]))

labs <- c("Ecological isolation", "Spatial isolation", "Reproductive isolation")
cube <- plot_cube(d, labs, phi = 30, theta = 300)

d$extra <- "A"
plot_plane(d, labs = labs[1:2], splitvar = "extra")

# Make it split by values of some parameter?
