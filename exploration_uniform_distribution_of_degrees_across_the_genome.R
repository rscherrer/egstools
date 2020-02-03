# Here we check out the simulations that ran with the new unbiased model
# Unbiased, in the sense that genes with higher degrees are not overrepresented on the first chromosome
# (nodes of the network are reshuffled before being assigned position)
# Simulations were run with 100 loci for each trait and high dispersal (0.01)
# Raphael Scherrer 29/1/2020

rm(list = ls())

library(egstools)
library(ggplot2)

mrep <- function(vecx, vecy) do.call(c, mapply(function(x, y) rep(x, y), vecx, vecy, SIMPLIFY = FALSE))

sourcedir <- "/media/raphael/bigass/"
add_dir <- "simulations_additive"
epi_dir <- "simulations_epistatic"
tag <- "_large"
add_dir <- paste0(sourcedir, add_dir, tag)
epi_dir <- paste0(sourcedir, epi_dir, tag)

d_add <- collect(add_dir, files = c("EI.dat", "RI.dat", "SI.dat"))
d_epi <- collect(epi_dir, files = c("EI.dat", "RI.dat", "SI.dat"))

epistasis <- as.factor(mrep(c(0, 1), c(nrow(d_add), nrow(d_epi))))
d <- rbind(d_add, d_epi)
d$epistasis <- epistasis

#### Outcome of the simulations across parameter space ####

# Use the following snippet to plot various outcome statistics
# such as EI or RI

h <- plot_heatmap(
  d,
  labs = c("Habitat symmetry", "Ecological selection"),
  xname = "hsymmetry",
  yname = "ecosel",
  zname = "RI",
  tname = "t",
  summary = "value",
  aggregate = "average",
  threshold = 0.9,
  collab = "Mean reproductive\nisolation",
  colors = c("black", "lightblue"),
  splitvar = "epistasis"
)

h

#### Simulation trajectories ####

p <- plot_phase(
  d,
  xname = "EI",
  yname = "RI",
  #tname = tname,
  labs = c("Ecological divergence", "Reproductive isolation"),
  colvar = "epistasis",
  collab = "Epistasis",
  splitvar = "ecosel",
  splitvar2 = "hsymmetry"
)

p

# The results are very similar to the biased model!
# We still observe cases of disassortative mating
# So, the evolution of disassortative mating may not be due to the biased spatial distribution of node degrees throughout the genome

#### Check that the distribution of nodes throughout the genome is indeed unbiased ####

simfolder <- list.files(epi_dir)[grep("sim_", list.files(epi_dir))][1] # example simulation
archfile <- paste0(epi_dir, "/", simfolder, "/architecture.txt")
arch <- collect_arch(archfile)

p <- ggplot(arch$nodes, aes(x = locations, y = degree))
p <- p + geom_line()
p <- p + theme_bw()
p <- p + xlab("Location")
p <- p + ylab("Degree")
p

# The degrees are uniformly distributed throughout the genome!

