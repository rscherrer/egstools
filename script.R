rm(list = ls())

library(egstools)

# Collect simulation data and parameters

# If I want to merge data from different batches of simulations
dirs <- paste0("/home/raphael/simulations_", c("additive", "epistatic"), "_large")
files <- c(
  "EI.dat",
  "SI.dat",
  "RI.dat",
  "varA_x.dat",
  "varI_x.dat",
  "varN_x.dat",
  "Fst_x.dat",
  "Fst_y.dat",
  "Fst_z.dat"
)

d <- lapply(dirs, function(curr_dir) {
  egstools::collect(
    dir = curr_dir,
    files = files
  )
})

mrep <- function(vecx, vecy) {

  do.call(c, mapply(function(x, y) rep(x, y), vecx, vecy, SIMPLIFY = FALSE))

}

epistasis <- mrep(c(0, 1), sapply(d, nrow))
d <- do.call(rbind, d)
d$epistasis <- epistasis

# Or just additive simulations
# d <- egstools::collect(dir = "/home/raphael/simulations_additive_large", files = files)

# Phase plane
p <- plot_phase(
  d,
  xname = "EI",
  yname = "RI",
  #tname = "t",
  labs = c("Ecological divergence", "Reproductive isolation"),
  colvar = "epistasis",
  collab = "Epistasis",
  splitvar = "ecosel",
  splitvar2 = "hsymmetry",
  xline = TRUE,
  yline = TRUE
)

p
ggsave("ERI.pdf", p)

# Heatmap
h <- plot_heatmap(
  d,
  labs = c("Habitat symmetry", "Ecological selection"),
  xname = "hsymmetry",
  yname = "ecosel",
  zname = "RI",
  tname = "t",
  summary = "value",
  aggregate = "average",
  collab = "Reproductive isolation",
  colors = c("black", "lightblue"),
  splitvar = "epistasis"
)

ggsave("heatmap_RI.pdf", h)


# Display a gene network?
#


############

# Speciation cube
#cube <- plot_cube(d, labs, phi = 30, theta = 300)
