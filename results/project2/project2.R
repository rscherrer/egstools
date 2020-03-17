# Project 3
#----------

# Adaptive dynamics analysis. Under what conditions is selection expected to lead to speciation in a two-patch asexual model?
# The model was analyzed in Mathematica
# Equilibria were derived across values of habitat symmetry and selection coefficient
# The first part plots equilibria across parameter space
# The second part plots a heatmap of evolutionary outcomes across parameter space

rm(list = ls())

library(dplyr)
library(ggplot2)

# Read the data produced by the Mathematica script
data <- read.csv("../adaptive_dynamics.csv", header = FALSE)
colnames(data) <- c("h", "s", "alpha", "m", "x0", "N1", "N2", "equilibrium", "invasibility", "convergence")

# Remove duplicate equilibrium points
data <- data[,colnames(data) != "x0"]
resolution <- 4
data[, sapply(data, is.numeric)] <- round(data[, sapply(data, is.numeric)], resolution)
data <- data %>% distinct()

# Keep only those this viable population
data <- data %>% filter(N1 > 0, N2 > 0)

smr <- data %>%
  group_by(h, s, alpha, m) %>%
  summarize(isbranching = any(invasibility > 0.001 & convergence < -0.001))

ggplot(smr, aes(x = h, y = s, fill = isbranching)) +
  geom_tile() +
  facet_grid(m ~ alpha)

# Deduce the nature of each equilibrium
data$stability <- "Repeller"
data$stability[data$convergence < 0 & data$invasibility < 0] <- "ESS"
data$stability[data$convergence > 0 & data$invasibility < 0] <- "Garden of Eden"
data$stability[data$convergence < 0 & data$invasibility > 0] <- "Branching point"



# Plot the equilibria across parameter space
p <- ggplot(data %>% filter(h %in% c(0, 0.25, 0.5, 0.75, 1), m == 0.01, alpha == 0), aes(x = s, y = equilibrium, color = stability))
p <- p + geom_point()
p <- p + theme_bw()
p <- p + ylab("Equilibrium trait value")
p <- p + xlab("Selection coefficient")
p <- p + labs(color = "")
p <- p + scale_color_manual(values = c("lightgreen", "gray30", "gray50", "gray80"))
facets <- paste("h =", levels(factor(data$h)))
names(facets) <- levels(factor(data$h))
p <- p + facet_wrap(~ h, labeller = labeller(h = facets))
p

# ggsave("results/project2/equilibria.png", p, width = 5, height = 3, dpi = 300)

head(data)

data %>% group_by(h, s, alpha, x0)

# Predict evolutionary outcomes for given starting conditions
xstart <- -1
lambda <- function(above, below, convabove, convbelow) {
  pair <- c(convabove, convbelow)
  if (any(pair == 0)) return (c(above, below)[which(pair != 0)])
  return (c(above, below)[which(pair == min(pair))])
}
d <- data %>%
  mutate(
    diff = xstart - equilibrium,
    isabove = diff < 0,
    isbelow = diff >= 0,
    distance = abs(diff)
  ) %>%
  group_by(s, h) %>%
  summarize(
    above = ifelse(any(isabove), equilibrium[distance == min(distance[isabove])], Inf),
    below = ifelse(any(isbelow), equilibrium[distance == min(distance[isbelow])], -Inf),
    convabove = ifelse(any(equilibrium == above), as.numeric(convergence[equilibrium == above] < 0), 2),
    convbelow = ifelse(any(equilibrium == below), as.numeric(convergence[equilibrium == below] < 0), 2),
    equil = lambda(above, below, convabove, convbelow),
    outcome = ifelse(equil %in% c(-Inf, Inf), "Runaway", stability[which(equilibrium == equil)[1]])
  )

# Customize outcome labels
bound <- 0.2
d$outcome[d$outcome == "ESS"] <- "Specialist"
d$outcome[d$outcome == "Specialist" & d$equil > -bound & d$equil < bound] <- "Generalist"
d$outcome <- gsub("Branching point", "Branching", d$outcome)

# Plot across parameter space
p <- ggplot(d, aes(x = hsymmetry, y = ecosel, fill = outcome))
p <- p + geom_tile()
p <- p + theme_bw()
p <- p + xlab("Habitat symmmetry")
p <- p + ylab("Selection coefficient")
p <- p + labs(fill = "")
p <- p + scale_fill_manual(values = c("lightgreen", "gray70", "gray50", "gray10"))
p

# ggsave("results/project2/outcomes_highres.png", p, width = 6, height = 5)

#### With two patches

data <- read.csv("results/project2/twopatches.csv", header = FALSE)
colnames(data) <- c("hsymmetry", "ecosel", "x1", "conv1", "inv1", "x2", "conv2", "inv2")

# Here we used two different starting points for the equilibrium search

# If they are not the same, is at least one convergent stable?
noproblem <- with(data, abs(x1 - x2) < 0.001 | abs(x1 - x2) >= 0.001 & (conv1 < 0 | conv2 < 0))
data <- data[noproblem, ]

data <- cbind(data, t(mapply(function(x1, x2, conv1, conv2, inv1, inv2) {

  if (abs(x1 - x2) < 0.001) x <- x1
  else if (conv1 > 0 & conv2 > 0) x <- NA
  else if (conv1 < 0 & conv2 < 0) x <- x1
  else if (conv1 < 0) x <- x1
  else x <- x2

  if (x == x1) inv <- inv1
  else if (x == x2) inv <- inv2
  else inv <- NA

  return(c(x = x, inv = inv))

}, data$x1, data$x2, data$conv1, data$conv2, data$inv1, data$inv2)))

p <- ggplot(data, aes(x = hsymmetry, y = ecosel, fill = inv > 0))
p <- p + geom_tile()
p
