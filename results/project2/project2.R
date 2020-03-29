# Project 3
#----------

# Adaptive dynamics analysis. Under what conditions is selection expected to lead to speciation in a two-patch asexual model?
# The model was analyzed in Mathematica
# Equilibria were derived across values of habitat symmetry and selection coefficient
# The first part plots equilibria across parameter space
# The second part plots a heatmap of evolutionary outcomes across parameter space

rm(list = ls())

library(dplyr)
library(tidyr)
library(ggplot2)
library(gridExtra)

# Read the data produced by the Mathematica script
data <- read.csv("/home/raphael/adaptive_dynamics1.csv", header = FALSE)
colnames(data) <- c("h", "s", "alpha", "m", "x0", "N1", "N2", "equilibrium", "invasibility", "convergence")

data_raw <- data

sapply(data, function(x) any(is.na(x)))

x0 <- -1

smr <- data %>%
  filter(N1 >= 0, N2 >= 0, equilibrium <= 1, equilibrium >= -1) %>%
  distinct() %>%
  group_by(h, s, alpha, m) %>%
  mutate(
    isconv = convergence < 0,
    isinv = invasibility > 0,
    isbp = isconv & isinv,
    isess = isconv & !isinv,
    isrep = !isconv,
    distance = abs(equilibrium - x0)
  ) %>%
  summarize(
    closest = min(which(distance == min(distance))),
    isclosestconv = isconv[closest],
    isclosestbp = ifelse(isclosestconv, isinv[closest], FALSE),
    isanyopposite = any(sapply(equilibrium, function(x) sign(x - x0) != sign(equilibrium[closest] - x0))),
    distopposite = ifelse(isanyopposite, min(sapply(equilibrium, function(x) ifelse(sign(x - x0) != sign(equilibrium[closest] - x0), abs(x - x0), NA)), na.rm = TRUE), NA),
    opposite = ifelse(isanyopposite, min(which(sapply(equilibrium, function(x) abs(x - x0) == distopposite & sign(x - x0) != sign(equilibrium[closest] - x0)))), NA),
    isoppositeconv = isconv[opposite],
    isoppositebp = ifelse(isoppositeconv, isinv[opposite], FALSE),
    bp = isclosestbp | !isclosestconv & isanyopposite & isoppositebp,
    isanyinv = any(isinv > 0)
  )

ggplot(smr, aes(x = h, y = s, fill = isclosestbp)) + geom_tile() + facet_grid(alpha ~ m)

# Remove duplicate equilibrium points
data <- data[, colnames(data) != "x0"]
resolution <- 10
data[, sapply(data, is.numeric)] <- round(data[, sapply(data, is.numeric)], resolution)
data <- data %>% distinct()

# Keep only those with viable population
data <- data %>% filter(N1 > 0, N2 > 0)

head(data)

length(which(is.na(data$N1)))
nrow(data)

smr <- data %>%
  group_by(h, s, alpha, m) %>%
  summarize(
    anyconv = any(convergence < 0),
    anybp = any(invasibility > 0),
    anyess = any(invasibility < 0),
    meaninv = mean(invasibility),
    nconv = length(which(convergence < 0)),
    anybpoutside = any(convergence < 0 & invasibility > 0 & equilibrium >= 0.01 & equilibrium <= -0.001),
    nbp = length(which(convergence < 0 & invasibility > 0))
  )

ggplot(smr, aes(x = h, y = s, fill = anybp)) + geom_tile() + facet_grid(alpha ~ m)

head(smr)

data <- rbind(data, do.call(rbind, lapply(c(-Inf, Inf), function(x) {
  cbind(
    data %>% expand(h, s, alpha, m),
    data.frame(
      N1 = NA,
      N2 = NA,
      equilibrium = x,
      invasibility = NA,
      convergence = -1
    )
  )
})))

xstart <- -1.1

smr <- data %>%
  group_by(h, s, alpha, m) %>%
  mutate(
    isbelow = equilibrium < xstart,
    isabove = equilibrium > xstart
  ) %>%
  summarize(
    mindist.a = min(abs(xstart - equilibrium[isabove])),
    closest.a = min(which(abs(xstart - equilibrium) == mindist.a & isabove)),
    equil.a = equilibrium[closest.a],
    conv.a = convergence[closest.a],
    mindist.b = min(abs(xstart - equilibrium[isbelow])),
    closest.b = min(which(abs(xstart - equilibrium) == mindist.b & isbelow)),
    equil.b = equilibrium[closest.b],
    conv.b = convergence[closest.b],
    closest = min(which(c(mindist.a, mindist.b) == min(mindist.a, mindist.b))),
    equil = ifelse(c(conv.a, conv.b)[closest] < 0, c(equil.a, equil.b)[closest], c(equil.a, equil.b)[-closest]),
    N1 = N1[min(which(equilibrium == equil))],
    N2 = N2[min(which(equilibrium == equil))],
    inv = invasibility[min(which(equilibrium == equil))],
    conv = convergence[min(which(equilibrium == equil))],
    anyconv = any(conv < 0),
    nconv = length(which(conv < 0))
  ) %>%
  select(h, s, alpha, m, N1, N2, equil, inv, conv, anyconv, nconv)

# How many equilibria?
ggplot(smr, aes(x = h, y = s, fill = nconv)) +
  geom_tile() +
  facet_grid(m ~ alpha)

data_raw %>% filter(h == 0.5, s == 1, alpha == 0, m == 0.001)

#------------------

pip <- read.csv("../pip.csv", header = FALSE)
pip <- data.frame(sapply(pip, round, 6))
colnames(pip) <- c("y", "x", "N1", "N2", "lambda")

head(pip)

p1 <- ggplot(pip, aes(x = x, y = y, fill = lambda > 1)) + geom_tile() + scale_fill_manual(values = c("black", "grey70"))

diagonal <- pip %>%
  group_by(x) %>%
  summarize(
    below = ifelse(any(y == x - 0.01), lambda[y == x - 0.01], NA),
    at = lambda[y == x],
    above = ifelse(any(y == x + 0.01), lambda[y == x + 0.01], NA)
  ) %>%
  mutate(
    diff = above - below
  )

p2 <- ggplot(diagonal, aes(x = x, y = diff)) + geom_line() + xlim(c(-0.1, 0.1))

p3 <- ggplot(pip %>% filter(x == -0.5), aes(x = y - x, y = lambda)) + geom_line()
p4 <- ggplot(pip %>% filter(x == 0), aes(x = y - x, y = lambda)) + geom_line()

grid.arrange(p1, p2, p3, p4)

#------------------
