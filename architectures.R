# Explore the features of some genetic architectures

library(GenomicRanges)
library(ggbio)
library(gridExtra)
library(tidyverse)

genomesize <- 30000000

folders <- c("architectures_disassortative", "architectures_assortative")

# For each folder...
out <- lapply(folders, function(folder) {

  print(folder)

  setwd(paste0("/media/raphael/bigass/architectures/", folder))

  archfiles <- list.files()[grep("txt$", list.files())]

  # For each architecture...
  out <- lapply(seq_along(archfiles), function(arch.id) {

    archfile <- archfiles[arch.id]
    print(archfile)

    arch <- read.delim(archfile)

    a <- 0
    b <- 0
    c <- 0
    d <- 0
    e <- 0

    for (i in seq_len(nrow(arch))) {

      isedges0 <- length(grep("edges0", arch[i,])) > 0
      isedges1 <- length(grep("edges1", arch[i,])) > 0
      isweights <- length(grep("weights", arch[i,])) > 0
      islocations <- length(grep("locations", arch[i,])) > 0
      istraits <- length(grep("traits", arch[i,])) > 0

      if (isedges0 & a == 0) {

        a <- 1
        edges0 <- as.numeric(unlist(strsplit(as.character(arch[i + 1,]), " ")))


      } else if (isedges1 & b == 0) {

        b <- 1
        edges1 <- as.numeric(unlist(strsplit(as.character(arch[i + 1,]), " ")))


      } else if (isweights & c == 0) {

        c <- 1
        weights <- as.numeric(unlist(strsplit(as.character(arch[i + 1,]), " ")))

      } else if (islocations & d == 0) {

        d <- 1
        locations <- as.numeric(unlist(strsplit(as.character(arch[i + 1,]), " ")))

      } else if (istraits & e == 0) {

        e <- 1
        traits <- as.numeric(unlist(strsplit(as.character(arch[i + 1,]), " ")))

      }
    }

    chrom <- rep(1, length(locations))
    chrom[locations > 1/3] <- 2
    chrom[locations > 2/3] <- 3

    chromlocs <- locations
    chromlocs[chrom == 2] <- chromlocs[chrom == 2] - 1/3
    chromlocs[chrom == 3] <- chromlocs[chrom == 3] - 2/3
    chromlocs <- chromlocs * genomesize

    if (any(duplicated(round(chromlocs)))) return(NULL)

    # Circular plot

    genes.gr <- GRanges(
      seqnames = Rle(paste0("chr", chrom)),
      ranges = IRanges(round(chromlocs)),
      trait = traits,
      seqlengths = c(chr1 = genomesize / 3, chr2 = genomesize / 3, chr3 = genomesize / 3)
    )

    degrees <- rep(0, length(locations))
    degrees[as.numeric(names(table(c(edges0, edges1)))) + 1] <- table(c(edges0, edges1))
    genes.gr$degree <- degrees

    edges.gr <- genes.gr[edges0 + 1]
    edges.gr$to.gr <- genes.gr[edges1 + 1]
    edges.gr$weight <- weights
    edges.gr$absweight <- abs(weights)
    edges.gr$signweight <- as.character(sign(weights))

    p <- ggbio() +
      circle(edges.gr, geom = "ideo", fill = "gray70") +
      circle(edges.gr, geom = "link", linked.to = "to.gr", radius = 30, aes(color = signweight, alpha = absweight)) +
      circle(genes.gr, geom = "bar", aes(y = degree)) +
      theme(legend.position = "none")

    # Nodes data

    nodes.msqedges <- sapply(seq_along(degrees), function(i) {
      if (degrees[i] == 0) return (0)
      0.5 * (sum(weights[edges0 == i]^2) + sum(weights[edges1 == i]^2)) / degrees[i]
    })

    node.df <- data.frame(
      id = seq_along(degrees),
      degree = degrees,
      location = locations,
      trait = traits,
      msqedge = nodes.msqedges,
      arch = arch.id
    )

    # Edge data

    edges.dist <- sapply(seq_along(weights), function(i) {
      y <- abs(locations[edges0[i]] - locations[edges1[i]])
      y <- ifelse(length(y) == 0, 0, y)
    })

    edges.middeg <- sapply(seq_along(weights), function(i) {
      y <- (degrees[edges0[i]] + degrees[edges1[i]]) / 2
      y <- ifelse(length(y) == 0, 0, y)
    })

    edges.maxdeg <- sapply(seq_along(weights), function(i) {
      max(c(degrees[edges0[i]], degrees[edges1[i]]))
    })

    edge.df <- data.frame(
      id = seq_along(weights),
      weight = weights,
      vtx0 = edges0,
      vtx1 = edges1,
      distance = edges.dist,
      middegree = edges.middeg,
      maxdegree = edges.maxdeg,
      arch = arch.id
    )

    # Package the output

    out <- list(
      plot = p,
      nodes = node.df,
      edges = edge.df
    )

    return(out)

  })

  return(out)

})

setwd("..")

matings <- c("disassortative", "assortative")
names(out) <- matings

# Save the plots

pd <- lapply(out$disassortative, function(x) if (!is.null(x)) x$plot)
pa <- lapply(out$assortative, function(x) if (!is.null(x)) x$plot)

for (i in 1:33) {

  pdf(paste0("../architectures_assortative/architecture_", i, ".pdf"), width = 5, height = 5)
  print(pa[[i]])
  dev.off()

  pdf(paste0("../architectures_disassortative/architecture_", i, ".pdf"), width = 5, height = 5)
  print(pd[[i]])
  dev.off()

}

# Extract data frames

nodes <- do.call("rbind", lapply(seq_along(out), function(i) {
  res <- do.call("rbind", lapply(out[[i]], function(out) out$nodes))
  res$mating <- matings[i]
  return (res)
}))

write.csv(nodes, "nodes.csv")

edges <- do.call("rbind", lapply(seq_along(out), function(i) {
  res <- do.call("rbind", lapply(out[[i]], function(out) out$edges))
  res$mating <- matings[i]
  return (res)
}))

write.csv(edges, "edges.csv")

# Compare degree distributions

nodes <- nodes %>% group_by(mating, arch) %>% mutate(sortdeg = rev(sort(degree)))

p <- ggplot(data = nodes, aes(x = id, y = sortdeg, color = mating))
p <- p + geom_line(alpha = 0.7)
p <- p + theme_bw()
p <- p + ylab("Degree")
p <- p + xlab("Rank")
p <- p + labs(color = "Mating type")
p

# No difference

# Compare distribution of weights

edges.cw <- edges %>%
  group_by(mating, arch) %>%
  do(data.frame(sapply(seq(0, 1, 0.01), function(p) quantile(.$weight, p))))

colnames(edges.cw)[3] <- "quantile"

edges.cw <- edges.cw %>%
  group_by(mating, arch) %>%
  mutate(prob = seq(0, 1, 0.01)) %>%
  ungroup()

edges.cw$arch <- as.factor(paste(edges.cw$mating, edges.cw$arch, sep = "_"))
ncolors <- nlevels(edges.cw$arch)

p <- ggplot(data = edges.cw, aes(x = prob, y = quantile, color = arch))
p <- p + geom_line()
p <- p + facet_grid(. ~ mating)
p <- p + scale_color_manual(values = gray(runif(ncolors, 0, 0.5)))
p <- p + theme_bw()
p <- p + theme(legend.position = "none")
p <- p + xlab("Probability")
p <- p + ylab("Weight quantile")
p

# More asymmetries in weight distributions in the disassortative case

# Distribution of pairwise distances between interacting partners

edges.cd <- edges %>%
  group_by(mating, arch) %>%
  do(data.frame(sapply(seq(0, 1, 0.01), function(p) quantile(.$distance, p))))

colnames(edges.cd)[3] <- "quantile"

edges.cd <- edges.cd %>%
  group_by(mating, arch) %>%
  mutate(prob = seq(0, 1, 0.01)) %>%
  ungroup()

edges.cd$arch <- as.factor(paste(edges.cd$mating, edges.cd$arch, sep = "_"))
ncolors <- nlevels(edges.cd$arch)

p <- ggplot(data = edges.cd, aes(x = prob, y = quantile, color = mating, alpha = arch))
p <- p + geom_line()
p <- p + scale_alpha_manual(values = runif(ncolors, 0.7, 1))
p <- p + theme_bw()
p <- p + xlab("Probability")
p <- p + ylab("Distance quantile")
p <- p + guides(alpha = FALSE)
p <- p + labs(color = "Mating type")
p

# No difference

# Is there a correlation between the weights and the distances?

edges.avg <- edges %>%
  group_by(mating, arch) %>%
  summarize(mweight = mean(abs(weight)), mdist = mean(distance), vweight = var(abs(weight)), vdist = var(distance))

# Means
p <- ggplot(edges.avg, aes(x = mdist, y = mweight, color = mating))
p <- p + geom_point()
p <- p + theme_bw()
p <- p + xlab("Mean pairwise distance")
p <- p + ylab("Mean weight")
p <- p + labs(color = "Mating type")
p

# Variances
p <- ggplot(edges.avg, aes(x = vdist, y = vweight, color = mating))
p <- p + geom_point()
p <- p + theme_bw()
p <- p + xlab("Variance in pairwise distance")
p <- p + ylab("Variance in weight")
p <- p + labs(color = "Mating type")
p

# Still nothing. Then what?
