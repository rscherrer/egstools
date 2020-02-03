#' Extract a genetic architecture
#'
#' Reads and collects information from a given genetic architecture file.
#'
#' @param archfile Path to the arhitecture file
#' @param genomesize Number of base pair in the genome (increases the resolution of the location of loci along the genome)
#'
#' @return A list with two data frames: one with node-specific information and the other with edge-specific information.
#'
#' @export

collect_arch <- function(archfile = "./architecture.txt", genomesize = 3000000) {

  arch <- read.delim(archfile)

  a <- 0
  b <- 0
  c <- 0
  d <- 0
  e <- 0
  f <- 0

  for (i in seq_len(nrow(arch))) {

    isedges0 <- length(grep("edges0", arch[i,])) > 0
    isedges1 <- length(grep("edges1", arch[i,])) > 0
    isweights <- length(grep("weights", arch[i,])) > 0
    islocations <- length(grep("locations", arch[i,])) > 0
    istraits <- length(grep("traits", arch[i,])) > 0
    ischromends <- length(grep("chromosomes", arch[i,])) > 0

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

    } else if (ischromends & f == 0) {

      f <- 1
      chromends <- as.numeric(unlist(strsplit(as.character(arch[i + 1,]), " ")))

    }
  }

  # What chromosome do each locus belong to?
  chrom <- sapply(locations, function(loc) min(which(loc < chromends)))

  # Size of each chromosome
  chromsizes <- sapply(seq_along(chromends), function(i) {
    chrombegin <- ifelse(i == 1, 0, chromends[i - 1])
    return (chromends[i] - chrombegin)
  })

  # Within-chromosome locations
  chromlocs <- sapply(seq_along(locations), function(i) 1 - (chromends[chrom[i]] - locations[i]) / chromsizes[chrom[i]] )
  chromlocs <- chromlocs * genomesize

  if (any(duplicated(round(chromlocs)))) {
    warning("Some loci have identical locations, try increasing the genome size")
    return(NULL)
  }

  #### Circular plot ####

  # genes.gr <- GRanges(
  #   seqnames = Rle(paste0("chr", chrom)),
  #   ranges = IRanges(round(chromlocs)),
  #   trait = traits,
  #   seqlengths = c(chr1 = genomesize / 3, chr2 = genomesize / 3, chr3 = genomesize / 3)
  # )
  #
  degrees <- rep(0, length(locations))
  degrees[as.numeric(names(table(c(edges0, edges1)))) + 1] <- table(c(edges0, edges1))
  # genes.gr$degree <- degrees
  #
  # edges.gr <- genes.gr[edges0 + 1]
  # edges.gr$to.gr <- genes.gr[edges1 + 1]
  # edges.gr$weight <- weights
  # edges.gr$absweight <- abs(weights)
  # edges.gr$signweight <- as.character(sign(weights))
  #
  # p <- ggbio() +
  #   circle(edges.gr, geom = "ideo", fill = "gray70") +
  #   circle(edges.gr, geom = "link", linked.to = "to.gr", radius = 30, aes(color = signweight, alpha = absweight)) +
  #   circle(genes.gr, geom = "bar", aes(y = degree)) +
  #   theme(legend.position = "none")

  #### Node data ####

  # Mean squared weight of the edges each node is involved in
  nodes.msqedges <- sapply(seq_along(degrees), function(i) {
    if (degrees[i] == 0) return (0)
    0.5 * (sum(weights[edges0 == i]^2) + sum(weights[edges1 == i]^2)) / degrees[i]
  })

  # Assemble the data frame
  node.df <- data.frame(
    id = seq_along(degrees),
    degree = degrees,
    location = locations,
    trait = traits,
    msqedge = nodes.msqedges
  )

  #### Edge data ####

  # Genetic distance between the nodes
  edges.dist <- sapply(seq_along(weights), function(i) {
    y <- abs(locations[edges0[i]] - locations[edges1[i]])
    y <- ifelse(length(y) == 0, 0, y)
  })

  # Midpoint between the degrees of the two interacting nodes
  edges.middeg <- sapply(seq_along(weights), function(i) {
    y <- (degrees[edges0[i]] + degrees[edges1[i]]) / 2
    y <- ifelse(length(y) == 0, 0, y)
  })

  # Degree of the most connected partner
  edges.maxdeg <- sapply(seq_along(weights), function(i) {
    max(c(degrees[edges0[i]], degrees[edges1[i]]))
  })

  # Assemble the data frame
  edge.df <- data.frame(
    id = seq_along(weights),
    weight = weights,
    vtx0 = edges0,
    vtx1 = edges1,
    distance = edges.dist,
    middegree = edges.middeg,
    maxdegree = edges.maxdeg
  )

  # Package the output
  return (list(nodes = node.df, edges = edge.df))

}
