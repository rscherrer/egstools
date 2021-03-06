% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/collect_arch.R
\name{collect_arch}
\alias{collect_arch}
\title{Extract a genetic architecture}
\usage{
collect_arch(archfile = "./architecture.txt", genomesize = 3e+06)
}
\arguments{
\item{archfile}{Path to the arhitecture file}

\item{genomesize}{Number of base pair in the genome (increases the resolution of the location of loci along the genome)}
}
\value{
A list with two data frames: one with node-specific information and the other with edge-specific information.
}
\description{
Reads and collects information from a given genetic architecture file.
}
