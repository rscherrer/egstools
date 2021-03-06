% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/aggregate_variance.R
\name{aggregate_variance}
\alias{aggregate_variance}
\title{Aggregate replicate simulations}
\usage{
aggregate_variance(
  d,
  xname = "hsymmetry",
  yname = "ecosel",
  zname = "summary",
  colname = NULL,
  keep = NULL
)
}
\arguments{
\item{d}{Data frame with coordinates per timepoint per simulation}

\item{xname}{Column name of the first grouping parameter}

\item{yname}{Column name of the second grouping parameter (set to NULL if only one grouping parameter)}

\item{zname}{Column name of the statistic to aggregate}

\item{colname}{Optional name of the aggregated statistic column}

\item{keep}{Optional names of columns to keep in the aggregated data frame}
}
\description{
Aggregate a statistic down to its variance over replicates within values of one or two parameters.
}
