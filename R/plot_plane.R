#' Plot speciation phase plane
#'
#' Plots simulations in a phase plane.
#'
#' @param d Data frame with coordinates per timepoint per simulation
#' @param colvar What variable to color according to
#' @param labs Labels for each axis
#' @param xname Column name of the x-axis
#' @param yname Column name of the y-axis
#' @param splitvar Facet splitting variable
#'
#' @export

plot_plane <- function(d, colvar = NULL, labs = NULL, xname = "x", yname = "y", splitvar = NULL) {

  library(ggplot2)

  if (is.null(labs)) labs <- c("x", "y")

  d$col <- d$id
  if (!is.null(colvar)) d$col <- as.factor(d[, colvar])

  colorset <- colorRampPalette(c("black", "lightgrey"))
  ncolors <- nlevels(d$col)

  p <- ggplot(data = d, aes(x = get(xname), y = get(yname), color = col, alpha = id)) +
    geom_line() +
    theme_bw() +
    scale_color_manual(values = colorset(ncolors)) +
    scale_alpha_manual(values = runif(nlevels(d$id), 0.7, 1), guide = FALSE) +
    labs(color = colvar) +
    xlim(c(-0.1, 1.1)) +
    ylim(c(-0.1, 1.1)) +
    xlab(labs[1]) +
    ylab(labs[2])

  if (!is.null(splitvar)) p <- p + facet_wrap(~get(splitvar))
  if (is.null(colvar)) p <- p + theme(legend.position = "none")

  return(p)

}
