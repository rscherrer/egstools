#' Plot speciation heatmap
#'
#' Summarizes simulation metrics in a heatmap.
#'
#' @param d Data frame with coordinates per timepoint per simulation
#' @param labs Labels for each axis
#' @param zname Column name of the heat variable
#' @param xname Column name of the x-axis
#' @param yname Column name of the y-axis
#' @param splitvar Facet splitting variable
#' @param splitvar2 Second facet splitting variable
#'
#' @export

plot_heatmap <- function(d, zname, labs = NULL, xname = "x", yname = "y", splitvar = NULL, splitvar2 = NULL) {

  library(ggplot2)

  if (is.null(labs)) labs <- c("x", "y")

  if (!is.null(splitvar)) d[, splitvar] <- as.factor(paste(splitvar, '=', d[, splitvar]))
  if (!is.null(splitvar2)) d[, splitvar2] <- as.factor(paste(splitvar2, '=', d[, splitvar2]))

  p <- ggplot(data = d, aes(x = get(xname), y = get(yname), color = col, alpha = id)) +
    geom_line() +
    theme_bw() +
    scale_color_manual(values = colorset(ncolors)) +
    scale_alpha_manual(values = runif(nlevels(d$id), 0.7, 1), guide = FALSE) +
    labs(color = colvar) +
    xlim(xlim) +
    ylim(ylim) +
    xlab(labs[1]) +
    ylab(labs[2])

  if (!is.null(splitvar)) {
    if (!is.null(splitvar2)) {
      p <- p + facet_wrap(get(splitvar) ~ get(splitvar2))
    } else {
      p <- p + facet_wrap(~get(splitvar))
    }
  }
  if (is.null(colvar)) p <- p + theme(legend.position = "none")

  return(p)

}
