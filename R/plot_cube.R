#' Plot cube
#'
#' Plots simulations in a speciation cube.
#'
#' @param d Data frame with three coordinates per timepoint per simulation
#' @param labs Labels for each axis
#' @param xname Column name of the x-axis
#' @param yname Column name of the y-axis
#' @param zname Column name of the z-axis
#' @param ... Parameters to be passed to text3D
#'
#' @export

plot_cube <- function(d, labs = NULL, xname = "x", yname = "y", zname = "z", ...) {

  library(plot3D)

  xlim <- c(-0.1, 1.1)
  ylim <- c(-0.1, 1.1)
  zlim <- c(-0.1, 1.1)

  if (is.null(labs)) labs <- c("x", "y", "z")

  corners <- expand.grid(xlim, ylim, zlim)
  colnames(corners) <- c("x", "y", "z")
  corners$txt <- ""

  # Make an empty cube
  with(
    corners,
    text3D(
      get(xname),
      get(yname),
      get(zname),
      txt,
      ...,
      type = "n",
      bty = "g",
      ticktype = "detailed",
      xlim = xlim,
      ylim = ylim,
      zlim = zlim,
      xlab = labs[1],
      ylab = labs[2],
      zlab = labs[3]
      )
    )

  # Plot all the lines
  lapply(levels(d$id), function(i) {
    d <- subset(d, id == i)
    lines3D(d$x, d$y, d$z, add = TRUE, colkey = list(plot = FALSE))
  })

}
