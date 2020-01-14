#' Plot speciation phase plane
#'
#' Plots simulations in a phase plane. If tname is provided, plots y against time.
#'
#' @param d Data frame with coordinates per timepoint per simulation
#' @param xname Column name of the x-axis
#' @param yname Column name of the y-axis
#' @param tname Optional time column
#' @param labs Labels for each axis
#' @param colvar What variable to color according to
#' @param collab Optional color legend title
#' @param splitvar Facet splitting variable
#' @param splitvar2 Second facet splitting variable
#' @param xlim Boundaries of the x-axis
#' @param ylim Boundaries of the y-axis
#' @param xline Whether to add a vertical line at the initial point
#' @param yline Whether to add a horizontal line at the initial point
#'
#' @export

plot_phase <- function(
  d,
  xname = "x",
  yname = "y",
  tname = NULL,
  labs = NULL,
  colvar = NULL,
  collab = NULL,
  splitvar = NULL,
  splitvar2 = NULL,
  xlim = NULL,
  ylim = NULL,
  xline = FALSE,
  yline = FALSE
) {

  library(ggplot2)

  if (is.null(labs)) labs <- c("x", "y")

  d$col <- d$id
  if (!is.null(colvar)) d$col <- as.factor(d[, colvar])

  colorset <- colorRampPalette(c("lightgrey", "black"))
  ncolors <- nlevels(d$col)

  if (!is.null(tname)) xname <- tname

  if (!is.null(tname)) xlim <- c(min(d[, tname]), max(d[, tname]))

  # Make the plot
  p <- ggplot(data = d, aes(x = get(xname), y = get(yname), color = col, alpha = id)) +
    geom_line() +
    theme_bw() +
    scale_color_manual(values = colorset(ncolors)) +
    scale_alpha_manual(values = runif(nlevels(d$id), 0.7, 1), guide = FALSE) +
    xlab(labs[1]) +
    ylab(labs[2])

  if (!is.null(xlim)) p <- p + xlim(xlim)
  if (!is.null(ylim)) p <- p + ylim(ylim)

  if (!is.null(splitvar)) {
    if (!is.null(splitvar2)) {
      p <- p + facet_grid(get(splitvar) ~ get(splitvar2))
    } else {
      p <- p + facet_wrap(~get(splitvar))
    }
  }

  if (is.null(colvar)) {
    p <- p + theme(legend.position = "none")
  } else {
    if (is.null(collab)) collab <- colvar
    p <- p + labs(color = collab)
  }

  # Add lines at initial values
  lines <- c(xline, yline)
  for (i in seq_along(lines)) {

    if (lines[i]) {

      axis <- c(xname, yname)[i]
      dl <- d
      dl <- dl %>% group_by(id)
      if (!is.null(splitvar)) dl <- dl %>% group_by(get(splitvar), add = TRUE)
      if (!is.null(splitvar2)) dl <- dl %>% group_by(get(splitvar2), add = TRUE)
      dl <- dl %>% summarize(first = get(axis)[t == min(t)])
      dl <- dl %>% ungroup()
      colnames(dl)[grep("get\\(splitvar\\)", colnames(dl))] <- splitvar
      colnames(dl)[grep("get\\(splitvar2\\)", colnames(dl))] <- splitvar2
      if (!is.null(splitvar)) dl <- dl %>% group_by(get(splitvar))
      if (!is.null(splitvar2)) dl <- dl %>% group_by(get(splitvar2), add = TRUE)
      dl <- dl %>% summarize(first = mean(first))
      colnames(dl)[grep("get\\(splitvar\\)", colnames(dl))] <- splitvar
      colnames(dl)[grep("get\\(splitvar2\\)", colnames(dl))] <- splitvar2

      if (i == 1) p <- p + geom_vline(dl, mapping = aes(xintercept = first), lty = 2)
      if (i == 2) p <- p + geom_hline(dl, mapping = aes(yintercept = first), lty = 2)

    }
  }

  return(p)

}
