#' @title Plots non-parametric density overlays for each column in a matrix
#'
#' @description
#'      Calculate and plot the non-parametric density for the data in each
#'      column of a matrix. All densities are plotted on the same graphic.
#'
#' @param x
#'      a matrix
#'
#' @param xlim
#'      vector of length 2 giving min and max for x-axis
#'
#' @param xlab
#'      x-axis label
#'
#' @param main
#'      main title for graphic
#'
#' @param plot.it
#'      logical, should a plot be created
#'
#' @param \dots
#'      additional argurments passed to \code{density}
#'
#' @return
#'      Invisibly returns a list with x and y components for the plotted density
#'      curves.
#'
#' @seealso \code{\link{density}}
#'
#' @export
#'
#' @author Michael Malick
#'
#' @examples
#' mat <- matrix(rnorm(100000), ncol = 100)
#' colDensity(mat)
#' colDensity(mat, main = "test")
#' colDensity(mat, xlim = c(-4, 3))
#' xx <- colDensity(mat, xlim = c(-4, 3))
#' class(xx)
#'
colDensity <- function(x, xlim = NULL, xlab = "", main = "", plot.it = TRUE, ...) {

    if (!is.matrix(x))
        stop("x is not a matrix")

    n.rows <- dim(x)[1]
    n.cols <- dim(x)[2]
    x.min  <- rep(NA, n.cols)
    x.max  <- rep(NA, n.cols)
    y.min  <- rep(NA, n.cols)
    y.max  <- rep(NA, n.cols)

    ## Calculate column densities
    dens   <- vector("list", n.cols)
    x.dens <- vector("list", n.cols)
    y.dens <- vector("list", n.cols)
    for(i in 1:n.cols) {
        dens[[i]]   <- stats::density(x[ , i], ...)
        x.dens[[i]] <- dens[[i]]$x
        y.dens[[i]] <- dens[[i]]$y
    }
    x.dens <- do.call("cbind", x.dens)
    y.dens <- do.call("cbind", y.dens)

    ## Set color palette
    ang <- seq(0, 240, length.out = n.cols)
    pal <- grDevices::hcl(h = ang, c = 100, l = 60, fixup = TRUE)

    if(is.null(xlim))
        xlim <- c(min(x.dens), max(x.dens))

    ## Create plot
    if(plot.it) {
        graphics::matplot(x.dens, y.dens,
                          type = "l",
                          lty  = 1,
                          xlim = xlim,
                          ylim = c(min(y.dens), max(y.dens)),
                          col  = pal,
                          main = main,
                          ylab = "Density",
                          xlab = xlab,
                          axes = FALSE)
        graphics::axis(2, lwd = 1, lwd.ticks = 1, las = 1, col = "grey50")
        graphics::axis(1, lwd = 1, lwd.ticks = 1, col = "grey50")
        graphics::box(col = "grey50")
    }

    invisible(list(x = x.dens, y = y.dens))
}
