#' @title Create Rhat diagnostic graphic
#'
#' @description
#'      \code{coda_rhat} creates a graphic of the potential scale reduction
#'      factor (Rhat)
#'
#' @param rhat
#'      named vector of Rhat values
#'
#' @param thresh
#'      threshold used to flag low Rhat values
#'
#' @param cex.labels
#'      scaling for parameters plotted by name
#'
#' @return
#'      Invisibly returns a vector of the Rhat values for each
#'      parameter sorted from highest (first) to lowest (last)
#'      and prints a plot of the effective sample sizes.
#'
#' @seealso \code{\link{gelman.diag}}
#'
#' @export
#'
#' @author Michael Malick
#'
#' @examples
#' library(coda)
#' data(line)
#'
#' gd <- coda::gelman.diag(line)
#' rhat <- gd$psrf[ , 1]
#' coda_rhat(rhat)
coda_rhat <- function(rhat, thresh = 1.01, cex.labels = 0.5) {

    ind.low  <- which(rhat > thresh)
    ind.high <- which(rhat <= thresh)
    def.par <- graphics::par(no.readonly = TRUE)

    graphics::par(mar = c(3, 4, 4, 2))
    graphics::plot(rhat, ylim = c(min(rhat), max(thresh, rhat)),
                   type = "n",
                   ylab = "Rhat",
                   xlab = "",
                   axes = FALSE,
                   main = "Potential scale reduction factor (Rhat)",
                   las = 1)
    graphics::axis(side = 2, lwd = 0, lwd.tick = 1, las = 1, col = "grey50")
    graphics::box(col = "grey50")

    graphics::points(ind.high, rhat[ind.high], pch = 16, col = "grey50")
    if(length(ind.low) > 0)
        graphics::text(ind.low, rhat[ind.low], names(ind.low),
                       cex = cex.labels)
    graphics::abline(h = thresh, col = "red3", lty = 2)
    txt <- paste( "max Rhat =", round(max(rhat), 3))
    graphics::mtext(txt, side = 3, cex = 0.7)
    graphics::par(def.par)

    invisible(rev(sort(rhat)))
}
