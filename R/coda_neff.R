#' @title Create effective sample size diagnostic graphic
#'
#' @description
#'      \code{coda_neff} creates a graphic of the effective sample size for each
#'      parameter in the input object. The left-hand y-axis is the total
#'      effective size and the right-hand y-axis is the effective size
#'      scaled by the total number of MCMC draws.
#'
#' @param coda.object
#'      an mcmc.list object
#'
#' @param thresh
#'      threshold used to flag parameters with low effective sample sizes, as a
#'      percentage of total MCMC draws
#'
#' @param plot
#'      logical, should a plot be printed
#'
#' @param cex.labels
#'      scaling for parameters plotted by name
#'
#' @return
#'      Invisibly returns a vector of the effective sample size for each
#'      parameter sorted from lowest (first) to highest (last). Optionally
#'      prints a plot of the effective sample sizes.
#'
#' @seealso \code{\link{effectiveSize}}
#'
#' @export
#'
#' @author Michael Malick
#'
#' @examples
#' library(coda)
#' data(line)
#'
#' coda_neff(line)
coda_neff <- function(coda.object, plot = TRUE,
                      thresh = 0.5, cex.labels = 0.5) {

    neff <- coda::effectiveSize(coda.object)

    if(plot) {
        samps    <- coda::niter(coda.object) * coda::nchain(coda.object)
        ind.low  <- which(neff < thresh * samps)
        ind.high <- which(neff >= thresh * samps)

        def.par <- graphics::par(no.readonly = TRUE)

        graphics::par(mar = c(3, 5, 4, 4))
        graphics::plot(neff, ylim = c(min(neff), max(samps, neff)),
                       type = "n",
                       ylab = "",
                       xlab = "",
                       axes = FALSE,
                       main = "Effective sample size",
                       las = 1)
        graphics::axis(side = 2, lwd = 0, lwd.tick = 1, las = 1, col = "grey50")
        graphics::box(col = "grey50")


        graphics::points(ind.high, neff[ind.high], pch = 16, col = "grey50")
        if(length(ind.low) > 0)
            graphics::text(ind.low, neff[ind.low], names(ind.low),
                           cex = cex.labels)
        graphics::mtext(side = 2, "Effective sample size", line = 4.0)
        graphics::abline(h = samps, col = "grey50", lty = 2)
        graphics::abline(h = thresh * samps, col = "red3", lty = 2)
        txt <- paste("N draws =", samps, "   Min Neff =", floor(min(neff)))
        graphics::mtext(txt, side = 3, cex = 0.7)
        axt <- graphics::axTicks(side = 2)
        graphics::axis(side = 4, at = axt, labels = axt / samps, las = 1, col = "grey50")
        graphics::mtext(side = 4, "Neff / N", line = 3)
        graphics::par(def.par)
    }
    invisible(sort(neff))
}
