#' @title Create a histogram of marginal posterior distributions
#'
#' @description
#'      \code{coda_hist} uses the \code{hist} function inbase R
#'      to plot histograms of the marginal posterior
#'      distribution for each parameter specified by the parameters argument.
#'      In addition, a density overlay, rug of mcmc sample values, median value
#'      (bold vertical dash), and 95\% credibility interval (bold horizontal
#'      line) are plotted. A single histogram is printed per graphics device.
#'
#' @param coda.object
#'      An mcmc.list object
#'
#' @param parameters
#'      character vector of parameter names to include in graphic. If none are
#'      supplied all monitored parameters are included.
#'
#' @return
#'      A graphics device
#'
#' @export
#'
#' @author Michael Malick
#'
#' @seealso \code{\link{hist}}
#'
#' @examples
#' library(coda)
#' data(line)
#'
#' coda_hist(line)
#'
#' coda_hist(line, parameters = "alpha")
#'
#' coda_hist(line, parameters = c("alpha", "beta"))
#'
#' coda_hist(line, parameters = grep("sig", varnames(line), value = TRUE))
#'
#' coda_hist(line, parameters = grep("a", varnames(line), value = TRUE))
#'
#' coda_hist(line, parameters = c("alpha", grep("sig", varnames(line),
#'                value = TRUE)))
#'
coda_hist <- function(coda.object,
                      parameters = NULL) {

    n.chains      <- coda::nchain(coda.object)
    sim.dat       <- coda_df(coda.object, parameters = parameters)
    chain.fac     <- sim.dat$chain
    sim.dat$chain <- NULL
    sim.dat$iter  <- NULL
    parms         <- names(sim.dat)
    n.parms       <- length(parms)

    for(i in 1:n.parms) {

        dat.hist <- sim.dat[ , i]
        dens     <- stats::density(dat.hist)
        y.min    <- min(dens$y)
        y.max    <- max(dens$y)
        x.min    <- min(dens$x)
        x.max    <- max(dens$x)

        graphics::hist(dat.hist,
                       freq   = FALSE,
                       col    = "grey60",
                       border = "white",
                       ylab   = "Density",
                       xlab   = "Value",
                       main   = parms[i],
                       ylim   = c(y.min, y.max),
                       xlim   = c(x.min, x.max),
                       axes   = FALSE)
        graphics::box(col = "grey50")
        graphics::axis(1, lwd = 0, col = "grey50", lwd.ticks = 1)
        graphics::axis(2, lwd = 0, col = "grey50", las = 1, lwd.ticks = 1)
        graphics::lines(dens, col = "grey30")
        graphics::rug(dat.hist, col = "grey75")
        graphics::segments( y0 = 0, y1 = 0,
            x0 = stats::quantile(dat.hist, probs = c(0.025, 0.975))[1],
            x1 = stats::quantile(dat.hist, probs = c(0.025, 0.975))[2],
            lwd = 3, col = "tomato", lend = 2)
        graphics::points(x = stats::median(dat.hist), y = 0, pch = "|", cex = 3,
                         col = "steelblue")
        txt <- paste("median =", round(stats::median(dat.hist), 3))
        graphics::mtext(txt, side = 3, cex = 0.7)

    }
}
