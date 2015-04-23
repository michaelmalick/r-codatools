#' @title Create a histogram of marginal posterior distributions
#'
#' @description
#'      \code{coda_histogram} uses the \code{histogram} function in the lattice 
#'      package to plot histograms of the marginal posterior
#'      distribution for each parameter specified by the parameters argument. 
#'      In addition, a density overlay, rug of mcmc sample values, median value
#'      (bold vertical dash), and 95\% credibility interval (bold horizontal
#'      line) are plotted. The function uses lattice to panel the histograms for
#'      each parameter.
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
#' @seealso \code{\link{histogram}} 
#'
#' @examples
#' library(coda)
#' data(line)
#' 
#' coda_histogram(line)
#' 
#' coda_histogram(line, parameters = "alpha")
#' 
#' coda_histogram(line, parameters = c("alpha", "beta"))
#' 
#' coda_histogram(line, parameters = grep("sig", varnames(line), value = TRUE))
#' 
#' coda_histogram(line, parameters = grep("a", varnames(line), value = TRUE))
#' 
#' coda_histogram(line, parameters = c("alpha", grep("sig", varnames(line), 
#'                value = TRUE)))
#' 
coda_histogram <- function(
    coda.object,
    parameters = NULL) {

    dat   <- coda_df(coda.object, parameters = parameters)
    dat.l <- reshape(dat, direction = "long",
        varying = 3:dim(dat)[2],
        v.names = "value",
        times   = names(dat)[3:dim(dat)[2]],
        timevar = "parm")
    row.names(dat.l) <- NULL
    dat.l$id <- NULL

    fun.par <- list(
        strip.background = list(col = c("grey95", "grey85")),
        strip.shingle    = list(col = c("grey95", "grey85")),
        strip.border     = list(col = "grey50"),
        axis.line        = list(col = "grey50"),
        axis.text        = list(col = "grey30"),
        reference.line   = list(col = "grey85"))

    dat.l$parm <- factor(dat.l$parm, levels = unique(dat.l$parm))

    l <- lattice::densityplot( ~ value | parm, 
        data = dat.l, 
        scales   = list(relation = "free"),
        col      = "grey30",
        pch      = "",
        xlab     = "Value",
        as.table = TRUE,
        par.settings = fun.par,
        panel = function(x, ...) {
            lattice::panel.histogram(x, pch = "", col = "grey60", 
                border = "white", type = "density", breaks = NULL)
            lattice::panel.rug(x, col = "grey60")
            lattice::panel.segments( y0 = 0, y1 = 0, 
                x0 = quantile(x, probs = c(0.025, 0.975))[1],
                x1 = quantile(x, probs = c(0.025, 0.975))[2],
                lwd = 3, col = "tomato", lend = 2)
            lattice::panel.points(x = median(x), y = 0, pch = "|", cex = 3, 
                col = "steelblue")
            lattice::panel.densityplot(x, ...)
        })
    print(l)
}
