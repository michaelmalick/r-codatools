#' @title Create a dotplot of median parameter values with 95\% credibility
#'        intervals
#'
#' @description
#'      \code{coda_dotplot} creates a single dotplot of the specified parameters
#'      (or all parameters if none are specified) with the dots giving the
#'      median value over all mcmc chains and horizontal bars giving the 80\%
#'      credibility interval (thick bars) and the 95\% credibility intervals
#'      (thin bars). This type of plot is also known as a catepillar plot and
#'      provides a way to easily summarize many parameters in a hierarchical
#'      model.
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
#' @seealso \code{\link{xyplot}} 
#'
#' @examples
#' library(coda)
#' data(line)
#' 
#' coda_dotplot(line)
#' 
#' coda_dotplot(line, parameters = "alpha")
#' 
#' coda_dotplot(line, parameters = c("alpha", "beta"))
#' 
#' coda_dotplot(line, parameters = grep("sig", varnames(line), value = TRUE))
#' 
#' coda_dotplot(line, parameters = grep("a", varnames(line), value = TRUE))
#' 
#' coda_dotplot(line, parameters = c("alpha", grep("sig", varnames(line), 
#'              value = TRUE)))
#' 
coda_dotplot <- function(
    coda.object,
    parameters = NULL) {
    
    parm.p <- coda_table(coda.object, parameters = parameters)

    fun.par <- list(
        strip.background = list(col = c("grey95", "grey85")),
        strip.shingle    = list(col = c("grey95", "grey85")),
        strip.border     = list(col = "grey50"),
        axis.line        = list(col = "grey50"),
        axis.text        = list(col = "grey30"),
        reference.line   = list(col = "grey85"))

    xmin <- min(parm.p$"2.5%") 
    xmax <- max(parm.p$"97.5%")

    xmin <- xmin - abs(xmax*0.03)
    xmax <- xmax + abs(xmax*0.03)

    l <- lattice::xyplot(
        factor(row.names(parm.p), levels = row.names(parm.p)) ~ parm.p$median,
        pch  = 19,
        cex  = 1.0,
        col  = "steelblue",
        ylab = "",
        xlab = "Value",
        xlim = c(xmin, xmax),
        par.settings = fun.par,
        scales = list(tck = c(1, 0)),
        panel = function(x, y, ...) {
            lattice::panel.segments(y0 = 1:dim(parm.p)[1], y1 = 1:dim(parm.p)[1],
                x0 = parm.p$"2.5%", x1 = parm.p$"97.5%", col = "steelblue",
                lwd = 1)
            lattice::panel.segments(y0 = 1:dim(parm.p)[1], y1 = 1:dim(parm.p)[1],
                x0 = parm.p$"10%", x1 = parm.p$"90%", col = "steelblue",
                lwd = 2.5)
            lattice::panel.xyplot(x, y, ...)
        })

    print(l)

}
