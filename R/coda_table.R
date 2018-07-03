#' @title Create a summary table of an mcmc.list object
#'
#' @description
#'      \code{coda_table} takes as input an mcmc.list object from the coda
#'      package and outputs a dataframe giving the mean, standard
#'      deviation, and median for each parameter estimate along with quantiles
#'      for 95\% and 80\% credibility intervals.
#'
#' @param coda.object
#'      an mcmc.list object
#'
#' @param parameters
#'      character vector of parameter names to include in table. If none are
#'      supplied all monitored parameters are included.
#'
#' @return
#'      A dataframe
#'
#' @seealso \code{\link{coda_df}}
#'
#' @export
#'
#' @author Michael Malick
#'
#' @examples
#' library(coda)
#' data(line)
#'
#' coda_table(line)
#'
#' coda_table(line, parameters = "alpha")
#'
#' coda_table(line, parameters = c("alpha", "beta"))
#'
#' coda_table(line, parameters = grep("sig", coda::varnames(line), value = TRUE))
#'
#' coda_table(line, parameters = grep("a", coda::varnames(line), value = TRUE))
#'
#' coda_table(line, parameters = c("alpha", grep("sig", coda::varnames(line),
#'          value = TRUE)))
#'
coda_table <- function(coda.object,
                       parameters = NULL) {

    sim.dat       <- coda_df(coda.object, parameters = parameters)
    sim.dat$chain <- NULL
    sim.dat$iter  <- NULL
    parms         <- names(sim.dat)
    n.parms       <- length(parms)

    ll <- list(mean   = apply(as.matrix(sim.dat), 2, mean),
               sd     = apply(as.matrix(sim.dat), 2, stats::sd),
               median = apply(as.matrix(sim.dat), 2, stats::median),
               p.conf = apply(as.matrix(sim.dat), 2, stats::quantile,
                              probs = c(0.025, 0.1, 0.9, 0.975)),
               n.iter = rep(coda::nchain(coda.object) * coda::niter(coda.object),
                            n.parms),
               n.eff  = coda::effectiveSize(coda.object)[parms],
               Rhat   = as.vector(coda::gelman.diag(coda.object)$psrf[,1][parms]))
    out <- do.call("rbind", ll)
    out <- as.data.frame(t(round(out, 3)))

    return(out)
}
