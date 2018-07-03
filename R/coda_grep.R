#' @title Quickly subset an mcmc.list object using grep
#'
#' @description
#'      \code{coda_grep} takes as input an mcmc.list object and uses
#'      grep to find the parameters that match the specified pattern.
#'
#' @param coda.object
#'      an mcmc.list object
#'
#' @param pattern
#'      character string giving the pattern to search for
#'
#' @param return.matrix
#'      should the mcmc.list object be returned as a single matrix. If TRUE,
#'      each element of the mcmc.list is binded together using rbind.
#'
#' @param \dots
#'      additional arguments for \code{\link{grep}}
#'
#' @return
#'      An mcmc, mcmc.list or matrix
#'
#' @seealso \code{\link{grep}}
#'
#' @export
#'
#' @author Michael Malick
#'
#' @examples
#' library(coda)
#' data(line)
#'
#' x <- coda_grep(line, "a")
#' head(x[[1]])
#'
#' x <- coda_grep(line, "beta")
#' head(x[[1]])
#'
#' x <- coda_grep(line, "sig")
#' head(x[[1]])
#'
#' x <- coda_grep(line, "a", return.matrix = TRUE)
#' class(x)
#' dim(x)
#' head(x)
#'
coda_grep <- function(coda.object,
                      pattern,
                      return.matrix = FALSE, ...) {

    if (!coda::is.mcmc(coda.object) && !coda::is.mcmc.list(coda.object))
        stop("Not an mcmc or mcmc.list object")

    params <- grep(pattern, coda::varnames(coda.object), value = TRUE, ...)
    out    <- coda.object[ , params]

    if(return.matrix)
        out <- do.call("rbind", out)

    return(out)
}
