#' @title Create a dataframe from an mcmc.list object
#'
#' @description
#'      \code{coda_df} takes as input an mcmc.list object and converts it to a
#'      dataframe with a column for each monitored parameter and two indicator
#'      columns, one giving the chain number and another giving the iteration
#'      number within that chain.
#'
#' @param coda.object
#'      an mcmc.list object
#'
#' @param parameters
#'      character vector of parameter names to include in dataframe. If none are
#'      supplied all monitored parameters are included.
#'
#' @return
#'      A dataframe
#'
#' @seealso \code{\link{data.frame}} \code{\link{mcmc}} \code{\link{mcmc.list}}
#'
#' @export
#'
#' @author Michael Malick
#'
#' @examples
#' library(coda)
#' data(line)
#'
#' df <- coda_df(line)
#' head(df)
#' tail(df)
#'
#' df <- coda_df(line, parameters = "alpha")
#' head(df)
#'
#' df <- coda_df(line, parameters = c("alpha", "beta"))
#' head(df)
#'
#' df <- coda_df(line, parameters = grep("sig", coda::varnames(line), value = TRUE))
#' head(df)
#'
#' df <- coda_df(line, parameters = grep("a", coda::varnames(line), value = TRUE))
#' head(df)
#'
#' df <- coda_df(line, parameters = c("alpha", grep("sig", coda::varnames(line),
#'               value = TRUE)))
#' head(df)
#'
coda_df <- function(coda.object,
                    parameters = NULL) {

    if (!coda::is.mcmc(coda.object) && !coda::is.mcmc.list(coda.object))
        stop("Not an mcmc or mcmc.list object")

    n.chain <- coda::nchain(coda.object)
    mat     <- as.matrix(coda.object, iter = TRUE, chain = TRUE)
    df      <- as.data.frame(mat)

    if(n.chain == 1)
        df <- data.frame(1, df)

    names(df) <- c("chain", "iter", coda::varnames(coda.object))

    if(is.null(parameters))
        out.df <- df

    if(!is.null(parameters))
        out.df <- subset(df, select = c("chain", "iter", parameters))

    out.df
}
