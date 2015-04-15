#' @title Create multiple diagnostic graphics for a single parameter
#'
#' @description
#'      \code{coda_diag} creates a graphic that includes six diagnostic plots to
#'      help monitor convergence of an MCMC chain for a parameter. By default
#'      the function will loop through all parameters in an mcmc.list object and
#'      create a single graphic of six diagnostic plots for each parameter.
#'
#' @param coda.object
#'      an mcmc.list object
#'  
#' @param parameters
#'      character vector of parameter names to create diagnostic plots for. If 
#'      none are supplied all monitored parameters are included.
#'
#' @return
#'      A graphic with six diagnostic plots:
#'
#'     \itemize{
#'         \item{traceplot of all chains}
#'         \item{density plot of marginal distribution for each chain}
#'         \item{autocorrelation for each chain with effective sample size 
#'               (ess)}
#'         \item{evolution of the Gelman-Rubin statistic with 97.5\% credibility
#'               interval}
#'         \item{evolution of the median value for the parameter with 95\%
#'               credibility interval}
#'         \item{histogram of the marginal distribution of all chains
#'               combined with median value and 95\% credibility interval}
#'      }
#'
#' @seealso \code{\link{traceplot}} \code{\link{density}}
#'          \code{\link{gelman.plot}} \code{\link{cumuplot}} \code{\link{hist}}  
#' 
#' @export
#'
#' @author Michael Malick
#'
#' @examples
#' library(coda)
#' data(line)
#' 
#' coda_diag(line)
#' 
#' coda_diag(line, parameters = "alpha")
#' 
#' coda_diag(line, parameters = "beta")
#' 
#' coda_diag(line, parameters = c("alpha", "beta"))
#' 
#' coda_diag(line, parameters = grep("sig", varnames(line), value = TRUE))
#' 
#' coda_diag(line, parameters = grep("a", varnames(line), value = TRUE))
#' 
#' coda_diag(line, parameters = c("alpha", grep("sig", varnames(line), 
#'          value = TRUE)))
#' 
coda_diag <- function(
    coda.object, 
    parameters = NULL) {

    n.chains      <- coda::nchain(coda.object)
    sim.dat       <- coda_df(coda.object, parameters = parameters)
    chain.fac     <- sim.dat$chain
    sim.dat$chain <- NULL
    sim.dat$iter  <- NULL
    parms         <- names(sim.dat)

    # Set color palette
    ang <- seq(0, 240, length.out = n.chains)
    pal <- hcl(h = ang, c = 100, l = 60, fixup = TRUE)

    n.parms <- length(parms)
    par(mfrow = c(3, 2), mar = c(4,4.5,3,0), oma = c(1,0,0,1))

    for(i in 1:n.parms) {

        dat.sp  <- subset(sim.dat, select = parms[i])
        sim.sp  <- split(dat.sp, chain.fac)
        sim.mat <- matrix(unlist(sim.sp), ncol = n.chains, byrow = FALSE)

        ## Traceplot
        matplot(as.vector(time(coda.object)), sim.mat, 
            type = "l",
            lty  = 1,
            col  = pal,
            ylab = "Value",
            xlab = paste("Iteration (thin = ", coda::thin(coda.object), ")", 
                         sep = ""),
            axes = FALSE)
        box(col = "grey50")
        axis(1, lwd = 0, col = "grey50", lwd.tick = 1)
        axis(2, lwd = 0, col = "grey50", las = 1, lwd.tick = 1)
        txt <- paste("niter =", coda::niter(coda.object), "   nchain =",
            coda::nchain(coda.object))
        mtext(txt, side = 3, cex = 0.7)


        ## Density plot
        dens   <- apply(sim.mat, 2, density)
        dens.x <- lapply(dens, function(x) x$x)
        dens.x <- do.call("cbind", dens.x)
        dens.y <- lapply(dens, function(x) x$y)
        dens.y <- do.call("cbind", dens.y)

        matplot(dens.x, dens.y, 
            type = "l",
            lty  = 1,
            col  = pal,
            ylab = "Density",
            xlab = "Value",
            axes = FALSE)
        box(col = "grey50")
        axis(1, lwd = 0, col = "grey50", lwd.tick = 1)
        axis(2, lwd = 0, col = "grey50", las = 1, lwd.tick = 1)


        ## Autocorrelation plot
        acor   <- apply(sim.mat, 2, acf, plot = FALSE)
        acor.x <- lapply(acor, function(x) x$lag[ , , 1])
        acor.x <- do.call("cbind", acor.x)
        acor.y <- lapply(acor, function(x) x$acf[ , , 1])
        acor.y <- do.call("cbind", acor.y)

        matplot(acor.x, acor.y, 
            type = "o",
            pch  = 19,
            cex  = 0.7,
            lty  = 1,
            col  = pal,
            ylab = "Autocorrelation",
            xlab = "Lag",
            axes = FALSE)
        box(col = "grey50")
        axis(1, lwd = 0, col = "grey50", lwd.tick = 1)
        axis(2, lwd = 0, col = "grey50", las = 1, lwd.tick = 1)
        abline(h = 0, col = "grey50", lty = 2)
        ess <- coda::effectiveSize(coda.object)[parms[i]]
        text(x = max(acor.x), y = max(acor.y),
            labels = paste("ESS =", round(ess, 1)), adj = c(1.25, 2.5))


        ## Gelman-Rubin Statistic
        gp <- coda::gelman.plot(coda.object[ , parms[i]], 
            main = "",
            ylab = "Shrink factor",
            xlab = "Last iteration in chain",
            col  = c("steelblue", "grey40"),
            lwd  = c(2, 1),
            axes = FALSE,
            auto.layout = FALSE)
        axis(1, lwd = 0, col = "grey50", lwd.tick = 1)
        axis(2, lwd = 0, col = "grey50", las = 1, lwd.tick = 1)
        abline(h = 1, col = "grey50", lwd = 1)
        box(col = "grey50")
        xx <- gelman.diag(coda.object)
        xx <- round(xx$psrf[i, 1], 3)
        txt <- paste("Rhat =", xx)
        mtext(txt, side = 3, cex = 0.7)
        
        ## Cumulative Quantiles
        n.iter <- dim(sim.mat)[1]
        probs <- c(0.025, 0.50, 0.975)
        cum.arr <- array(NA, dim = c(n.iter, length(probs), n.chains))
        for(j in 1:n.chains) {
            for(k in 1:n.iter) 
                cum.arr[k , , j] <- quantile(sim.mat[1:k, j], probs = probs)
        }

        for(j in 1:n.chains) {

            if(j > 1)
                add <- TRUE
            else
                add <- FALSE

            matplot(as.vector(time(coda.object)), cum.arr[ , , j],
                type = "l",
                lwd = c(1, 2, 1),
                # cex  = 0.7,
                ylim = c(min(cum.arr), max(cum.arr)),
                lty  = c(2, 1, 2),
                col  = pal[j],
                ylab = "Median",
                xlab = "Iteration",
                add  = add,
                axes = FALSE)

            if(j == 1) {
                box(col = "grey50")
                axis(1, lwd = 0, col = "grey50", lwd.tick = 1)
                axis(2, lwd = 0, col = "grey50", las = 1, lwd.tick = 1)
            }
        }

        ## Histogram
        dat.hist <- sim.dat[ , i]
        dens     <- density(dat.hist)
        y.min    <- min(dens$y)
        y.max    <- max(dens$y)
        x.min    <- min(dens$x)
        x.max    <- max(dens$x)

        hist(dat.hist,
            freq   = FALSE,
            col    = "grey60",
            border = "white",
            ylab   = "Density",
            xlab   = "Value",
            main   = "",
            ylim   = c(y.min, y.max),
            xlim   = c(x.min, x.max),
            axes   = FALSE)
        box(col = "grey50")
        axis(1, lwd = 0, col = "grey50", lwd.tick = 1)
        axis(2, lwd = 0, col = "grey50", las = 1, lwd.tick = 1)
        lines(dens, col = "grey30")
        rug(dat.hist, col = "grey75")
        segments( y0 = 0, y1 = 0, 
            x0 = quantile(dat.hist, probs = c(0.025, 0.975))[1],
            x1 = quantile(dat.hist, probs = c(0.025, 0.975))[2],
            lwd = 3, col = "tomato", lend = 2)
        points(x = median(dat.hist), y = 0, pch = "|", cex = 3, 
            col = "steelblue")
        txt <- paste("median =", round(median(dat.hist), 3))
        mtext(txt, side = 3, cex = 0.7)

        ## Title
        mtext(parms[i], outer = TRUE, line = -1.5, font = 2)
    }
}
