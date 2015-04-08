# mcmc-utilities
# Michael Malick
# 08 Apr 2015
#
#
# Functions:
#   - mcmc_df()
#   - mcmc_table()
#   - mcmc_diag()
#   - mcmc_dotplot()
#   - mcmc_histogram()


# ----------------------------
# mcmc_df()
# ----------------------------
# {{{
mcmc_df <- function(
    coda.object, 
    parms = NULL) {
    
    # coda.object = an mcmc.list object from the coda package
    # parms = character vector of parameter names
    #
    # This function takes as input an mcmc.list object from the coda package and
    # outputs a data frame with a column for each parameter and two indicator
    # columns, one giving the chain number, and another giving the iteration
    # number within that chain. 

    mat       <- as.matrix(coda.object, iter = TRUE, chain = TRUE)
    df        <- as.data.frame(mat)
    names(df) <- c("chain", "iter", coda::varnames(coda.object))

    if(is.null(parms))
        out.df <- df

    if(!is.null(parms))
        out.df <- subset(df, select = c("chain", "iter", parms))

    out.df
}



## TESTING ##
if(FALSE){

    library(coda)
    data(line)

    df <- mcmc_df(line)
    head(df)
    tail(df)

    df <- mcmc_df(line, parms = "alpha")
    head(df)

    df <- mcmc_df(line, parms = c("alpha", "beta"))
    head(df)

    df <- mcmc_df(line, parms = grep("sig", varnames(line), value = TRUE))
    head(df)

    df <- mcmc_df(line, parms = grep("a", varnames(line), value = TRUE))
    head(df)

    df <- mcmc_df(line, parms = c("alpha", grep("sig", varnames(line), 
                  value = TRUE)))
    head(df)

}


# }}}



# ----------------------------
# mcmc_table()
# ----------------------------
# {{{
mcmc_table <- function(
    coda.object, 
    parms = NULL) {
    
    # coda.object = an mcmc.list object from the coda package
    # parms = character vector of parameter names
    #
    # This function takes as input an mcmc.list object from the coda package and
    # outputs a dataframe giving the median for each parameter estimate along
    # with the 2.5% and 97.5% credibility intervals. 

    sim.dat       <- mcmc_df(coda.object, parms = parms)
    sim.dat$chain <- NULL
    sim.dat$iter  <- NULL
    parms         <- names(sim.dat)
    n.parms       <- length(parms)
    
    ll <- list(
        mean   = apply(as.matrix(sim.dat), 2, mean),
        sd     = apply(as.matrix(sim.dat), 2, sd),
        median = apply(as.matrix(sim.dat), 2, median),
        p.conf = apply(as.matrix(sim.dat), 2, quantile,
                       probs = c(0.025, 0.1, 0.9, 0.975)),
        n.iter = rep(coda::nchain(coda.object) * coda::niter(coda.object), 
                     n.parms),
        n.eff  = coda::effectiveSize(coda.object)[parms],
        Rhat   = as.vector(coda::gelman.diag(coda.object)$psrf[,1][parms]))
    out <- do.call("rbind", ll)
    out <- as.data.frame(t(round(out, 3)))

    return(out)
}



## TESTING ##
if(FALSE){

    library(coda)
    data(line)

    mcmc_table(line)

    mcmc_table(line, parms = "alpha")

    mcmc_table(line, parms = c("alpha", "beta"))

    mcmc_table(line, parms = grep("sig", varnames(line), value = TRUE))

    mcmc_table(line, parms = grep("a", varnames(line), value = TRUE))

    mcmc_table(line, parms = c("alpha", grep("sig", varnames(line), 
             value = TRUE)))

}



# }}}



# ----------------------------
# mcmc_diag()
# ----------------------------
# {{{
mcmc_diag <- function(
    coda.object, 
    parms = NULL) {

    # coda.object = an mcmc.list object from the coda package
    # parms = character vector of parameter names
    #
    # This function provides four diagnostic plots for mcmc output:
    #   1. Traceplot of all chains
    #   2. Density plot of marginal distributions for each chain with median
    #      values and 95% credibility intervals shown as rug
    #   3. Autocorrelation for each chain with effective sample size (ess)
    #   4. Gelman-Ruben statistic

    n.chains      <- coda::nchain(coda.object)
    sim.dat       <- mcmc_df(coda.object, parms = parms)
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

        # med <- apply(sim.mat, 2, median)
        # abline(v = med, col = pal, lty = 2)

        # conf <- t(apply(sim.mat, 2, quantile, probs = c(0.025, 0.975)))
        # segments(y0 = -1, y1 = max(dens.y) * 0.01, x0 = conf[ , 1], 
        #     x1 = conf[ , 1], col = pal)
        # segments(y0 = -1, y1 = max(dens.y) * 0.01, x0 = conf[ , 2], 
        #     x1 = conf[ , 2], col = pal)


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
        xx <- round(xx$psrf[1, i], 3)
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
        dens <- density(dat.hist)
        y.min <- min(dens$y)
        y.max <- max(dens$y)
        x.min <- min(dens$x)
        x.max <- max(dens$x)

        hist(dat.hist,
            freq = FALSE,
            col = "grey60",
            border = "white",
            ylab = "Density",
            xlab = "Value",
            main = "",
            ylim = c(y.min, y.max),
            xlim = c(x.min, x.max),
            axes = FALSE)
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


        ## Title
        mtext(parms[i], outer = TRUE, line = -1.5, font = 2)
    }
}

## TESTING ##
if(FALSE){

    library(coda)
    data(line)
    
    mcmc_diag(line)

    mcmc_diag(line, parms = "alpha")

    mcmc_diag(line, parms = "beta")

    mcmc_diag(line, parms = c("alpha", "beta"))

    mcmc_diag(line, parms = grep("sig", varnames(line), value = TRUE))

    mcmc_diag(line, parms = grep("a", varnames(line), value = TRUE))

    mcmc_diag(line, parms = c("alpha", grep("sig", varnames(line), 
             value = TRUE)))

}



# }}}



# ----------------------------
# mcmc_dotplot()
# ----------------------------
# {{{
mcmc_dotplot <- function(
    coda.object,
    parms = NULL) {

    # coda.object = an mcmc.list object from the coda package
    # parms = character vector of parameter names
    #
    # This function plots the median value for each parameter specified by the
    # parms arguement as a dot and the 80% credibility interval is shown as a
    # thick horizontal line and the 95% credibility interval is shown as a thin
    # horizontal line. This type of plot is also known as a catepillar plot and
    # provides a way to easily summarize many parameters in a hierarchical
    # model.

    parm.p <- mcmc_table(coda.object, parms = parms)

    fun.par <- list(
        strip.background  = list(col  = c("grey95", "grey85")),
        strip.shingle     = list(col  = c("grey95", "grey85")),
        strip.border      = list(col  = "grey50"),
        axis.line         = list(col  = "grey50"),
        axis.text         = list(col  = "grey30"),
        reference.line    = list(col  = "grey85"))

    xmin <- min(parm.p$"2.5%") 
    xmax <- max(parm.p$"97.5%")

    xmin <- xmin - abs(xmax*0.03)
    xmax <- xmax + abs(xmax*0.03)

    l <- xyplot(as.factor(row.names(parm.p)) ~ parm.p$median,
        pch = 19,
        cex = 1.0,
        col = "steelblue",
        ylab = "",
        xlab = "Value",
        xlim = c(xmin, xmax),
        par.settings = fun.par,
        scales = list(tck = c(1, 0)),
        panel = function(x, y, ...) {
            panel.segments(y0 = 1:dim(parm.p)[1], y1 = 1:dim(parm.p)[1],
                x0 = parm.p$"2.5%", x1 = parm.p$"97.5%", col = "steelblue",
                lwd = 1)
            panel.segments(y0 = 1:dim(parm.p)[1], y1 = 1:dim(parm.p)[1],
                x0 = parm.p$"10%", x1 = parm.p$"90%", col = "steelblue",
                lwd = 2.5)
            panel.xyplot(x, y, ...)
        })


    print(l)

}

## TESTING ##
if(FALSE){

    library(coda)
    data(line)

    mcmc_dotplot(line)

    mcmc_dotplot(line, parms = "alpha")

    mcmc_dotplot(line, parms = c("alpha", "beta"))

    mcmc_dotplot(line, parms = grep("sig", varnames(line), value = TRUE))

    mcmc_dotplot(line, parms = grep("a", varnames(line), value = TRUE))

    mcmc_dotplot(line, parms = c("alpha", grep("sig", varnames(line), 
                 value = TRUE)))

}




# }}}



# ----------------------------
# mcmc_histogram()
# ----------------------------
# {{{
mcmc_histogram <- function(
    coda.object,
    parms = NULL) {
    
    # coda.object = an mcmc.list object from the coda package
    # parms = character vector of parameter names
    #
    # This function plots a histogram of the marginal posterior distribution for
    # each parameter specified by the parms argument. In addition, a density
    # overlay, rug of mcmc sample values, median value (bold vertical dash), and
    # 95% credibility interval (bold horizontal line) are plotted. The function
    # uses lattice to panel the histograms for each parameter.

    dat <- mcmc_df(coda.object, parms = parms)
    dat.l <- reshape(dat, direction = "long",
        varying = 3:dim(dat)[2],
        v.names = "value",
        times = names(dat)[3:dim(dat)[2]],
        timevar = "parm")
    row.names(dat.l) <- NULL
    dat.l$id <- NULL

    fun.par <- list(
        strip.background  = list(col  = c("grey95", "grey85")),
        strip.shingle     = list(col  = c("grey95", "grey85")),
        strip.border      = list(col  = "grey50"),
        axis.line         = list(col  = "grey50"),
        axis.text         = list(col  = "grey30"),
        reference.line    = list(col  = "grey85"))

    l <- densityplot( ~ value | parm, data = dat.l, 
        scales   = list(relation = "free"),
        col      = "grey30",
        pch      = "",
        xlab     = "Value",
        as.table = TRUE,
        par.settings = fun.par,
        panel = function(x, ...) {
            panel.histogram(x, pch = "", col = "grey60", border = "white",
                type = "density", breaks = NULL)
            panel.rug(x, col = "grey60")
            panel.segments( y0 = 0, y1 = 0, 
                x0 = quantile(x, probs = c(0.025, 0.975))[1],
                x1 = quantile(x, probs = c(0.025, 0.975))[2],
                lwd = 3, col = "tomato", lend = 2)
            panel.points(x = median(x), y = 0, pch = "|", cex = 3, 
                col = "steelblue")
            panel.densityplot(x, ...)
        })
    print(l)
}

## TESTING ##
if(FALSE){

    library(coda)
    data(line)

    mcmc_histogram(line)

    mcmc_histogram(line, parms = "alpha")

    mcmc_histogram(line, parms = c("alpha", "beta"))

    mcmc_histogram(line, parms = grep("sig", varnames(line), value = TRUE))

    mcmc_histogram(line, parms = grep("a", varnames(line), value = TRUE))

    mcmc_histogram(line, parms = c("alpha", grep("sig", varnames(line), 
                 value = TRUE)))

}


# }}}


