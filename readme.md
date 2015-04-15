# codatools
*An R package to extend the usefulness of CODA MCMC diagnostics*


# Overview
`codatools` is a collection of R functions to diagnose MCMC chain
convergence and to visualize and summarize posterior distributions. All
functions expect an `mcmc` or `mcmc.list` objects from the coda package as
input. The functions do not extend the functionality provided by the coda
package, but instead were built to speed up MCMC diagnostics by providing more
informative visualizations of MCMC chains and posterior distributions.


# Installation
The `codatools` package is not on CRAN, but can be installed from R using:

    install.packages("devtools")
    library(devtools)
    install_github(repo = "michaelmalick/r-codatools")
    library(codatools)


# License 
The `codatools` package is [MIT/X11](http://opensource.org/licenses/MIT)
licensed. Copyright (c) 2015 Michael Malick

