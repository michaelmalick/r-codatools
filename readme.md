# mcmc-utilities
A collection of R functions to extend the usefulness of coda MCMC diagnostics


# Overview
`mcmc-utilities` is a collection of R functions to diagnose MCMC chain
convergence and to visualize and summarize posterior distributions. All
functions expect an `mcmc` or `mcmc.list` objects from the coda package as
input. The functions do not extend the functionality provided by the coda
package, but instead were built to speed up MCMC diagnostics by providing more
informative visualizations of MCMC chains and posterior distributions.


# Installation
`mcmc-utilities` is currently not an R package, but can be installed in a number
of ways:

1. Using `devtools`

    install.packages("devtools")
    library(devtools)
    source_url("https://raw.githubusercontent.com/MichaelMalick/r-mcmc-utilities/master/mcmc-utilities.R")


2. Using `wget`

    source(pipe(paste("wget -O -", "https://raw.githubusercontent.com/MichaelMalick/r-mcmc-utilities/master/mcmc-utilities.R")))


# License 
The `mcmc-utilities` functions are [MIT/X11](http://opensource.org/licenses/MIT)
licensed. Copyright (c) 2015 Michael Malick

