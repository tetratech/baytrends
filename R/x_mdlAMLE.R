#'Estimate Statistics
#'
#'Support function for computing statistics for left-censored data using the
#'adjusted maximum-likelihood method (Cohn, 1988)
#'
#' @param x the data to estimate, Missing values permitted and ignored.
#'Must be an object of class "lcens," a numeric vector, or the output from censpp.
#' @param method the method to use, either "AMLE" or "log AMLE."
#' @param alpha the offset for plotting postion, used to compute the filled in values.
#' @return A list containing the mean and standard deviation, filled in
#'values for the censored values, and the maximum censor level. If \code{method}
#'is "log AMLE," then the list also contains the mean and standard deviation of the 
#'natural log-transformed values computed by maximum likelihood.
#' @references Cohn, T.A., 1988, Adjusted maximum likelihood estimation of the moments
#'of lognormal populations from type 1 censored samples: U.S. Geological Survey 
#'Open-File Report 88-350, 34 p.
#' @keywords misc
#' @export
mdlAMLE <- function(x, method="AMLE", alpha=0.4) {
  ## Coding history:
  ##    2012Apr04 DLLorenz original coding
  ##    2013Jan02 DLLorenz Roxygenized
  ##    2013Jan02          This version
  ##
  method <- match.arg(method, c("AMLE", "log AMLE"))
  if(class(x) != "list") # make into 
    x <- censpp(x, a=alpha)
  obs <- as.double(c(x$x, x$xcen))
  censflag <- c(rep(FALSE, length(x$x)), rep(TRUE, length(x$xcen)))
  if(method == "AMLE") {
    uselog <- FALSE
    detlim <- c(rep(min(x$x) - 1, length(x$x)), x$xcen)
  }
  else {
    uselog <- TRUE
    detlim <- c(rep(min(x$x)/2, length(x$x)), x$xcen)
  }
  nobs <- as.integer(length(obs))
  zmu <- zsig <- double(2L)
  nlt <- ierr <- integer(1L)
  step1 <- .Fortran("automdl",
                    obs=obs,
                    Censflag=censflag,
                    detlim=detlim,
                    nobs=nobs,
                    zmu=zmu,
                    zsig=zsig,
                    nlt=nlt, 
                    uselog=uselog,
                    ierr=ierr)
  if(step1$ierr > 0) # Just bail
    retval <- list(mean=NA_real_, meanlog=NA_real_, sd=NA_real_, sdlog=NA_real_,
                   fitted=rep(NA_real_, nobs), censorlevels=NA_real_)
  else {
    if(length(x$xcen) > 0L) {
      cmax <- max(x$xcen)
      uncpred <- x$x < cmax
      topred <- length(x$xcen) + sum(uncpred)
      fitted <- qnorm(ppoints(nobs, a=alpha)[seq(topred)]) * step1$zsig[1] + step1$zmu[1]
      if(uselog)
        fitted <- exp(fitted)
      fitted <- c(fitted, x$x[!uncpred])
    }
    else {
      fitted <- x$x
      cmax <- -Inf
    }
    if(uselog)
      retval <- list(mean=step1$zmu[2L], meanlog=step1$zmu[1L],
                     sd=step1$zsig[2L], sdlog=step1$zsig[1L]*sqrt(nobs/(nobs-1)), # Adjust manually
                     fitted=fitted, censorlevels=cmax)
    else
      retval <- list(mean=step1$zmu[1L], sd=step1$zsig[1L]*sqrt(nobs/(nobs-1)), # Correct for bias
                     fitted=fitted, censorlevels=cmax)
  }
  return(retval)
}
