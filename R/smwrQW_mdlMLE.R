#' @title Estimate Statistics
#'
#' @description Support function for computing statistics for left-censored data using the 
#'maximum likelihood method (Helsel and Cohn, 1988).
#'
#'Added from smwrQW.
#'
#' @importFrom survival survreg Surv
#' @param x the data to estimate, Missing values permitted and ignored.
#'Must be an object of class "lcens," a numeric vector, or the output from censpp.
#' @param method the method to use, either "MLE" or "log MLE."
#' @param alpha the offset for plotting position, used to compute the filled in values.
#' @return A list containing the mean and standard deviation, filled in
#'values for the censored values, and the censored levels. If \code{method}
#'is "log MLE," then the list also contains the mean and standard deviation of the 
#'natural log-transformed values computed by maximum likelihood.
#' @references Helsel, D.R. and Cohn, T.A., 1988, Estimation of descriptive statistics 
#'for multiply censored water quality data: Water Resources Research v. 24, n.
#'12, p.1997--2004
#' @keywords internal misc
#' @export
mdlMLE <- function(x, method="MLE", alpha=0.4) {
  ## Coding history:
  ##    2012Mar09 DLLorenz original coding
  ##    2013Jan05 DLLorenz Roxygenized
  ##    2013Jan05          This version
  ##
  method <- match.arg(method, c("MLE", "log MLE"))
  if(class(x) != "list")
    x <- censpp(x, a=alpha)
  step1 <- Surv(c(x$x, x$xcen), c(rep(1, length(x$x)), rep(0, length(x$xcen))),
                type="left")
  if(method == "MLE") {
    step2 <- survreg(step1 ~ 1, dist="gaussian")
    coefs <- as.vector(c(step2$coefficients, step2$scale))
    step3 <- qnorm(x$ppcen) *  coefs[2L] + coefs[1L]
    step4 <- as.vector(c(step3, x$x))
    retval <- list(mean=coefs[1L], sd=coefs[2L], fitted=step4)
  }
  else {
    step2 <- survreg(step1 ~ 1, dist="lognormal")
    coefs <- as.vector(c(step2$coefficients, step2$scale))
    step3 <- qnorm(x$ppcen) *  coefs[2L] + coefs[1L]
    step4 <- as.vector(c(exp(step3), x$x))
    retval <- list(meanlog=coefs[1L], sdlog=coefs[2L], fitted=step4)
  }
  if(length(x$xcen) > 0L)
    retval$censorlevels <- x$xcen
  else
    retval$censorlevels  <- -Inf
  return(retval)
}
