#' @title Estimate Statistics
#'
#' @description Support function for computing statistics for multiply-censored 
#'data extended from the maximum likelihood method (Helsel and Cohn, 1988).
#'
#'Added from smwrQW.
#'
#' @importFrom survival survreg Surv
#' @param x the data to estimate, Missing values permitted and ignored.
#'Must be an object of class "mcens," a numeric vector, or the output from censpp.
#' @param method the method to use, either "MLE" or "log MLE."
#' @param alpha the offset for plotting position, used to compute the filled in values.
#' @return A list containing the mean and standard deviation, filled in
#'values for the censored values, and the censored levels. If \code{method}
#'is "log MLE," then the list also contains the mean and standard deviation of the 
#'natural log-transformed values computed by maximum likelihood.
#' @references Helsel, D.R. and Cohn, T.A., 1988, Estimation of descriptive statistics 
#'for multiply censored water quality data: Water Resources Research v. 24, n.
#'12, pp.1997-2004
#' @keywords internal misc
#' @export
mcenMLE <- function(x, method="MLE", alpha=0.4) {
  ## Coding history:
  ##    204Sep06 DLLorenz original coding from mdlROS
  ##
  method <- match.arg(method, c("MLE", "log MLE"))
  if(class(x) != "list")
    x <- censpp(x, a=alpha)
  step1 <- Surv(c(x$x, rep(NA, length(x$xcen)), x$xrcn), 
                c(x$x, x$xcen, rep(NA, length(x$xrcn))),
                type="interval2")
  if(method == "MLE") {
    step2 <- survreg(step1 ~ 1, dist="gaussian")
    coefs <- as.vector(c(step2$coefficients, step2$scale))
    step3 <- qnorm(x$ppcen) *  coefs[2L] + coefs[1L]
	step4 <- qnorm(x$pprcn) *  coefs[2L] + coefs[1L]
    step5 <- as.vector(c(step3, x$x, step4))
    retval <- list(mean=coefs[1L], sd=coefs[2L], fitted=step5)
  }
  else {
    step2 <- survreg(step1 ~ 1, dist="lognormal")
    coefs <- as.vector(c(step2$coefficients, step2$scale))
    step3 <- qnorm(x$ppcen) *  coefs[2L] + coefs[1L]
	step4 <- qnorm(x$pprcn) *  coefs[2L] + coefs[1L]
    step5 <- as.vector(c(exp(step3), x$x, exp(step4)))
    retval <- list(meanlog=coefs[1L], sdlog=coefs[2L], fitted=step5)
  }
  if(length(x$xcen) > 0L)
    retval$censorlevels <- c(x$xcen, x$xrcn)
  else
    retval$censorlevels  <- -Inf
  return(retval)
}
