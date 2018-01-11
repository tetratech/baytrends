#' Extract Log-Likelihood
#'
#' Extracts the log-likelihood statistic from a censored regression object: 
#'method for "censReg" object.
#'
#'@param object the output from \code{censReg}.
#'@param \dots further arguments passed to or from other methods.
#'@return An object of class "logLik" containing the log-likelihood and
#'the attributes "df" (degrees of freedom) and "nobs" (number of observations).
#'@keywords internal
#'@seealso \code{\link{censReg}},
#' @export
#'@method logLik censReg
logLik.censReg <- function(object, ...) {
  ## Coding history:
  ##    2012Sep25 DLLorenz Original Coding
  ##    2012Dec31 DLLorenz Roxygenized
  ##    2012Dec31          This version
  ##
  ll <- object$LLR
  attr(ll, "df") <- object$NPAR + 1
  attr(ll, "nobs") <- object$NOBSC
  class(ll) <- "logLik"
  return(ll)
}
