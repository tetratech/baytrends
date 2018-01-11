#'Extract Model Coefficients
#'
#'Extracts the model coefficients from a censored regression object: method for
#'"censReg" object.
#'
#'@param object the output from \code{censReg}.
#'@param summary include standard errors and other information?
#'@param \dots further arguments passed to or from other methods.
#'@return Either a names vector of the coefficients, if \code{summary} is
#'\code{FALSE} or a matrix of the coefficients, their standard errors,
#'z-scores, and attained p-values, if \code{summary} is \code{TRUE}.
#'@note The attained p-values are computed from the log-likelihood test for
#'AMLE regression and from a Wald chi-square test for MLE regression.
#'@seealso \code{\link{censReg}},
#' @keywords internal
#' @export
#'@method coef censReg
coef.censReg <- function(object, summary=FALSE, ...) {
  ## Coding history:
  ##    2012Sep25 DLLorenz Original Coding
  ##    2012Dec31 DLLorenz Roxygenized
  ##    2012Dec31          This version
  ##
  if(object$IERR > 0)
    return(NA)
  NPAR <- object$NPAR
  if(object$method == "AMLE")
    coef <- object$PARAML[1:NPAR]
  else
    coef <- object$PARMLE[1:NPAR]
  names(coef) <- dimnames(object$XLCAL)[[2L]] # cheat--take names from xlcal
  if(summary) { # Supply std errs, etc
    stder <-  object$STDDEV[1:NPAR]
    zscor <- coef/stder
    ## Note that the p-value is from the log likelihood test, not t or chisq
    pval <- object$PVAL[1:NPAR]
    coef <- cbind(Estimate=coef, "Std. Error"=stder, "z-score"=zscor,
                  "p-value"=pval)
  }
  coef
}
