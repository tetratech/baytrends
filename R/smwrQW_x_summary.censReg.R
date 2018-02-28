#' Summarizing Linear Model Fits
#'
#' Summarizes the output from a censored regression object: method 
#'for "censReg" object.
#'
#' @param object an object of class "censReg"---output from \code{censReg}.
#' @param correlation include the correlation matrix of the estimated
#'parameters?
#' @param \dots further arguments passed to or from other methods.
#' @return An object of class "summary.censReg" containing the object, 
#'the pseudo R-squared, variance inflation factors, a table of diagnostic 
#'statistics, critical values for selected diagnostic statistics, 
#'an indication of which observations exceed any of the selected 
#'diagnostic statistics, and optionally the parameter correlation matrix.
#' @note The pseudo R-squared is computed using the McKelvey-Zavoina (1975) method,
#'which tries to describe the proportion of variance explained by the fit and
#'tries to capture the square of the correlation between the fitted and
#'actual values. For uncensored data, it is equal to the usual R-squared 
#'for ordinary least squares.
#' @references McKelvey, R.D., and Zavoina, W., 1975, A statistical model for
#'the analysis of ordinal dependent variables: The Journal of Mathematical
#'Sociology, v. 4, no. 1, p. 103--120.
# @seealso \code{\link{censReg}}
#' @keywords internal regression
#' @export
#' @method summary censReg
summary.censReg <- function(object, correlation=FALSE, ...) {
  ## Coding history:
  ##    2012Dec31 DLLorenz Initial Coding
  ##    2014Jan06 DLLorenz Added pseudo R2
  ##    2014Oct16 DLLorenz bug fix on flagobs
  ##    2015Jan30 DLLorenz Added error stop
  ##
  if(object$IERR > 0L) {
    stop("\nFatal error in censReg, error code: ", object$IERR, "\n\n")
  }
  ## Pseudo R-squared McKelvey-Zavoina method, which tries to replicate
  ## the R-squared of OLS
  vmat <- var(object$XLCAL[,-1, drop=FALSE])
  ## Correct to Sum of squares
  vmat <- vmat * (object$NOBSC - 1)
  cf <- object$PARAML[seq(2L, object$NPAR)]
  ## Regression sum-of-squares
  RSS <- t(cf) %*% vmat %*% cf
  ## Error sum-of-squares
  ESS <- object$PARMLE[object$NPAR + 1L] * object$NOBSC
  R2 <- RSS/(ESS+RSS)
  ## Construct the diagnostics table
  diagstats <- data.frame(Y=object$YLCAL,
                          ycen=object$CENSFLAG,
                          yhat=fitted(object, suppress.na.action=TRUE),
                          resids=residuals(object, suppress.na.action=TRUE),
                          leverage=residuals(object, suppress.na.action=TRUE,
                            type="leverage"),
                          cooksD=residuals(object, suppress.na.action=TRUE,
                            type="influence"))
  names(diagstats)[1L] <- make.names(deparse(object$terms[[2L]]))
  ## Compute critical values for diagnostic statistics and pick out offending
  ##  observations
  p <- object$NPAR - 1
  n <- object$NOBS
  cvlev <- 3*p/n
  cvcook <- qf(.5,p+1,n-p)
  cvs <- c(leverage=cvlev, cooksD=cvcook)
  pck <- c(diagstats$leverage > cvlev | diagstats$cooksD > cvcook)
  ## Pack it up
  retval <- list(object=object, vif=vif(object), R2=R2,
                 diagstats=diagstats, crit.val=cvs, flagobs=pck)
  if(correlation) {
    corr <- cov2cor(object$COV)
    Nm <- c(colnames(object$XLCAL), "Scale")
    dimnames(corr) <- list(Nm, Nm)
    retval$correlation <- corr
  }
  class(retval) <- "summary.censReg"
  return(retval)
}
