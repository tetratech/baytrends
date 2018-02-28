#'Print Results
#'
#'Print the results of an AMLE/MLE regression.
#'
#'@param x an object of class "censReg"---output from \code{censReg}
#'@param digits the number of significant digits to print
#'@param \dots further arguments passed to or from other methods.
#'@return The object \code{x} is returned invisibly.
#'@note The printed output includes the call, the coefficent table, the
#'estimated residual standard error, the log-likelihood of the model and
#'null model with the attained p-value, and the computational method.
#@seealso \code{\link{censReg}}, \code{\link{coef.censReg}}
#'@keywords internal utilities
#'
#' @export
#'@method print censReg
print.censReg <- function(x, digits=4, ...) {
  ## Coding history:
  ##    2012Sep25 DLLorenz Initial Coding
  ##    2012Dec31 DLLorenz Roxygenized
  ##    2013Jan21 DLLorenz Added nobs and ncen
  ##    2015Jan30 DLLorenz Added error messages
  cat("Call:\n")
  dput(x$call)
  if(x$IERR == -201L) {
    warning("Excessive censoring, greater than 80%")
  } else if(x$IERR == -202L) {
    warning("Excessive censoring, fewer than 3 uncensored values for each parameter")
  } else if(x$IERR == 1L) {
    stop("Too many parameters")
  } else if(x$IERR == 2L) {
    stop("Too many observations")
  } else if(x$IERR == 201L) {
    stop("Excessive censoring, greater than 90%")
  } else if(x$IERR == 202L) {
    stop("Excessive censoring, fewer than 1.5 uncensored values for each parameter")
  } else if(x$IERR == 203L) {
    stop("Variance of uncensored values is 0")
  } else if(x$IERR > 0L) {
    stop("\nFatal error in censReg, error code: ", x$IERR, "\n")
  }
  cat("\nCoefficients:\n")
  ctab <- coef(x, summary=TRUE)
  ## round last column of table to eliminate scientific notation
  ctab[,4L] <- round(ctab[, 4L], 4)
  print(ctab, digits=digits)
  cat("\nEstimated residual standard error (Unbiased) = ",
      signif(rmse(x), digits), "\n", sep="")
  cat("Distribution: ", x$dist, "\n", sep="")
  ## If not normal, print SEE in percent and upper and lower ranges
  if(x$dist == "lognormal") {
  	MSE <- x$PARAML[x$NPAR + 1L]
  	cat("Percent standard error: ", signif(100*sqrt(exp(MSE)-1), digits),
  			"\nPositive percent error: ", signif(100*(exp(sqrt(MSE))-1), digits),
  			"\nNegative percent error: ", signif(100*(exp(-sqrt(MSE))-1), digits),
  			"\n\n", sep="")
  } else if(x$dist == "commonlog") {
  	MSE <- x$PARAML[x$NPAR + 1L] * log(10)^2 # in natural units
  	cat("Percent standard error: ", signif(100*sqrt(exp(MSE)-1), digits),
  			"\nPositive percent error: ", signif(100*(exp(sqrt(MSE))-1), digits),
  			"\nNegative percent error: ", signif(100*(exp(-sqrt(MSE))-1), digits),
  			"\n\n", sep="")
  }
  ## Compute the p-value of the regression model
  x1 <- update(x, formula=.~ 1)
  llx <- logLik(x)
  llx1 <- logLik(x1)
  chi2 <- 2*(llx - llx1)
  df <- x$NPAR - 1
  pval <- 1 - pchisq(chi2, df)
  ## Format the attained p-value, last one prevents scientific printing
  if(pval < 0.0001)
    pval <- "<0.0001"
  else
    pval <- format(round(pval, 4), scientific=5)
  if(is.logical(x$CENSFLAG)) # Only left-censored
    cat("Number of observations = ", x$NOBSC, ", number censored = ",
      sum(x$CENSFLAG), " (", round(sum(x$CENSFLAG)/x$NOBSC*100, 1),
      " percent)\n", sep="")
  else
   cat("Number of observations = ", x$NOBSC, ", number left censored = ",
      sum(x$CENSFLAG < 0), ",\n  number right censored = ", sum(x$CENSFLAG > 0),
      " (", round(sum(abs(x$CENSFLAG))/x$NOBSC*100, 1),
      " percent)\n", sep="")
  cat("\nLoglik(model) = ", signif(llx, digits),
      " Loglik(intercept only) = ", signif(llx1, digits),
      "\n  Chi-square = ", signif(chi2, digits), 
      ", degrees of freedom = ", df, ", p-value = ", pval,
      "\n\nComputation method: ", x$method, "\n\n", sep="")
  invisible(x)
}
