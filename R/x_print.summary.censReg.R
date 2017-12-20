#'Print Results
#'
#'Print the summary output of an AMLE/MLE regression.
#'
#' @param x an object of class "censReg"---output from \code{censReg}
#' @param digits the number of significant digits to print
#' @param \dots further arguments passed to or from other methods.
#' @return The object \code{x} is returned invisibly.
#' @note The printed output includes the call, the coefficent table, the
#'estimated residual standard error, the log-likelihood of the model and
#'null model with the attained p-value, and the computational method.
#' @seealso \code{\link{censReg}}, \code{\link{coef.censReg}}
#' @keywords utilities
#'
#' @export
#' @method print summary.censReg
print.summary.censReg <- function(x, digits=4, ...) {
  ## Coding history:
  ##    2012Dec31 DLLorenz Initial Coding
  ##
  print(x$object, digits=digits)
  ## Print Pseudo R-squared
  cat("Pseudo R-squared: ", signif(x$R2, digits), "\n\n", sep="")
  ## Add model comparison stats:
  cat("  AIC: ", signif(AIC(x$object), digits),
    "\n  BIC: ", signif(BIC(x$object), digits),
    "\n\n", sep="")
  if(length(x$vif) > 1L) {
    cat("\nVariance inflation factors\n")
    namvif <- format(names(x$vif), justify = "right")
    valvif <- format(round(x$vif, 2), justify = "right")
    for (i in seq(along = x$vif)) cat(namvif[i], " ", valvif[i], 
    																	"\n", sep = "")  
    ## Only makes sense to print correlations if more than 1 explanatory var.
    if(!is.null(x$correlation)) {
      corr <- format(round(x$correlation, digits))
      corr[upper.tri(corr, diag=TRUE)] <- ""
      print(corr[-1L, -ncol(corr), drop=FALSE], quote=FALSE)
    }
  }
  cat("\nTest criteria\n")
  print(x$crit.val, digits=digits)
  if(any(x$flagobs)) {
    cat("\tObservations exceeding at least one test criterion\n")
    print(x$diagstats[x$flagobs,], digits=digits)
  }
  else
    cat("\tNo observations exceeded any test criteria\n")
  invisible(x)
}
