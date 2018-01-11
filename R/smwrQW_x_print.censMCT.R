#'Print Results
#'
#'Print the results of mutliple nonparametric comparison tests among groups 
#'of left-censored data.
#'
#' @param x an object of class "censMCT"---output from \code{censMulticomp.test}
#' @param digits the number of significant digits to print
#' @param \dots further arguments passed to or from other methods.
#' @return The object \code{x} is returned invisibly.
#' @note The printed output includes the name of the test, the error rate, the
#'method for adjusting the p-values for the individual tests, the names of the
#'response and group variables, and a table of the paired comparisons. The 
#'table includes the names of the groups comapred, the z-score from the
#'\code{genWilcox.test}, the adjusted p-value, and a column to indicate if the
#'adjusted p-value is less than the \code{alpha} level set in
#'\code{censMulticomp.test}.
#' @seealso \code{\link{censKSample.test}}, \code{\link{censMulticomp.test}},
#'\code{\link{genWilcox.test}}
#' @keywords internal utilities
#'
#' @export
#' @method print censMCT
print.censMCT <- function(x, digits=4, ...) {
  ## Coding history:
  ##    2012Sep18 DLLorenz Initial Coding
  ##    2012Dec31 DLLorenz Roxygenized
  ##    2012Dec31          This version
  ##
  cat("\t", x$title, "\n")
  if(x$method == "none")
    cat("Pairwise")
  else
    cat("Overall")
  cat(" error rate: ", x$alpha, "\nAttained P-values adjusted by the ", x$method,
      " method\n\n", sep="")
  cat("Response variable: ", x$response, "\nGroup variable: ", x$groups,
      "\n\n", sep="")
  cat("Table of paired comparisons, attained p-values less than",
      x$alpha, "are flagged by '*'\n", sep=" ")
  pmat <- format(round(x$table, digits))
  flag <- ifelse(x$table[, 2L] < x$alpha, "*", " ")
  flag[is.na(flag)] <- " "
  pmat <- cbind(pmat, flag=flag)
  print(pmat, quote=FALSE)
  cat("\n")
  invisible(x)
}
