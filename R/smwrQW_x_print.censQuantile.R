#'Print Results
#'
#'Print the output of \code{censQuantile}.
#'
#' @param x an object of class "censQuantile."
#' @param digits the number of significant digits to print
#' @param \dots further arguments passed to or from other methods.
#' @return The object \code{x} is returned invisibly.
#' @note The printed output appears as 2 columns, the first column is the
#'percentile and the second column is the value.
#' @keywords utilities
#'
#' @export
#' @method print censQuantile
print.censQuantile <- function(x, digits=4, ...) {
  ## Coding history:
  ##    2013Jan05 DLLorenz Initial Coding
  ##
  x2pr <- lapply(x, format,  digits=digits)
  for(i in names(x2pr))
    cat(i, ": ", x2pr[[i]], "\n", sep="")
  invisible(x)
}
