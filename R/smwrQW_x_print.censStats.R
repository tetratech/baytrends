#'Print Results
#'
#'Print the output of \code{censStats}.
#'
#' @param x an object of class "censStats."
#' @param digits the number of significant digits to print
#' @param \dots further arguments passed to or from other methods.
#' @return The object \code{x} is returned invisibly.
#' @note The printed includes the mean and standard deviation of the data and
#'if a log-transform was used in the computation, then the mean and standard
#'deviation of the log-transformed values.
#' @keywords internal utilities
#'
#' @export
#' @method print censStats
print.censStats <- function(x, digits=4, ...) {
  ## Coding history:
  ##    2013Jan05 DLLorenz Initial Coding
  ##
  mn <- format(x$mean, digits=digits)
  sd <- format(x$sd, digits=digits)
  print(c(mean=mn, "std. dev."=sd), quote=FALSE)
  if(!is.null(x$meanlog)) {
    cat("Statistics for the log transforms:\n")
    mn <- format(x$meanlog, digits=digits)
    sd <- format(x$sdlog, digits=digits)
    print(c(mean=mn, "std. dev."=sd), quote=FALSE)
  }
  invisible(x)
}
