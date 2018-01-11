#' Arithmetic Mean
#'
#' Computes the arithmetic mean of water-quality data. This function is intended 
#'primarily to compute a useful mean of a small set of data, for example to 
#'compute the mean of daily samples. See \bold{Note}.
#'
#' The \code{lt.tol} argument can be used to collapse interval censored data, creating an
#'uncensored value, when the range of interval censored data is less than \code{lt.tol}
#'time the detection limit. This generally results in reasonable estimates of values for 
#'the intended purpose.
#'
#' @param x the water-quality data object.
#' @param lt.tol an arbitrary tolerance metric to drop interval-censoring. See \bold{Details}.
#' @param \dots further arguments passed to or from other methods.
#' @return The mean of thw water-quality data as class "qw."
#' @note The mean and standard deviation of censored water-quality data should be computed
#'using \code{censStats}.
#'@seealso \code{\link{censStats}}
#'@keywords internal censored univariate
#' @export
#'@method mean qw
mean.qw <- function(x, lt.tol=.5, ...) {
  if(length(x) <= 1L)
    return(x) # that's easy
  retval <- x[1L] # Set up the structure for the return value
  Vals <- colMeans(x@.Data, na.rm=TRUE)
  ## All Missing?
  if(is.na(Vals[1L]))
    return(retval) # Must be missing too
  ## Check for collapsable data
  if(Vals[1L] > 0 && Vals[2L] < Inf && Vals[1L] < Vals[2L]) {
    DL <- retval@reporting.level
    if(is.na(DL))
      DL <- min(x@.Data[, 2L]) # best guess
    if(any(x@remark.codes == "<") && (Vals[2L] - Vals[1L]) < lt.tol * DL) {
      Vals[1L] <- (Vals[1L] + Vals[2L])/2
      Vals[2L] <- Vals[1L]
    }
  }
  retval@.Data[1L, ] <- Vals
  if(Vals[1L] > 0)
    retval@remark.codes <- ""
  return(retval)
}
