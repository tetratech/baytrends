#' Missing Values
#'
#' Indicates which elements are missing: methods for "lcens," "mcens," and "qw" data.  From smwrQW package.
#'
#'
#' @aliases is.na.lcens is.na.mcens is.na.qw
#' @param x the object to be tested.
#' @return A logical vector of the same length as its argument \code{x},
#'containing TRUE for those elements marked NA and FALSE otherwise.
#' @keywords manip
#' @examples
#'
#'is.na(as.lcens(c(1,3, NA), 2))
#'
#' @rdname is.na
#' @export
#' @method is.na lcens
is.na.lcens <- function(x) {
  ## Any value or censor code missing is missing data, detection limits can be missing!
  return(is.na(x@.Data[, 1]) | is.na(x@censor.codes))
}

#' @rdname is.na
#' @export
#' @method is.na mcens
is.na.mcens <- function(x)
  return(is.na(x@.Data[, 1L]))

#' @rdname is.na
#' @export
#' @method is.na qw
is.na.qw <- function(x) {
  ## Any pair value
  return(is.na(x@.Data[, 1]) & is.na(x@.Data[, 2]))
}
