#' Subset an Object
#'
#' Extracts or subsets a vector: methods for "lcens," "mcens," and "qw" data.
#'
#'The values for \code{i} can be either numeric indexes or logical.\cr
##'Valid slot names are "remark.codes," "reporting.level," "reporting.method,"
#'"reporting.units," "analyte.method," "analyte.name," and "unique.code."
#'
#' @rdname subset
#' @aliases [.lcens [.mcens [.qw subset.qw
#' @param x any R object to subset.
#' @param i index specifying elements to extract.
#' @param subset an logical expression involving a slot name of the qw object.
#' @param \dots not used, required for other methods.
#' @return A subset of the object \code{x}.
#' @note In general, assignment of specific values should not be done using the
#'"[" extraction operator because it does not modify the meta data assiciated
#'with a value. But any value can be set to \code{NA} by treating the value as
#'a matrix and setting the equivalent of the row to \code{NA}. See the example below.
#' @seealso \code{\link{[.qw}}
#' @keywords manip
#' @examples
#'Xcens <- as.lcens(c(1, 2, 3, 4), 2)
#'# Extract the 3rd value
#'Xcens[3]
#'# Change the 3rd value to NA and print the data
#'Xcens[3,] <- NA
#'Xcens
#' @export
#'@method [ lcens
"[.lcens" <- function(x, i, ...) {
 x@.Data <- x@.Data[i, , drop=FALSE]
 x@censor.codes <- x@censor.codes[i]
 x@names <- as.character(seq(length(x@censor.codes)))
 return(x)
}

#' @export
#'@method [ mcens
#'@rdname subset
"[.mcens" <- function(x, i, ...) {
  x@.Data <- x@.Data[i, , drop=FALSE]
  x@censor.codes <- x@censor.codes[i]
  x@interval <- x@interval[i]
  x@names <- as.character(seq(length(x@interval)))
  return(x)
}

#' @export
#'@method [ qw
#'@rdname subset
"[.qw" <- function(x, i, ...) {
  x@.Data <- x@.Data[i, , drop=FALSE]
  x@remark.codes <- x@remark.codes[i]
  x@value.codes <- x@value.codes[i]
  x@reporting.level <- x@reporting.level[i]
  x@reporting.method <- x@reporting.method[i]
  x@reporting.units <- x@reporting.units[i]
  x@analyte.method <- x@analyte.method[i]
  x@analyte.name <- x@analyte.name[i]
  x@unique.code <- x@unique.code[i]
  x@names <- as.character(seq(length(x@remark.codes)))
  return(x)
}

#' @export
#'@method subset qw
#'@rdname subset
subset.qw <- function(x, subset, ...) {
  if(missing(subset))
    return(x)
  xt <- as.data.frame(x, expand=T, nm="x")
  names(xt) <- c("va1", "va2", "remark.codes", "value.codes", "reporting.level",
                 "reporting.method", "reporting.units", "analyte.method", 
                 "analyte.name", "unique.code")
  i <- eval(substitute(subset), xt, parent.frame())
  x[na2miss(i, FALSE)]
}
