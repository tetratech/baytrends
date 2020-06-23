#' Combine Values into a Vector
#'
#' Combines arguments to form a vector for censored or water-quality data: 
#'methods for "cens," "mcens," and "qw" data. All arguments are coerced to
#'a common type which is the type of the first argument, except for the qw method,
# which requires all arguments to be of class "qw."
#'
#' @aliases c.lcens c.mcens c.qw
#' @param \dots any objects that can be coerced to the class of the first one.
#' @param recursive not used.
#' @return A vector of an appropriate class.
#' @keywords internal
#' @examples
#'
#'c(as.lcens(c(1,3), 2), 2)
#'
#' @rdname c
#' @export
#' @method c lcens
c.lcens <- function (..., recursive=FALSE) {
  all.lcens <- lapply(list(...), as.lcens)
  vals <- unlist(lapply(all.lcens, function(x) x@.Data[, 1]))
  dls <- unlist(lapply(all.lcens, function(x) x@.Data[, 2]))
  ccds <- unlist(lapply(all.lcens, function(x) x@censor.codes))
  mat <- cbind(values=vals, detlim=dls)
  retval <- new("lcens", mat, censor.codes=ccds)
  retval@names <- as.character(seq(nrow(mat)))
  return(retval)
}

#' @rdname c
#' @export
#' @method c mcens
c.mcens <- function (..., recursive=FALSE) {
  all.mcens <- lapply(list(...), as.mcens)
  lower.val <- unlist(lapply(all.mcens, function(x) x@.Data[, 1L]))
  upper.val <- unlist(lapply(all.mcens, function(x) x@.Data[, 2L]))
  censor.codes <- unlist(lapply(all.mcens, function(x) x@censor.codes))
  interval <- unlist(lapply(all.mcens, function(x) x@interval))
  mat <- cbind(lower.val=lower.val, upper.val=upper.val)
  retval <- new("mcens", mat, censor.codes=censor.codes, interval=interval)
  retval@names <- as.character(seq(nrow(mat)))
  return(retval)
}

#' @rdname c
#' @export
#' @method c qw
c.qw <- function (..., recursive=FALSE) {
  all.qw <- list(...)
  lower.val <- unlist(lapply(all.qw, function(x) x@.Data[, 1L]))
  upper.val <- unlist(lapply(all.qw, function(x) x@.Data[, 2L]))
  remark.codes <- unlist(lapply(all.qw, function(x) x@remark.codes))
  value.codes <- unlist(lapply(all.qw, function(x) x@value.codes))
  reporting.level <- unlist(lapply(all.qw, function(x) x@reporting.level))
  reporting.method <- unlist(lapply(all.qw, function(x) x@reporting.method))
  reporting.units <- unlist(lapply(all.qw, function(x) x@reporting.units))
  analyte.method <- unlist(lapply(all.qw, function(x) x@analyte.method))
  analyte.name <- unlist(lapply(all.qw, function(x) x@analyte.name))
  unique.code <- unlist(lapply(all.qw, function(x) x@unique.code))
  mat <- cbind(values=lower.val, value2=upper.val)
  retval <- new("qw", mat, remark.codes=remark.codes, value.codes=value.codes,
  							reporting.level=reporting.level, reporting.method=reporting.method,
  							reporting.units=reporting.units, analyte.method=analyte.method,
  							analyte.name=analyte.name, unique.code=unique.code, rounding=c(2,3),
  							names=as.character(seq(length(lower.val))))
  # Not sure why necessary
  retval@names <- as.character(seq(length(lower.val)))
  return(retval)
}
