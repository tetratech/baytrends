#'Coerce to a Data Frame
#'
#'Creates data frames from censored and water-quality data: 
#'methods for "lcens," "mcens," and "qw" data.
#'
#' @aliases as.data.frame.lcens as.data.frame.mcens as.data.frame.qw
#' @param x any R object to put into a data frame.
#' @param row.names NULL or a character vector giving the row names for the data
#'frame. Missing values are not allowed.
#' @param optional logical. If \code{TRUE}, set column names to \code{nm}.
#' @param \dots additional arguments to be passed to or from methods.
#' @param nm set as column name if \code{optional} is \code{TRUE}.
#' @param expand create a data frame from all of the slots from the object
#'instead of creating a data frame containting the object.
#' @return A data frame constructred from \code{x}.
#' @seealso \code{\link{data.frame}}
#' @keywords internal manip
#' @rdname as.data.frame
#' @export
#' @method as.data.frame lcens
as.data.frame.lcens <- function(x, row.names = NULL, optional = FALSE,
                                ..., nm = deparse(substitute(x))) {
  force(nm)
  nrows <- length(x)
  if(is.null(row.names)) {
    if(nrows == 0L) 
      row.names <- character(0L)
    else
      row.names <- .set_row_names(nrows)
  }
  retval <- list(x)
  if(!optional) 
    names(retval) <- make.names(nm)
  attr(retval, "row.names") <- row.names
  class(retval) <- "data.frame"
  retval
}

#' @rdname as.data.frame
#' @export
#' @method as.data.frame mcens
as.data.frame.mcens <- function(x, row.names = NULL, optional = FALSE,
                                ..., nm = deparse(substitute(x))) {
  force(nm)
  nrows <- length(x)
  if(is.null(row.names)) {
    if(nrows == 0L) 
      row.names <- character(0L)
    else
      row.names <- .set_row_names(nrows)
  }
  retval <- list(x)
  if(!optional) 
    names(retval) <- make.names(nm)
  attr(retval, "row.names") <- row.names
  class(retval) <- "data.frame"
  retval
}

#' @rdname as.data.frame
#' @export
#' @method as.data.frame qw
as.data.frame.qw <- function (x, row.names = NULL, optional = FALSE, ...,
                              nm = deparse(substitute(x)),
                              expand = FALSE) {
  force(nm)
  nrows <- length(x)
  if (is.null(row.names)) {
    if (nrows == 0L) 
      row.names <- character(0L)
    else
      row.names <- .set_row_names(nrows)
  }
  if(expand) { # expand each slot to create a full-detail data set
    retval <- data.frame(x@.Data[,1], x@.Data[,2], x@remark.codes, x@value.codes,
                         x@reporting.level, x@reporting.method, x@reporting.units,
                         x@analyte.method, x@analyte.name, x@unique.code,
                         stringsAsFactors=FALSE)
    names(retval) <- paste(nm, c("va1", "va2", "rmk", "vqc", "rlv", "rmt",
                                 "unt", "mth", "nam", "pcd"), sep=".")
  } else {
    retval <- list(x)
    if(!optional) 
      names(retval) <- nm
  }
  attr(retval, "row.names") <- row.names
  class(retval) <- "data.frame"
  retval
}
