#' Encode in a Common Format
#'
#' Formats data for pretty printing: methods for "lcens," "mcens," and "qw" data.
#'
#' @details The argument \code{style} must be one of "asis," which tries to format
#'the data to best show the values; "simple," which is like "asis" except that interval
#'censored data are the midrange values preceded by "I"; or "interpreted," which shows 
#'the values as they would be used in analysis and \code{round} is ignored.
#'Only the first letter is required.
#'
#' @aliases format.lcens format.mcens format.qw
#' @param x the censored data object to format.
#' @param digits how many significant digits are to be used for numbers?
#' @param \dots further arguments passed to or from other methods.
#' @param units logical, if \code{TRUE} then include the units of the data.
#' @param style the style for objects of class "qw." See \bold{Details}.
#' @param round round the data before formating? Can also be a numeric vector of
#'length 2 containing the maximum number of significant digits and the maximum
#'number of decimal digits.
#' @note For \code{style} set to "asis," it is often useful to set \code{round} to a
#'single value so that the interval data are not rounded to the same value.
#' @return A character representations of the elements of \code{x}.
#' @keywords manip
#' @examples
#'
#'format(as.lcens(1:3, 1))
#'format(as.mcens(1:3, 1:3))
#'
#' @rdname format
#' @export
#' @method format lcens
format.lcens <- function(x, digits=NULL, ...) {
  if(!is.null(digits))
    x@.Data <- signif(x@.Data, digits)
  retval <- as.character(x)
  return(format(retval, ...))
}

#' @rdname format
#' @export
#' @method format mcens
format.mcens <- function(x, digits=NULL, ...) {
  if(!is.null(digits))
    x@.Data <- signif(x@.Data, digits)
  retval <- as.character(x)
  return(format(retval, ...))
}

#' @rdname format
#' @export
#' @method format qw
format.qw <- function(x, round=TRUE, units=FALSE, style="asis", ...) {
	style <- match.arg(style, c("asis", "simple", "interpreted"))
  xval <- x@.Data
	xmid <- rowMeans(xval)
  xrl <- x@reporting.level
  xrmk <- x@remark.codes
  xrmk[is.na(xrmk)] <- " " # fix NAs in the remark codes
	# Create I remark codes if necessary
	Icens <- na2miss(xval[,1L] < xval[,2L] & !(xrmk == "<" | xrmk == ">"), FALSE)
	xrmk[Icens] <- "I"
  ## Force detection limit on left-censored data if style is interpreted
	if(style == "interpreted") {
		fdl <- ifelse(is.na(xrl), FALSE,
									ifelse(xval[, 2L] >= xrl, FALSE, TRUE))
		fdl <- na2miss(fdl, FALSE)
		if(any(fdl)) {
			xval[fdl, 2L] <- xrl[fdl]
			xrmk[fdl] <- "<"
		}
	}
	if(style != "interpreted") {
		if(is.logical(round)) {
			if(round) {
				rnd <- x@rounding
				xval <- round(signif(xval, rnd[1L]), rnd[2L])
				xmid <- round(signif(xmid, rnd[1L]), rnd[2L])
			}
		}
		else if(length(round) == 2L) { # Standard qw usage
			xval <- round(signif(xval, round[1L]), round[2L])
			xmid <- round(signif(xmid, round[1L]), round[2L])
		}
		else {
			xval <- round(xval, round)
			xmid <- round(xmid, round)
		}
	}
  xval <- format(xval, ...) # xval is now character matrix
  ## Interval censoring
	if(style != "simple") {
		xsho <- ifelse(xrmk == "I", xval[, 2L], xval[, 1L])
		xrmk <- ifelse(xrmk == "I", paste(xval[, 1L], "-", sep=''), xrmk)
	} else {
		xsho <- ifelse(xrmk == "I", xmid, xval[, 1L])
	}
  ## Left censoring
  xsho <- ifelse(xrmk == "<", xval[, 2L], xsho)
  retval <- paste(xrmk, xsho, sep='')
  if(units)
    retval <- paste(retval, x@reporting.units, sep=' ')
  return(retval)
}
