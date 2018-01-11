#' @title Power Transforms
#'
#' @description Raises censored data to a positive power.
#'
#' @include a2_smwrQW_lcens-class.R a3_smwrQW_mcens-class.R a1_smwrQW_qw-class.R
#' @param x any object that can be converted to class "mcens" or "lcens." Must be non negative (zero data are
#'permitted). Missing values are permitted and missing vlues in the returnned vector.
#' @param lambda the power to raise \code{x}. Must be strictly positive.
#' @param out the output format, must be one of "numeric," "lcens," "mcens," or "Auto." If "Auto," then if
#'\code{x} is class "numeric," then the output format is "numeric" otherwise the output format is "mcens."
#' @return An object of class "mcens" that is the result of the requested operations. Each of the range of the 
#'lower and upper values are raised to the power of \code{lambda} and the result divided by \code{lambda}
#'to approximately maintain scale. But the reciprocal value of \code{lambda} cannot be used to restore the 
#'original values.
#' @keywords internal
#' @examples
#' pow(as.mcens(c(0,2), c(1,2)), 2)
#' pow(as.mcens(c(0,2), c(1,2)), .5)
#' # Numeric values
#' pow(c(1,3), 2)
#' @export pow
pow <- function(x, lambda, out="Auto") {
	# Checks
	if(lambda <= 0) {
		stop("lambda must be greater than 0")
	}
	out <- match.arg(out, c("Auto", "mcens", "lcens", "numeric"))
	# Process the data
	# This should set up for the conversion in the following section of code
	if(out == "numeric") {
		if(censoring(x) != "none") {
			stop("censored values cannot be output as numeric")
		}
		if(class(x) %in% c("mcens", "'lcens")) { # no as.numeric converter
			x <- x@.Data[,1]
		} else {
			x <- as.numeric(x)
		}
	}
	if(inherits(x, "numeric") & out %in% c("Auto", "numeric")) {
		if(any(x < 0)) {
			stop("data must be non negative")
		}
		return(x^lambda/lambda)
	} # Done with numeric output
	if(out == "lcens" || (out == "Auto" && inherits(x, "lcens"))) {
		if(censoring(x) == "multiple") {
			stop("data cannot by multiply censored for output as lcens")
		}
		x <- as.lcens(x)
		if(any(x@.Data[, 1L] < 0)) {
			stop("data must be non negative")
		}
		x@.Data[,1L] <- x@.Data[,1L]^lambda/lambda
		x@.Data[,2L] <- ifelse(x@.Data[,2L] > 0, x@.Data[,2L]^lambda/lambda, x@.Data[,2L])
		return(x)
	} # Done with lcens output
	if(inherits(x, "qw")) { # use qw2mcens
		retval <- qw2mcens(x)
	} else if(inherits(x, "mcens")) {
		retval <- x
	} else { # convert it
		retval <- as.mcens(x)
	}
	# Checks for mcens
	if(any(retval@censor.codes < 0, na.rm=TRUE)) {
		stop("mcens data must not be left-censored")
	}
	if(any(retval@.Data[, 1L] < 0, na.rm=TRUE)) {
		stop("data must be non negative")
	}
	# OK, do it
	retval@.Data <- retval@.Data^lambda/lambda
  return(retval)
}
