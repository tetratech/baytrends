#' Arithmetic Mean
#'
#' Computes the arithemtic mean. These functions return \code{NA} and are 
#'designed to stop the user from accidentally using the function \code{mean} 
#'on censored data.
#'
#' @aliases mean.lcens mean.mcens
#' @param x the censored data object.
#' @param \dots not used, require for other methods.
#' @return An error message.
#' @note The mean and standard deviation of censored data should be computed
#'using \code{censStats}.
#' @seealso \code{\link{censStats}}
#' @keywords censored univariate
#' @rdname meanStats
#' @export
#' @method mean lcens
mean.lcens <- function(x, ...) {
  warning("mean not supported for objects of class 'lcens' use censStats")
	return(NA_real_)
}

#' @rdname meanStats
#' @export
#' @method mean mcens
mean.mcens <- function(x, ...) {
  warning("mean not supported for objects of class 'mcens' use censStats")
	return(NA_real_)
}