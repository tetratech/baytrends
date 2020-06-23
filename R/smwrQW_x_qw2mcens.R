#' Conversion to class "mcens"
#'
#' Converts objects of class "qw" to "mcens" retaining interval censoring
#'for "less-than" data.
#'
#' @param x an object of class "qw" to convert.
#' @return A vector of the data converted to class "mcens" retaining interval 
#'censoring for "less-than" data.
#' @seealso \code{\link{as.mcens}}
#' @keywords internal
#' @export
qw2mcens <- function(x) {
	## Check, then proceed
	if(class(x)[1L] != "qw")
		stop("x is not class 'qw'")
	mat <- x@.Data
	## Compute the censor.codes and interval
	censor.codes <- rep(0L, length(x))
	censor.codes[mat[, 1L] == -Inf] <- -1L
	censor.codes[mat[, 2L] == Inf] <- 1L
	interval <- censor.codes == 0L & (mat[, 1L] < mat[, 2L])
	retval <- new("mcens", mat, censor.codes=as.integer(censor.codes),
								interval=interval,
								names="")
	retval@names <- as.character(seq(nrow(mat)))
	return(retval)
}
