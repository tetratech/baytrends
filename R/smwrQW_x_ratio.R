#' Compute Ratios
#'
#' Computes ratios from water-quality 
#'or other data and maintain the integrity of the data, as opposed to methods that use
#'simple substitution
#'
#' Objects of class "qw" are converted to class "mcens" using the \code{qw2mcens} 
#'function, which preserves the minimum value of 0 for "less than" data rather 
#'than \code{as.mcens}, which converts the to left-censored values.
#'
#' @param numerator any valid vector that can be converted to class "mcens." Missing values
#'are permitted but result in missing values in the output.
#' @param denominator any valid vector that can be converted to class "mcens." Missing values
#'are permitted but result in missing values in the output.
#' @return An object of class "mcens" that is the result of the requested division.
#' @keywords internal
#' @export ratio
ratio <- function(numerator, denominator) {
	# Force to mcens
	if(inherits(numerator, "qw")) {
		numerator <- qw2mcens(numerator)
	} else
		numerator <- as.mcens(numerator)
	if(inherits(denominator, "qw")) {
		denominator <- qw2mcens(denominator)
	} else
		denominator <- as.mcens(denominator)
	# extract the necessary values and compute the ratio
	numerator <- numerator@.Data
	denominator <- denominator@.Data
	lower.val <- numerator[, 1L]/ denominator[, 2L]
	upper.val <- numerator[, 2L]/ denominator[, 1L]
	return(as.mcens(lower.val, upper.val))
}