#' @title Comparison Methods for \code{lcens}, \code{mcens}, and \code{qw} objects
#'
#' @description Comprisons are possible and well-defined between numeric and
#'censored or water-quality data. Comparisons between censored data types are
#'not supported in this version.
#'
#' @details For all comparisons, the censored data are converted to multiply censored data,
#'class "mcens." The conversion from class "qw" preserves the strict interpretation
#'of less-than values as greater than or equal to 0 and less than the reporting level.
#'The distinction of being strictly less than is lost for the conversion of left-censored
#'data, class "lcens."
#'
#' @include a3_smwrQW_mcens-class.R a2_smwrQW_lcens-class.R a1_smwrQW_qw-class.R
#' @name Compare-censored
#' @param e1,e2 numeric, censored, or water-quality data. Missing values are permitted in 
#'either argument and result in a missing value in the output.
#' @return A logical vector the represents the result of the comparison between each
#'element in \code{e1} and \code{e2}.
#' @keywords internal methods manip
#' @exportMethod Compare
#' @examples
#'as.lcens(c(1, 3), 2) > 1
#'as.lcens(c(1, 3), 2) < 4
#' 

#' @rdname Compare-censored
#' @aliases Compare,lcens,numeric-method
setMethod("Compare", signature(e1 = "lcens", e2="numeric"), function(e1, e2) {
	e1 <- as.mcens(e1)
	if(.Generic %in% c("<", "<=", ">", ">=")) {
		# Tweak upper range if left censored--makes truly less than
		e1@.Data[,2L] <- ifelse(e1@.Data[,1L] == -Inf, 
														e1@.Data[,2L] - e1@.Data[,2L] * .Machine$double.eps,
														e1@.Data[,2L])
		retval <- callGeneric(e1@.Data[,1L],e2)
		ret2 <- callGeneric(e1@.Data[,2L],e2)
		return(ifelse(retval != ret2, NA, retval))
	}
	if(.Generic == "==") {
		retval <- callGeneric(e1@.Data[,2L],e2) & callGeneric(e1@.Data[,1L],e2)
		ret2 <- (e2 > e1@.Data[,1L]) & (e2 < e1@.Data[,2L]) # check in range
		retval <- ifelse(ret2, NA, retval)
		return(retval)
	} # Must be !=
	# Tweak e2 if within range of e1
	e2 <- ifelse((e1@.Data[,2L] >= e2) & (e1@.Data[,1L] <= e2) & (e1@.Data[,1L] < e1@.Data[,2L]),
							 NA, e2)
	return((e1@.Data[,2L] > e2) | (e1@.Data[,1L] < e2)) 
}
)

#' @rdname Compare-censored
#' @aliases Compare,mcens,numeric-method
setMethod("Compare", signature(e1 = "mcens", e2="numeric"), function(e1, e2) {
	if(.Generic %in% c("<", "<=", ">", ">=")) {
		retval <- callGeneric(e1@.Data[,1L],e2)
		ret2 <- callGeneric(e1@.Data[,2L],e2)
		return(ifelse(retval != ret2, NA, retval))
	}
	if(.Generic == "==") {
		retval <- callGeneric(e1@.Data[,2L],e2) & callGeneric(e1@.Data[,1L],e2)
		ret2 <- (e2 > e1@.Data[,1L]) & (e2 < e1@.Data[,2L]) # check in range
		retval <- ifelse(ret2, NA, retval)
		return(retval)
	} # Must be !=
	# Tweak e2 if within range of e1
	e2 <- ifelse((e1@.Data[,2L] >= e2) & (e1@.Data[,1L] <= e2) & (e1@.Data[,1L] < e1@.Data[,2L]),
							 NA, e2)
	return((e1@.Data[,2L] > e2) | (e1@.Data[,1L] < e2)) 
}
)

#' @rdname Compare-censored
#' @aliases Compare,qw,numeric-method
setMethod("Compare", signature(e1 = "qw", e2="numeric"), function(e1, e2) {
	e1 <- qw2mcens(e1)
	if(.Generic %in% c("<", "<=", ">", ">=")) {
		retval <- callGeneric(e1@.Data[,1L],e2)
		ret2 <- switch(.Generic,
									 "<" = ifelse(e1@.Data[,1L] < e1@.Data[,2L], 
									 						 e1@.Data[,2L] <= e2, e1@.Data[,2L]  < e2),
									 "<=" = e1@.Data[,2L] <= e2,
									 ">" =  e1@.Data[,2L] > e2,
									 ">=" = ifelse(e1@.Data[,1L] < e1@.Data[,2L],
									 							e1@.Data[,2L]  > e2, e1@.Data[,2L] >= e2))
		return(ifelse(retval != ret2, NA, retval))
	}
	if(.Generic == "==") {
		retval <- callGeneric(e1@.Data[,2L],e2) & callGeneric(e1@.Data[,1L],e2)
		ret2 <- (e2 > e1@.Data[,1L]) & (e2 < e1@.Data[,2L]) # check in range
		retval <- ifelse(ret2, NA, retval)
		return(retval)
	} # Must be !=
	e2 <- ifelse((e1@.Data[,2L] >= e2) & (e1@.Data[,1L] <= e2) & (e1@.Data[,1L] < e1@.Data[,2L]),
							 NA, e2)
	return((e1@.Data[,2L] > e2) | (e1@.Data[,1L] < e2) | 
				 	((e2 == e1@.Data[,2L]) & (e1@.Data[,1L] < e1@.Data[,2L]))) 
}
)

#' @rdname Compare-censored
#' @aliases Compare,numeric,lcens-method
setMethod("Compare", signature(e1 = "numeric", e2="lcens"), function(e1, e2) {
	e2 <- as.mcens(e2)
	if(.Generic %in% c("<", "<=", ">", ">=")) {
		# Tweak upper range if left censored--makes truly less than
		e2@.Data[,2L] <- ifelse(e2@.Data[,1L] == -Inf, 
														e2@.Data[,2L] - e2@.Data[,2L] * .Machine$double.eps,
														e2@.Data[,2L])
		retval <- callGeneric(e1, e2@.Data[,1L])
		ret2 <- callGeneric(e1, e2@.Data[,2L])
		return(ifelse(retval != ret2, NA, retval))
	}
	if(.Generic == "==") {
		retval <- callGeneric(e1, e2@.Data[,2L]) & callGeneric(e1, e2@.Data[,1L])
		ret2 <- (e1 > e2@.Data[,1L]) & (e1 < e2@.Data[,2L]) # check in range
		retval <- ifelse(ret2, NA, retval)
		return(retval)
	} # Must be !=
	# Tweak e1
	e1 <- ifelse((e2@.Data[,2L] >= e1) & (e2@.Data[,1L] <= e1) & (e2@.Data[,1L] < e2@.Data[,2L]),
							 NA, e1)
	return((e1 > e2@.Data[,2L]) | (e1 < e2@.Data[,1L])) 
}
)

#' @rdname Compare-censored
#' @aliases Compare,numeric,mcens-method
setMethod("Compare", signature(e1 = "numeric", e2="mcens"), function(e1, e2) {
	if(.Generic %in% c("<", "<=", ">", ">=")) {
		retval <- callGeneric(e1, e2@.Data[,1L])
		ret2 <- callGeneric(e1, e2@.Data[,2L])
		return(ifelse(retval != ret2, NA, retval))
	}
	if(.Generic == "==") {
		retval <- callGeneric(e1, e2@.Data[,2L]) & callGeneric(e1, e2@.Data[,1L])
		ret2 <- (e1 > e2@.Data[,1L]) & (e1 < e2@.Data[,2L]) # check in range
		retval <- ifelse(ret2, NA, retval)
		return(retval)
	} # Must be !=
	e1 <- ifelse((e2@.Data[,2L] >= e1) & (e2@.Data[,1L] <= e1) & (e2@.Data[,1L] < e2@.Data[,2L]),
							 NA, e1)
	return((e1 > e2@.Data[,2L]) | (e1 < e2@.Data[,1L])) 
}
)

#' @rdname Compare-censored
#' @aliases Compare,numeric,qw-method
setMethod("Compare", signature(e1 = "numeric", e2="qw"), function(e1, e2) {
	e2 <- qw2mcens(e2)
	if(.Generic %in% c("<", "<=", ">", ">=")) {
		retval <- callGeneric(e1, e2@.Data[,1L])
		ret2 <- switch(.Generic,
									 "<" = e1 < e2@.Data[,2L],
									 "<=" = ifelse(e2@.Data[,1L] < e2@.Data[,2L], 
									 							e1 < e2@.Data[,2L], e1 <= e2@.Data[,2L]),
									 ">" = ifelse(e2@.Data[,1L] < e2@.Data[,2L],
									 						 e1 >= e2@.Data[,2L], e1 > e2@.Data[,2L]),
									 ">=" = e1 >= e2@.Data[,2L])
		return(ifelse(retval != ret2, NA, retval))
	}
	if(.Generic == "==") {
		retval <- callGeneric(e1, e2@.Data[,2L]) & callGeneric(e1, e2@.Data[,1L])
		ret2 <- (e1 > e2@.Data[,1L]) & (e1 < e2@.Data[,2L]) # check in range
		retval <- ifelse(ret2, NA, retval)
		return(retval)
	} # Must be !=
	e1 <- ifelse((e2@.Data[,2L] > e1) & (e2@.Data[,1L] <= e1) & (e2@.Data[,1L] < e2@.Data[,2L]),
							 NA, e1)
	return((e1 > e2@.Data[,2L]) | (e1 < e2@.Data[,1L]) | 
				 	((e1 == e2@.Data[,2L]) & (e2@.Data[,1L] < e2@.Data[,2L]))) 
}
)
