#' @title Mathematical Transform Methods for \code{lcens} objects
#'
#' @description Some limited mathematical methods are possible and well-defined for
#'censored or water-quality data.
#'
#'
#' @name Math-censored
#' @include a3_smwrQW_mcens-class.R a2_smwrQW_lcens-class.R a1_smwrQW_qw-class.R
#' @param x the object to transform
#' @keywords internal methods manip
#' @exportMethod  Math
#' @examples
#'log(as.lcens(c(1, 3), 2))
#' 

#' @rdname Math-censored
#' @aliases Math,lcens-method
setMethod("Math", "lcens", function(x) {
  if(length(x) == 0L)
    return(x) # Do nothing
  switch(.Generic,
  			 log={lt0 <- which(x@.Data[, 2L] <= 0) # protect against neg DL
  			 		 tmp <- log(pmax(x@.Data, 0))
  			 		 tmp[lt0, 2L] <- tmp[lt0, 1L]- 100
  			 		 x@.Data <- tmp
  			 		 x},
  			 log10={lt0 <- which(x@.Data[, 2L] <= 0)
  			 			 tmp <- log10(pmax(x@.Data, 0))
  			 			 tmp[lt0, 2L] <- tmp[lt0, 1L]- 100
  			 			 x@.Data <- tmp
  			 			 x},
  			 sqrt={lt0 <- which(x@.Data[, 2L] < 0)
  			 			tmp <- sqrt(pmax(x@.Data, 0))
  			 			tmp[lt0, 2L] <- tmp[lt0, 1L]- 100
  			 			x@.Data <- tmp
  			 			x},
  			 exp={x@.Data <- exp(x@.Data)
              x},
         stop(gettextf("'%s' not defined for lcens objects", .Generic),
              domain=NA))
  }
)

#' @rdname Math-censored
#' @aliases Math,mcens-method
setMethod("Math", "mcens", function(x) {
	if(length(x) == 0L)
		return(x) # Do nothing
	x <- switch(.Generic,
							log={tmp <- log(pmax(x@.Data, 0))
									 x@.Data <- tmp
									 x},
							log10={tmp <- log10(pmax(x@.Data, 0))
										 x@.Data <- tmp
										 x},
							sqrt={tmp <-sqrt(x@.Data)
										x@.Data <- tmp
										x},
							exp={x@.Data <- exp(x@.Data)
									 x},
							stop(gettextf("'%s' not defined for mcens objects", .Generic),
									 domain=NA))
	natest <- x@.Data[, 1L] == -Inf & x@.Data[, 2L] == -Inf # Both less that 0
	x@.Data[natest, ] <- c(NA_real_, NA_real_)
	return(x)
}
)

#' @rdname Math-censored
#' @aliases Math,qw-method
setMethod("Math", "qw", function(x) {
	if(length(x) == 0L)
		return(x) # Do nothing
	cen <- censoring(x)
	if(cen == "none") {
		x=as.numeric(x)
	} else if(cen == "left") {
		x=as.lcens(x)
	} else { # remaining option is mcens
		x=as.mcens(x)
	}
	get(.Generic)(x)
}
)
