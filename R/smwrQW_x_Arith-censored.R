#' @title Arithmetic Methods for \code{lcens}, \code{mcens}, and \code{qw} objects
#'
#' @description Some limited arithmetic methods are possible and well-defined for
#'censored data. Water-quality data require specialized functions to maintain data
#'integrity.
#'
#' @include a3_smwrQW_mcens-class.R a2_smwrQW_lcens-class.R a1_smwrQW_qw-class.R
#' @name Arith-censored
#' @param e1,e2 numeric, censored, or water-quality data. Missing values are permitted in 
#'either argument and result in a missing value in the output.
#' @return An object of the appropriate class for the data.
#' @import methods
#' @seealso \code{\link{add}}, \code{\link{ratio}}
#' @keywords internal methods manip
#' @exportMethod Arith
#' @examples
#'as.lcens(c(1, 3), 2) + 1
#'as.lcens(c(1, 3), 2) * 2
#'

#' @rdname Arith-censored
#' @aliases Arith,lcens,numeric-method
setMethod("Arith", signature(e1="lcens", e2="numeric"), function(e1, e2) {
  if(.Generic %in% c("%%", "%/%", "^"))
    stop(gettextf("'%s' not defined for 'lcens' objects", .Generic),
              domain=NA)
  if(.Generic %in% c("*", "/") && any(e2 <= 0))
    stop(gettextf("'%s' only valid for positive values", .Generic),
              domain=NA)
  e1@.Data[, 1L] <- callGeneric(e1@.Data[, 1L], e2)
  e1@.Data[, 2L] <- callGeneric(e1@.Data[, 2L], e2)
  e1}
)

#' @rdname Arith-censored
#' @aliases Arith,numeric,lcens-method
setMethod("Arith", signature(e1="numeric", e2="lcens"), function(e1, e2) {
  if(.Generic %in% c("%%", "%/%", "^", "-", "/"))
    stop(gettextf("'%s' not defined for 'lcens' objects", .Generic),
              domain=NA)
  if(.Generic %in% c("*") && any(e1 <= 0))
    stop(gettextf("'%s' only valid for positive values", .Generic),
              domain=NA)
  e2@.Data[, 1L] <- callGeneric(e1, e2@.Data[, 1L])
  e2@.Data[, 2L] <- callGeneric(e1, e2@.Data[, 2L])
  e2}
)

#' @rdname Arith-censored
#' @aliases Arith,lcens,lcens-method
setMethod("Arith", signature(e1="lcens", e2="lcens"), function(e1, e2) {
  if(.Generic == "+") {
    e1@censor.codes <- e1@censor.codes | e2@censor.codes
    e1@.Data[, 1L] <- e1@.Data[, 1L] + e2@.Data[, 1L]
    e1@.Data[, 2L] <- pmax(e1@.Data[, 2L] + e2@.Data[, 2L],
              e1@.Data[, 2L], e2@.Data[, 2L], ifelse(e1@censor.codes,
                                                   e1@.Data[, 1L], -101))
    return(e1)
  }
  else
    stop(gettextf("'%s' not defined for 'lcens' objects", .Generic),
         domain=NA)
})

#' @rdname Arith-censored
#' @aliases Arith,mcens,numeric-method
setMethod("Arith", signature(e1="mcens", e2="numeric"), function(e1, e2) {
	if(.Generic %in% c("%%", "%/%", "^"))
		stop(gettextf("'%s' not defined for 'mcens' objects", .Generic),
				 domain=NA)
	if(.Generic %in% c("*", "/")) {
		## Negatives reverse the sign for both * and /
		e2 <- rep(e2, length.out=length(e1))
		tmp2 <- callGeneric(e1@.Data[, 2L], e2)
		tmp1 <- callGeneric(e1@.Data[, 1L], e2)
		flip <- e2 < 0
		e1@.Data[, 1L] <- ifelse(flip, tmp2, tmp1)
		e1@.Data[, 2L] <- ifelse(flip, tmp1, tmp2)
		e1@censor.codes[flip] <- e1@censor.codes[flip] * -1L
	} else {
		e1@.Data[, 1L] <- callGeneric(e1@.Data[, 1L], e2)
		e1@.Data[, 2L] <- callGeneric(e1@.Data[, 2L], e2)
	}
	e1}
)

#' @rdname Arith-censored
#' @aliases Arith,numeric,mcens-method
setMethod("Arith", signature(e1="numeric", e2="mcens"), function(e1, e2) {
	if(.Generic %in% c("%%", "%/%", "^"))
		stop(gettextf("'%s' not defined for 'mcens' objects", .Generic),
				 domain=NA)
	if(.Generic %in% c("*", "/")) {
		## Negatives reverse the sign for *, vice versa for /
		e1 <- rep(e1, length.out=length(e2))
		tmp2 <- callGeneric(e1, e2@.Data[, 2L])
		tmp1 <- callGeneric(e1, e2@.Data[, 1L])
		## Special considerations required for /, not so much for *
		if(.Generic == "*") {
			flip <- e1 < 0
			e2@.Data[, 1L] <- ifelse(flip, tmp2, tmp1)
			e2@.Data[, 2L] <- ifelse(flip, tmp1, tmp2)
			e2@censor.codes[flip] <- e2@censor.codes[flip] * -1L
		} else {
			flip <- e1 > 0
			## Any crossing 0 causes interval on -Inf, Inf
			ckx0 <- sign(e2@.Data[, 1L] * e2@.Data[, 2L])
			ckx0[is.nan(ckx0)] <- 0
			## If value is 0, then interval on -Inf, Inf
			cke0 <- which(e2@.Data[, 1L] == 0 & e2@.Data[, 2L] == 0)
			e2@.Data[, 1L] <- ifelse(flip, tmp2, tmp1)
			e2@.Data[, 2L] <- ifelse(flip, tmp1, tmp2)
			## Fix from 0 issues
			e2@.Data[ckx0 < 0L, 1L] <- -Inf
			e2@.Data[ckx0 < 0L, 2L] <- Inf
			e2@.Data[cke0, 1L] <- -Inf
			e2@.Data[cke0, 2L] <- Inf
			## Now set censor codes and interval
			e2@censor.codes <- e2@censor.codes * 0L
			lcens <- e2@.Data[, 1L] == -Inf & e2@.Data[, 2L] < Inf
			e2@censor.codes[lcens] <- -1L
			rcens <- e2@.Data[, 1L] > -Inf & e2@.Data[, 2L] == Inf
			e2@censor.codes[rcens] <- 1L
			icens <- e2@censor.codes == 0L & e2@.Data[, 1L] < e2@.Data[, 2L]
			e2@interval <- icens	
		}
	} else {
		e2@.Data[, 1L] <- callGeneric(e1, e2@.Data[, 1L])
		e2@.Data[, 2L] <- callGeneric(e1, e2@.Data[, 2L])
	}
	e2}
)

#' @rdname Arith-censored
#' @aliases Arith,mcens,mcens-method
setMethod("Arith", signature(e1="mcens", e2="mcens"), function(e1, e2) {
	if(length(e1) > 1L && length(e2) > 1L && length(e1) != length(e2))
		stop("length of arguments must be 1 or equal")
	if(.Generic == "+") {
		e1@interval <- e1@interval | e2@interval
		e1@censor.codes <- as.integer(sign(e1@censor.codes + e2@censor.codes))
		e1@.Data[, 1L] <- e1@.Data[, 1L] + e2@.Data[, 1L]
		e1@.Data[, 2L] <- e1@.Data[, 2L] + e2@.Data[, 2L]
		# Check for any > or < values and set meta info
		cks <- is.infinite(e1@.Data[, 1L]) | is.infinite(e1@.Data[, 2L])
		e1@interval[cks] <- FALSE
		# Check for < + > and reset meta info (recovers from above)
		infs <- is.infinite(e1@.Data[, 1L]) & is.infinite(e1@.Data[, 2L])
		if(any(infs)) {
			e1@censor.codes[infs] <- 0L
			e1@interval[infs] <- TRUE
		}
	} else if(.Generic == "-") {
		e1@interval <- e1@interval | e2@interval
		e1@censor.codes <- as.integer(sign(e1@censor.codes - e2@censor.codes))
		e1@.Data[, 1L] <- e1@.Data[, 1L] - e2@.Data[, 2L]
		e1@.Data[, 2L] <- e1@.Data[, 2L] - e2@.Data[, 1L]
		# Check for any > or < values and set meta info
		cks <- is.infinite(e1@.Data[, 1L]) | is.infinite(e1@.Data[, 2L])
		e1@interval[cks] <- FALSE
		# Check for < + > and reset meta info (recovers from above)
		infs <- is.infinite(e1@.Data[, 1L]) & is.infinite(e1@.Data[, 2L])
		if(any(infs)) {
			e1@censor.codes[infs] <- 0L
			e1@interval[infs] <- TRUE
		}
	} else if(.Generic == "*") {
		e1@interval <- e1@interval | e2@interval
		e1@censor.codes <- as.integer(sign(e1@censor.codes + e2@censor.codes))
		## logic required to account for positive and negative ranges
		tmp1 <- e1@.Data[, 1L] * e2@.Data[, 1L]
		tmp2 <- e1@.Data[, 2L] * e2@.Data[, 2L]
		tmp3 <- e1@.Data[, 1L] * e2@.Data[, 2L]
		tmp4 <- e1@.Data[, 2L] * e2@.Data[, 1L]
		## NaNs are generated from -Inf * Inf, set result to -Inf (the limit)
		## Only possible in 3 & 4
		tmp3[is.nan(tmp3)] <- -Inf
		tmp4[is.nan(tmp4)] <- -Inf
		## NaNs are also generated from 0 * (+/-) Inf, set to 0
		tmp1[e1@.Data[, 1L] == 0 & e2@.Data[, 1L] == -Inf] <- 0
		tmp2[e1@.Data[, 2L] == 0 & e2@.Data[, 2L] == Inf] <- 0
		tmp3[e1@.Data[, 1L] == 0 & e2@.Data[, 2L] == Inf] <- 0
		tmp4[e1@.Data[, 2L] == 0 & e2@.Data[, 1L] == -Inf] <- 0
		## Also need to check for any value = 0
		ck0 <- (e1@.Data[, 1L] == 0 & e1@.Data[, 2L] == 0) |
			(e2@.Data[, 1L] == 0 & e2@.Data[, 2L] == 0)
		## Update return values and set to 0
		e1@.Data[, 1L] <- pmin(tmp1, tmp2, tmp3, tmp4)
		e1@.Data[, 2L] <- pmax(tmp1, tmp2, tmp3, tmp4)
		e1@.Data[ck0, 1L] <- 0
		e1@.Data[ck0, 2L] <- 0
		# Check for any > or < values and set meta info
		cks <- is.infinite(e1@.Data[, 1L]) | is.infinite(e1@.Data[, 2L])
		e1@interval[cks] <- FALSE
		# Check for < + > and reset meta info (recovers from above)
		infs <- is.infinite(e1@.Data[, 1L]) & is.infinite(e1@.Data[, 2L])
		if(any(infs)) {
			e1@censor.codes[infs] <- 0L
			e1@interval[infs] <- TRUE
		}
	} else if(.Generic == "/") {
		## Take the easy way out
		e1 <- e1 * (1 / e2)
	} else
		stop(gettextf("'%s' not defined for 'mcens' objects", .Generic),
				 domain=NA)
	return(e1)
})

#' @rdname Arith-censored
#' @aliases Arith,qw,numeric-method
setMethod("Arith", signature(e1="qw", e2="numeric"), function(e1, e2) {
		stop(gettextf("'%s' not defined for 'qw' objects", .Generic),
				 domain=NA)
})

#' @rdname Arith-censored
#' @aliases Arith,numeric,qw-method
setMethod("Arith", signature(e1="numeric", e2="qw"), function(e1, e2) {
	stop(gettextf("'%s' not defined for 'qw' objects", .Generic),
			 domain=NA)
})

#' @rdname Arith-censored
#' @aliases Arith,qw,qw-method
setMethod("Arith", signature(e1="qw", e2="qw"), function(e1, e2) {
	stop(gettextf("'%s' not defined for 'qw' objects", .Generic),
			 domain=NA)
})