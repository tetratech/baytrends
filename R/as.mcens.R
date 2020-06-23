#' @title Multiply-censored Data Conversion
#'
#' @description Converts data to a multiply-censored data (\code{mcens})
#'object: various methods.
#'
#' @details In keeping with the logic of \code{Surv}, \code{NA} is permitted to indicate
#'left- or right-censored data. If both are \code{NA}, then the observation is
#'treated as missing.
#'
#' @name as.mcens
#' @rdname as.mcens
#' @include a3_smwrQW_mcens-class.R a2_smwrQW_lcens-class.R a1_smwrQW_qw-class.R
#' @param lower.val The lower limit of the actual value, the special value of
#'\code{-Inf} or \code{NA} can be used to indicate left-censoring.
#'A vector of character strings is allowed when the remark code is
#'combined with the value; blank values are treated as missing values, any other value
#'is converted to a missing value with a warning. See \bold{Examples}.
#' @param upper.val The upper limit of the actual value, the special value of
#'\code{Inf} or \code{NA} can be used to indicate right-censoring.
#' @param censor.codes optional codes if \code{upper.val} is missing. Any
#'numeric value less than 0 indicates left-censored, any value greater than 0
#'indicates right-censored, and 0 indicates an observed value. The character
#'value "<" indicates left-censored, ">" indicates right-censored, any anything
#'else indicates an observed value.
#' @return An S4 object of class "mcens." These objects are the basis for the
#'analysis of censored data that are not strictly left censored.
#' @references Lorenz, D.L., in preparation
#' @seealso \code{\link[survival]{Surv}}
#' @exportMethod as.mcens
#' @keywords internal methods manip
#' @examples
#'## Create one of each type of censoring, including uncensored
#'# the last value is missing
#'as.mcens(c(-Inf, 2, 2, 5, NA), c(1, 2, 3, Inf, NA))
#'# For combined remark and values:
#'as.mcens(c("<1", "1", ">3", "1", "2"))
setGeneric("as.mcens", function(lower.val, upper.val, censor.codes)
           standardGeneric("as.mcens")
           ## Coding history:
           ##    2012Mar16 DLLorenz Original coding from lcens code
           ##    2013Jan13 DLLorenz Roxygenized
           ##    2013Jan13          This version
)

#' @rdname as.mcens
setMethod("as.mcens", signature(lower.val="mcens", upper.val="missing",
																censor.codes="missing"),
  function(lower.val, upper.val, censor.codes)
  	return(lower.val))

#' @rdname as.mcens
setMethod("as.mcens", signature(lower.val="lcens", upper.val="missing",
																censor.codes="missing"),
  function(lower.val, upper.val, censor.codes) {
  	## Remember lower.val is of class lcens, so it looks funny
  	upper.val <- lower.val@.Data[, 1L]
  	censor.codes <- as.integer(0L - lower.val@censor.codes)
  	lower.val <- ifelse(censor.codes == -1L, -Inf, upper.val)
  	mat <- cbind(lower.val=lower.val, upper.val=upper.val)
  	retval <- new("mcens", mat, censor.codes=censor.codes,
  								interval=rep(FALSE, length(lower.val)),
  								names="")
  	## This convention must be used to avoid doubling of the length of names
  	retval@names <- as.character(seq(nrow(mat)))
  	return(retval) } )

#' @rdname as.mcens
setMethod("as.mcens", signature(lower.val="numeric", upper.val="missing",
																censor.codes="missing"),
  function(lower.val, upper.val, censor.codes) {
  	mat <- cbind(lower.val=lower.val, upper.val=lower.val)
  	retval <- new("mcens", mat,
  								censor.codes=as.integer(rep(0L, length(lower.val))),
  								interval=rep(FALSE, length(lower.val)),
  								names="")
  	retval@names <- as.character(seq(nrow(mat)))
  	return(retval) } )

#' @rdname as.mcens
setMethod("as.mcens", signature(lower.val="numeric", upper.val="numeric",
																censor.codes="missing"),
  function(lower.val, upper.val, censor.codes) {
  	if(length(lower.val) != length(upper.val))
  		stop("lengths of lower.val and upper.val must match")
  	## convert NA to Proper Inf value, preserving both missing
  	NAs <- is.na(lower.val) & is.na(upper.val)
  	lower.val <- na2miss(lower.val, -Inf)
  	upper.val <- na2miss(upper.val, Inf)
  	if(any(lower.val > upper.val))
  		stop("All lower.val must be less than or equal to upper.val")
  	censor.codes <- ifelse(lower.val == -Inf, -1L,
  												 ifelse(upper.val == Inf, 1L, 0L))
  	interval <- censor.codes == 0L & upper.val > lower.val
  	lower.val[NAs] <- NA
  	upper.val[NAs] <- NA
  	censor.codes[NAs] <- NA
  	interval[NAs] <- NA
  	mat <- cbind(lower.val=lower.val, upper.val=upper.val)
  	retval <- new("mcens", mat, censor.codes=as.integer(censor.codes),
  								interval=interval,
  								names="")
  	retval@names <- as.character(seq(nrow(mat)))
  	return(retval) } )

#' @rdname as.mcens
setMethod("as.mcens", signature(lower.val="numeric", upper.val="missing",
																censor.codes="numeric"),
  function(lower.val, upper.val, censor.codes) {
  	if(length(lower.val) != length(censor.codes))
  		stop("lengths of lower.val and censor.codes must match")
  	upper.val <- lower.val
  	censor.codes <- as.integer(sign(censor.codes))
  	lower.val[censor.codes == -1L] <- -Inf
  	upper.val[censor.codes == 1L] <- Inf
  	mat <- cbind(lower.val=lower.val, upper.val=upper.val)
  	retval <- new("mcens", mat, censor.codes=censor.codes,
  								interval=rep(FALSE, length(lower.val)),
  								names="")
  	retval@names <- as.character(seq(nrow(mat)))
  	return(retval) } )

#' @rdname as.mcens
setMethod("as.mcens", signature(lower.val="numeric", upper.val="missing",
																censor.codes="integer"),
  function(lower.val, upper.val, censor.codes) {
  	if(length(lower.val) != length(censor.codes))
  		stop("lengths of lower.val and censor.codes must match")
  	upper.val <- lower.val
  	censor.codes <- as.integer(sign(censor.codes))
  	lower.val[censor.codes == -1L] <- -Inf
  	upper.val[censor.codes == 1L] <- Inf
  	mat <- cbind(lower.val=lower.val, upper.val=upper.val)
  	retval <- new("mcens", mat, censor.codes=censor.codes,
  								interval=rep(FALSE, length(lower.val)),
  								names="")
  	retval@names <- as.character(seq(nrow(mat)))
  	return(retval) } )

#' @rdname as.mcens
setMethod("as.mcens", signature(lower.val="numeric", upper.val="missing",
																censor.codes="character"),
  function(lower.val, upper.val, censor.codes) {
  	if(length(lower.val) != length(censor.codes))
  		stop("lengths of lower.val and censor.codes must match")
  	cc <- unique(censor.codes)
  	bad <- which(!(cc %in% c("<", ">", "", " ")))
  	if(any(bad))
  		warning(paste("censor.codes ignored:", cc[bad], collapse=" "))
  	## Done checking, get on with it
  	upper.val <- lower.val
  	censor.codes <- ifelse(censor.codes == "<", -1L,
  												 ifelse(censor.codes == ">", 1L, 0L))
  	lower.val[censor.codes == -1L] <- -Inf
  	upper.val[censor.codes == 1L] <- Inf
  	mat <- cbind(lower.val=lower.val, upper.val=upper.val)
  	retval <- new("mcens", mat, censor.codes=censor.codes,
  								interval=rep(FALSE, length(lower.val)),
  								names="")
  	retval@names <- as.character(seq(nrow(mat)))
  	return(retval) } )

#' @rdname as.mcens
setMethod("as.mcens", signature(lower.val="qw", upper.val="missing",
																censor.codes="missing"),
  function(lower.val, upper.val, censor.codes) {
  	## Make copy so that lower.val, etc have ususal meanings
  	temp <- lower.val 
  	lower.val <- temp@.Data[, 1L]
  	upper.val <- temp@.Data[, 2L]
  	Cc <- temp@remark.codes
  	Dl <- temp@reporting.level
  	## Fix less-thans
  	Pick <- na2miss(Dl > upper.val, FALSE)
  	Cc[Pick] <- "<"
  	upper.val[Pick] <- Dl[Pick]
  	lower.val[Cc == "<"] <- -Inf
  	## Fix greater-thans
  	upper.val[Cc == ">"] <- Inf
  	## Compute the censor.codes and interval
  	censor.codes <- match(Cc, c("<", " ", ">"), nomatch=2L) - 2L
  	interval <- Cc == "I"
  	mat <- cbind(lower.val=lower.val, upper.val=upper.val)
  	retval <- new("mcens", mat, censor.codes=as.integer(censor.codes),
  								interval=interval,
  								names="")
  	retval@names <- as.character(seq(nrow(mat)))
  	return(retval) } )

#' @rdname as.mcens
setMethod("as.mcens", signature(lower.val="character", upper.val="missing",
																censor.codes="missing"),
  function(lower.val, upper.val, censor.codes) {
  	# Split data into remarks and values and call the correct converter
  	values <- splitQual(lower.val, "X")
  	return(as.mcens(values$X, censor.codes=values$X.rmk))
  }
)
