#' @title Left-censored Data Conversion
#'
#' @description Converts data to a left-censored data (\code{lcens}) object: various methods.
#'
#' @name as.lcens
#' @rdname as.lcens
#' @include a2_smwrQW_lcens-class.R a1_smwrQW_qw-class.R
#' @param values numeric values representing "raw" values. Missing values
#'are permitted. A vector of character strings is allowed when the remark code is
#'combined with the value; blank values are treated as missing values, any other value
#'is converted to a missing value with a warning. See \bold{Examples}.
#' @param detlim the corresponding detection limit of the sensor. Missing values
#'are permitted. Detection limits are required for each non-missing value in \code{values};
#'they will be imputed if missing. The imputation scheme is fairly sophisticated, see
#'Lorenz (in preparation) for details, so it is better to set to missing, rather than
#'provide an arbitrary detection limit.
#' @param censor.codes optional codes indicating a left-censored value. If class
#'"logical," then \code{TRUE} indicates left-censored. If class "character,"
#'then "<" indicates left-censored and anything other than "" or " " generates
#'a warning.
#' @return An S4 object of class "lcens." These objects are the basis for the
#'analysis of left-censored data.
#' @note All methods force \code{values} to be no less than the corresponding
#'\code{detlim}. All \code{values} that are less than \code{detlim} are treated
#'as left-censored values at \code{detlim}. This ensures unbiased analytical results.
#' @references Lorenz, D.L., in preparation
#' @seealso \code{\link{as.mcens}}
#' @exportMethod as.lcens
#' @keywords internal methods manip
#' @examples
#'
#'## The first value is censored at 2
#'as.lcens(c(1,3), 2)
#'## Set the first value to censored at that level and the detection limit is
#'# carried forward
#'as.lcens(c(1,3), censor.codes=c("<", ""))
#'# For combined remark and values:
#'as.lcens(c("<1", "1", "<1", "1", "2"))
#'
setGeneric("as.lcens", function(values, detlim, censor.codes) standardGeneric("as.lcens")
           ## Coding history:
           ##    2012Mar01 DLLorenz Conversion to R/new style
           ##    2012Mar22 DLLorenz Conversion to S4
           ##    2012Jul24 DLLorenz Modified dfns to produce typeof = "numeric"
           ##    2012Aug03 DLLorenz Added detlim="missing" signatures to as.lcens.
           ##    2012Sep17 DLLorenz Added values="qw" signature to as.lcens
           ##    2013Jan13 DLLorenz Roxygenized
           ##    2013Jan13          This version
)

#' @rdname as.lcens
setMethod("as.lcens", signature(values="lcens", detlim="missing",
																censor.codes="missing"),
					function(values, detlim, censor.codes)
						return(values))

#' @rdname as.lcens
setMethod("as.lcens", signature(values="numeric", detlim="missing",
																censor.codes="missing"),
  function(values, detlim, censor.codes) {
  	## detlim is missing, so set as less than the minimum values (no censored values)
  	if(all(is.na(values))) # Need to protext against a single NA or NA vector
  		detlim <- 0
  	else
  		detlim <- min(values, na.rm=TRUE)
  	detlim <- floor(detlim - 100)
  	detlim <- rep(detlim, length(values))
  	mat <- cbind(values=values, detlim=detlim)
  	retval <- new("lcens", mat, censor.codes=values < detlim,
  								names="")
  	## This convention must be used to avoid doubling of the length of names
  	retval@names <- as.character(seq(nrow(mat)))
  	return(retval) })

#' @rdname as.lcens
setMethod("as.lcens", signature(values="numeric", detlim="numeric", 
																censor.codes="missing"),
  function(values, detlim, censor.codes) {
  	detlim <- rep(detlim, length.out=length(values))
  	## easiest one to make from scratch
  	mat <- cbind(values=pmax(values, detlim), detlim=detlim)
  	retval <- new("lcens", mat, censor.codes=values < detlim,
  								names="")
  	retval@names <- as.character(seq(nrow(mat)))
  	return(retval) })

#' @rdname as.lcens
setMethod("as.lcens", signature(values="numeric", detlim="numeric",
																censor.codes="logical"),
  function(values, detlim, censor.codes) {
  	## There are 4 possible conditions between values and detlim:
  	##  value detlim result         comment
  	##    V     DL     max(V, DL)   censored if DL > V, ties resolved by censor.codes
  	##   NA     NA     NA
  	##   NA     DL     DL           left-censored at DL
  	##    V     NA     V            detlim left as NA and warning issued
  	## There is 1 additional possibility to protect against:
  	##  a value reported as <V where V is in elevated RL, but detlim is the
  	##  method reporting limit
  	detlim <- rep(detlim, length.out=length(values)) ## Rare, but possible
  	censored <- values < detlim
  	vals <- pmax(values, detlim)
  	## Check for 3rd possibility (modern databases often store censored values
  	##  as NA and rely on some kind of censoring indicator and the detection/
  	##  reporting limit)
  	if(any(sel <- (is.na(values) & !is.na(detlim) & !is.na(censor.codes)))) {
  		censored[sel] <- TRUE
  		vals[sel] <- detlim[sel]
  	}
  	## check for the 4th possibility No detection/reporting limit
  	if(any(sel <- (!is.na(values) & is.na(detlim)))) {
  		censored[sel] <- FALSE
  		vals[sel] <- values[sel]
  		warning("values with missing detlim--retained as uncensored value")
  	}
  	## This fixes ties and additional issues
  	good <- which(censor.codes) # Protect against NAs
  	censored[good] <- TRUE
  	detlim[good] <- pmax(vals, detlim)[good]
  	mat <- cbind(values=vals, detlim=detlim)
  	retval <- new("lcens", mat, censor.codes=censored,
  								names="")
  	retval@names <- as.character(seq(nrow(mat)))
  	return(retval) })

#' @rdname as.lcens
setMethod("as.lcens", signature(values="numeric", detlim="numeric",
																censor.codes="character"),
  function(values, detlim, censor.codes) {
  	## There are 4 possible conditions between values and detlim:
  	##  see method of censor.codes = "logical"
  	## Check for < and convert < to T
  	cc <- unique(censor.codes)
  	bad <- which(!(cc %in% c("<", "", " ")))
  	if(any(bad))
  		warning(paste("censor.codes ignored:", cc[bad], collapse=" "))
  	censor.codes <- censor.codes == "<"
  	detlim <- rep(detlim, length.out=length(values))
  	censored <- values < detlim
  	vals <- pmax(values, detlim)
  	## Check for 3rd possibility
  	if(any(sel <- (is.na(values) & !is.na(detlim) & !is.na(censor.codes)))) {
  		censored[sel] <- TRUE
  		vals[sel] <- detlim[sel]
  	}
  	## Check for the 4th possibility No detection/reporting limit
  	if(any(sel <- (!is.na(values) & is.na(detlim)))) {
  		censored[sel] <- FALSE
  		vals[sel] <- values[sel]
  		warning("values with missing detlim--retained as uncensored value")
  	}
  	## This fixes ties and additional issues
  	good <- which(censor.codes) # Protect against NAs
  	censored[good] <- TRUE
  	detlim[good] <- pmax(vals, detlim)[good]
  	mat <- cbind(values=vals, detlim=detlim)
  	retval <- new("lcens", mat, censor.codes=censored,
  								names="")
  	retval@names <- as.character(seq(nrow(mat)))
  	return(retval) } )

#' @rdname as.lcens
setMethod("as.lcens", signature(values="numeric", detlim="missing",
																censor.codes="logical"),
  function(values, detlim, censor.codes) {
  	detlim <- dlimit(values, censor.codes)
  	mat <- cbind(values=values, detlim=detlim)
  	retval <- new("lcens", mat, censor.codes=censor.codes,
  								names="")
  	retval@names <- as.character(seq(nrow(mat)))
  	return(retval) } )

#' @rdname as.lcens
setMethod("as.lcens", signature(values="numeric", detlim="missing",
																censor.codes="character"),
  function(values, detlim, censor.codes) {
  	##
  	detlim <- dlimit(values, censor.codes)
  	mat <- cbind(values=values, detlim=detlim)
  	retval <- new("lcens", mat, censor.codes=censor.codes=="<",
  								names="")
  	retval@names <- as.character(seq(nrow(mat)))
  	return(retval) } )

#' @rdname as.lcens
setMethod("as.lcens", signature(values="qw", detlim="missing",
																censor.codes="missing"),
  function(values, detlim, censor.codes) {
  	## Convert qw object to lcens if possible
  	if(censoring(values) == "multiple")
  		stop("The qw object does not contain only left-censored data")
  	Val <- values@.Data[, 2L]
  	Dl <- values@reporting.level
  	Cen <- values@remark.codes == "<"
  	## Prevent all FALSE and some NA (any(Cen) returns NA in that case)
  	Cen <- na2miss(Cen, FALSE)
  	## Determine if any Dls need to be estimated
  	picks <- !is.na(Val) & is.na(Dl)
  	if(any(picks)) {
  		if(any(Cen))
  			def <-  min(Val[picks])
  		else
  			def <- 1.e-25
  		Guess <- dlimit(Val[picks], Cen[picks], def)
  		Dl[picks] <- Guess
  	}
  	## Censor Val < Dl
  	Cen <- Cen | (Val < Dl)
  	Val <- pmax(Val, Dl)
  	## Check for elevated less-thans
  	picks <- Cen & Val > Dl
  	# fix picks like Cen
  	picks <- na2miss(picks, FALSE)
  	if(any(picks))
  		Dl[picks] <- Val[picks]
  	## Build object
  	mat <- cbind(values=Val, detlim=Dl)
  	retval <- new("lcens", mat, censor.codes=Cen,
  								names="")
  	retval@names <- as.character(seq(nrow(mat)))
  	return(retval) } )

#' @rdname as.lcens
setMethod("as.lcens", signature(values="qw", detlim="numeric",
																censor.codes="missing"),
  function(values, detlim, censor.codes) {
  	## Convert qw object to lcens if possible
  	if(censoring(values) == "multiple")
  		stop("The qw object does not contain only left-censored data")
  	Val <- values@.Data[, 2L]
  	## Force censoring at minimum DL
  	Dl <- pmax(values@reporting.level, detlim)
  	Cen <- values@remark.codes == "<" | na2miss(Val, detlim + 2) < detlim
  	## Prevent all FALSE and some NA (any(Cen) returns NA in that case)
  	Cen <- na2miss(Cen, FALSE)
  	Val <- pmax(Val, detlim)
  	## Determine if any Dls need to be estimated
  	picks <- !is.na(Val) & is.na(Dl)
  	if(any(picks)) {
  		if(any(Cen))
  			def <-  min(Val[picks])
  		else
  			def <- 1.e-25
  		Guess <- dlimit(Val[picks], Cen[picks], def)
  		Dl[picks] <- Guess
  	}
  	## Censor Val < Dl
  	Cen <- Cen | (Val < Dl)
  	Val <- pmax(Val, Dl)
  	## Check for elevated less-thans
  	picks <- Cen & Val > Dl
  	# fix picks like Cen
  	picks <- na2miss(picks, FALSE)
  	if(any(picks))
  		Dl[picks] <- Val[picks]
  	## Build object
  	mat <- cbind(values=Val, detlim=Dl)
  	retval <- new("lcens", mat, censor.codes=Cen,
  								names="")
  	retval@names <- as.character(seq(nrow(mat)))
  	return(retval) } )

#' @rdname as.lcens
setMethod("as.lcens", signature(values="character", detlim="missing",
																censor.codes="missing"),
  function(values, detlim, censor.codes) {
  	# Split data into remarks and values and call the correct converter
  	values <- splitQual(values, "X")
  	return(as.lcens(values$X, censor.codes=values$X.rmk))
  }
)