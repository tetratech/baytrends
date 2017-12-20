#' @title Code Censored Values
#' 
#' @description Recodes censored data to a binary coding system with 0 indicating
#'less than some critical value and 1 indicating greater than or equal to that critical 
#'value.
#' 
#' @param \dots either a data frame that contains columns of class "qw" or any combination of
#'individual vectors of class "integer," "numeric," "lcens," or "qw." Missing values are
#'removed before processing.
#' @param criteria a vector indicating the critical values for each repsective value in
#'\code{\dots}. May be a named vector containing critical values for the named argument
#'in \code{\dots} or not supplied. For any argument in \code{\dots} that does not have
#'a critical value, the critical value is the largest censoring level or if the data have
#'no censored values, the median. The critical value can never be smaller than
#'the largest censoring level and will be changed with a warning message. A special case
#'is for integer values that have a minimum of 0---for those data the default criterion is to
#'return 0 if the value is 0 and return 1 for any other value.
#' @return A data frame containing the binary coded values. Each column has an attribute
#'called "critical.value" that reports the critical value used to recode the values in 
#'that column. The column names are taken from \code{\dots}; the rownames are derived 
#'from the sequential numbers of the original data.
#' @export
code01 <- function(..., criteria) {
  ## Coding history:
  ##    2014Oct06 DLLorenz original coding
	##    2014Oct13 DLLorenz complete, working versions
	##
	UseMethod("code01")
}

#' @rdname code01
#' @export
#' @method code01 default
code01.default <- function(..., criteria) {
  dots <- list(...)
  if(is.null(names(dots))) {
  	dotname <- as.list(match.call())
  	dotname$criteria <- NULL
  	dotname <- sapply(dotname, deparse)
  	names(dots) <- dotname[-1]
  } else
  	names(dots) <- make.names(names(dots), unique=TRUE)
  # Check that all are numeric, etc
  keep <- sapply(dots, function(col) class(col)[1L] %in% 
  							 	c("integer", "numeric", "qw", "lcens"))
  if(any(!keep))
    stop("not all data are integer, numeric, qw, or lcens")
  ## Capture special case
  special <- sapply(dots, function(col) class(col)[1L] == "integer")
  if(any(special)) {
  	# check in min is 0
  	special[special] <- sapply(dots[special], function(col) min(col) == 0L)
  }
  dots <- do.call(data.frame, dots)
  special <- names(dots)[special]
  # OK, start processing, invoke na.omit to remove any rows with missings and trim Data
  ret.rows <- as.character(seq(nrow(dots)))
  dots <- na.omit(dots)
  if(!is.null(ret.na <- attr(dots, "na.action")))
  	ret.rows <- ret.rows[-ret.na]
  # Convert everything to class lcens
  for(i in names(dots))
    dots[[i]] <- as.lcens(dots[[i]])
  # Compute the largest censored value and compare to criteria
  MaxCen <- sapply(dots, function(x) max(censorLevels(x)))
  if(missing(criteria)) {
  	criteria <- MaxCen
  } else if(is.null(names(criteria))) {
  	if(length(criteria) != length(dots))
  		stop("length of criteria must match the other arguments if not named")
  	names(criteria) <- names(MaxCen)
  } else {
  	criteria <- criteria[names(MaxCen)]
  	names(criteria) <- names(MaxCen) # need to copy unused names
  }
  # Now verify validity of criteria and populate retval
  retval <- list()
  for(i in names(criteria)) {
  	## If uncensored
  	if(MaxCen[i] == -Inf) {
  		if(!is.finite(criteria[i])) # nothing specified, use median unless special
  			if(i %in% special) {
  				criteria[i] <- 0.5
  			} else {
  			criteria[i] <- median(dots[[i]])
  			}
  	} else { # censored
  		if(is.na(criteria[i])) { # nothing specified, use max cen
  			criteria[i] <- MaxCen[i]
  		} else if(criteria[i] < MaxCen[i]) { # Too small a value specified
  			criteria[i] <- MaxCen[i]
  			warning("criterion for ", i, " too small, using ", MaxCen[i])
  		}
  		## Otherwise use criteria value
  	}
  	## OK compute 0/1
  	zo <- na2miss(as.integer(dots[[i]] >= criteria[i]), 0L)
  	attr(zo, "criterion") <- criteria[[i]]
  	retval[[i]] <- zo
  }
  retval <- as.data.frame(retval)
  row.names(retval) <- ret.rows
  return(retval)
}

#' @rdname code01
#' @export
#' @method code01 data.frame
code01.data.frame <- function(..., criteria) {
	dots <- ..1
	if(!missing(..2))
		stop("only one data frame at a time")
	## Keep only columns of class "qw"
	keep <- sapply(dots, inherits, what="qw")
	if(all(!keep))
		stop("No columns of class qw in dataset")
	Data <- dots[!keep] # Retain the non-qw data for group, if nec.
	dots <- dots[keep]
	## OK, start processing, invoke na.omit to remove any rows with missings and trim Data
	## Check for excessive missings
	misses <- sapply(dots, function(x) sum(is.na(x)))
	dots <- na.omit(dots)
	deleted <- attr(dots, "na.action")
	if(length(deleted) > nrow(dots)) {
		misses <- names(misses)[misses > length(deleted)/2]
		misses <- paste(misses, collapse=", ")
		warning("Many rows deleted from data set, check columns ",
						misses,	" for excessive missing values")
	}
	if(length(deleted)) {
		Data <- Data[-deleted,]
	}
	# Set criteria if needed, will fail if length mismatch
	if(!missing(criteria) && is.null(names(criteria)))
		names(criteria) <- names(dots)
	# Convert everything to class lcens and call default method
	for(i in names(dots)) {
		zo <- if(!missing(criteria)) {
			code01.default(X=as.lcens(dots[[i]]), criteria[i])
		} else
			code01.default(X=as.lcens(dots[[i]]))
		Data[[i]] <- zo[[1L]]
	}
	return(Data)
}