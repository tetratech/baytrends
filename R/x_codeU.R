#' @title Code Censored Values
#' 
#' @description Recodes censored data to u-scores, which is the sum of the sign of the 
#'differences beween each value and all other values. It is equivalent to the rank but 
#'scaled so the the mean is 0.
#' 
#' @param \dots either a data frame that contains columns of class "qw" or any combination of
#'individual vectors of class "numeric," "lcens," or "qw." Missing values are
#'removed before processing.
#' @return A data frame containing the u-coded values. The column names are taken 
#'from \code{\dots}; the rownames are derived from the sequential numbers of the 
#'original data.
#' @export
codeU <- function(...) {
  ## Coding history:
  ##    2014Oct07 DLLorenz original coding
	##    2014Oct13 DLLorenz complete, working versions
  ##
	UseMethod("codeU")
}

#' @rdname codeU
#' @export
#' @method codeU default
codeU.default <- function(...) {
  dots <- list(...)
  if(is.null(names(dots))) {
  	dotname <- as.list(match.call())
  	dotname <- sapply(dotname, deparse)
  	names(dots) <- dotname[-1]
  } else
  	names(dots) <- make.names(names(dots), unique=TRUE)
  # Check that all are numeric, etc
  keep <- sapply(dots, function(col) class(col)[1L] %in% c("numeric", "qw", "lcens"))
  if(any(!keep))
    stop("not all data are numeric, qw, or lcens")
  dots <- do.call(data.frame, dots)
  # OK, start processing, invoke na.omit to remove any rows with missings and trim Data
  ret.rows <- as.character(seq(nrow(dots)))
  dots <- na.omit(dots)
  if(!is.null(ret.na <- attr(dots, "na.action")))
  	ret.rows <- ret.rows[-ret.na]
  # Now compute the u-scores using gehanScores
  for(i in names(dots)) {
  	dots[[i]] <- gehanScores(dots[[i]])
  }
  row.names(dots) <- ret.rows
  return(dots)
}

#' @rdname codeU
#' @export
#' @method codeU data.frame
codeU.data.frame <- function(...) {
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
	for(i in names(dots)) {
		Data[[i]] <- gehanScores(dots[[i]])
	}
	return(Data)
}