#' @title Impute Censored Values
#' 
#' @description Imputes substitute values left-censored values multiple times using 
#'the data augmentation method.
#' 
#' @details Imputation of left-censored data requires the assumption of multivariate 
#'log-normality for a single population. If the data represent samples from multiple
#'populations, then they should be identified by the \code{group} argument. The minimum size 
#'for any group is 3.
#'
#' @param \dots either a data frame that contains columns of class "qw" or any combination of
#'individual vectors of class "numeric," "lcens," or "qw." Missing values are
#'removed before processing.
#' @param group character string, the name of the column in the data frame to indicate
#'a group for imputation. See \bold{Details}
#' @param m the number of multiple imputations desired.
#' @param initial the method to use for the initial log-ratio covariance matrix, either
#'"lrEM" that uses expectation maximization to construct the matrix,
#'"complete.obs" that uses only the rows with no censored data to construct the matrix, or
#'"multRepl" that uses simple substitution of censored values to compute the matrix.
#'See \code{\link[zCompositions]{lrDA}} for details.
#' @seealso \code{\link[zCompositions]{lrDA}}, \code{\link{imputeLessThans}}
#' @importFrom zCompositions lrDA
#' @return A list of length \code{m} containing data frames with imputed censored values. If
#'\code{group} is used, then the returned object is a tagged list with names corresponding
#'to the values in the grouping column. Each group contains the \code{m} repititions
#'of imputed values.
#' @keywords internal
#' @export
mImputeLessThans <- function(..., m=10, 
														initial=c("lrEM", "complete.obs", "multRepl")) {
	## Coding history:
	##    2014Oct06 DLLorenz original coding
	##    2014Oct13 DLLorenz complete, working versions
	##
	UseMethod("mImputeLessThans")
}

#' @rdname mImputeLessThans
#' @export
#' @method mImputeLessThans default
mImputeLessThans.default <- function(..., m=10, 
																		 initial=c("lrEM", "complete.obs", "multRepl")) {
	dots <- list(...)
  dotname <- as.list(match.call())
  dotname$m <- dotname$initial <- NULL
  dotname <- sapply(dotname, deparse)
  names(dots) <- dotname[-1]
  # Check that all are numeric, etc
  keep <- sapply(dots, function(col) class(col)[1L] %in% c("numeric", "qw", "lcens"))
  if(any(!keep))
    stop("not all data are numeric, qw, or lcens")
  dots <- do.call(data.frame, dots)
  # OK, start processing, invoke na.omit to remove any rows with missings and trim Data
  dots <- na.omit(dots)
  deleted <- attr(dots, "na.action")
  # Convert everything to class lcens
  for(i in names(dots))
    dots[[i]] <- as.lcens(dots[[i]])
  # Now create the X matrix and dl matrix
  X <- sapply(dots, function(x) x@.Data[, 1L])
  LTs <- sapply(dots, function(x) x@censor.codes)
  X <- ifelse(LTs, 0, X)
  dls <- sapply(dots, function(x) x@.Data[, 2L])
  # Verify that at every row has atleast two uncensored values and at least 1 column
  # has no censored values.
  Rows <- rowSums(!LTs)
  if(any(Rows < 2L))
    stop("at least on row has fewer than 2 uncensored values")
  Cols <- colSums(LTs)
  if(all(Cols > 0L))
    stop("no column has completely uncensored data")
  # Everything is in place, do it
  initial <- match.arg(initial)
  retval <- lrDA(X, label=0, dl=dls, ini.cov=initial,
                 m=m, store.mi=TRUE)
  return(retval)
}


#' @rdname mImputeLessThans
#' @export
#' @method mImputeLessThans data.frame
mImputeLessThans.data.frame <- function(..., group=NULL, m=10, 
																				initial=c("lrEM", "complete.obs", "multRepl")) {
	dots <- ..1
	if(!missing(..2))
		stop("only one data frame at a time")
	## Keep only columns of class "qw"
	keep <- sapply(dots, inherits, what="qw")
	if(all(!keep))
		stop("No columns of class qw in dataset")
	Data <- dots[!keep] # Retain the non-qw data for group, if nec.
	dots <- dots[keep]
	if(is.null(group)) {
		group <- rep("all", length.out=nrow(Data))
	} else
		group <- Data[[group]]
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
		# Stop if only 3 rows
		if(nrow(dots) < 3)
			stop("Too few rows")
	}
	if(length(deleted)) {
		group <- group[-deleted]
	}
	# Convert everything to class lcens
	for(i in names(dots)) {
		dots[[i]] <- as.lcens(dots[[i]])
	}
	# Now create the X matrix and dl matrix
	X <- sapply(dots, function(x) x@.Data[, 1L])
	LTs <- sapply(dots, function(x) x@censor.codes)
	X <- ifelse(LTs, 0, X)
	dls <- sapply(dots, function(x) x@.Data[, 2L])
	# Verify that at every row has atleast two uncensored values and at least 1 column
	# has no censored values.
	Rows <- rowSums(!LTs)
	if(any(Rows < 2L))
		stop("at least on row has fewer than 2 uncensored values")
	Cols <- colSums(LTs)
	if(all(Cols > 0L))
		stop("no column has completely uncensored data")
	# Everything is in place, do it
	initial <- match.arg(initial)
	retval <- list()
	grps <- unique(group)
	for(grp in grps) {
		picks <- group == grp
		if(sum(picks) > 2) {
			retval[[grp]] <- lrDA(X, label=0, dl=dls, ini.cov=initial,
													 m=m, store.mi=TRUE)
		} else
			warning("group ", grp, " has fewer than 3 observations, skipped")
	}
	if(length(grps) == 1L && grps == "all")
		retval <- retval[[1L]]
	return(retval)
}