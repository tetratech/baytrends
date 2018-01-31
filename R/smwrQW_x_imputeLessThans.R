#' @title Impute Censored Values
#' 
#' @description Imputes substitute values for left-censored values using the expectation maximization method.
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
#' @param type the type of estimate, "MLE" for maximum likelihood estimates, or "robust"
#'for robust estimation methods. See \code{\link[zCompositions]{lrEM}} for details.
#' @param initial the method to use for the initial log-ratio covariance matrix, either
#'"complete.obs" that uses only the rows with no censored data to construct the matrix, or
#'"multRepl" that uses simple substitution of censored values to compute the matrix.
#'See \code{\link[zCompositions]{lrEM}} for details.
#' @seealso \code{\link[zCompositions]{lrEM}}, \code{\link{mImputeLessThans}}
#' @importFrom zCompositions lrEM
#' @return A data frame containing the original data with imputed censored values.
#' @keywords internal
#' @export
imputeLessThans <- function(..., type=c("MLE", "robust"), 
                            initial=c("complete.obs", "multRepl")) {
  ## Coding history:
  ##    2014Oct06 DLLorenz original coding
	##    2014Oct13 DLLorenz complete, working versions
  ##
  UseMethod("imputeLessThans")
}

#' @rdname imputeLessThans
#' @export
#' @method imputeLessThans default
#' @keywords internal
imputeLessThans.default <- function(..., type=c("MLE", "robust"), 
                                    initial=c("complete.obs", "multRepl")) { 
  dots <- list(...)
  dotname <- as.list(match.call())
  dotname$type<- dotname$initial <- NULL
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
  type <- match.arg(type)
  initial <- match.arg(initial)
  retval <- lrEM(X, label=0, dl=dls, rob=type=="robust", ini.cov=initial,
                 suppress.print=TRUE)
  return(retval)
}

#' @rdname imputeLessThans
#' @export
#' @method imputeLessThans data.frame
#' @keywords internal
imputeLessThans.data.frame <- function(..., group=NULL, type=c("MLE", "robust"), 
                                    initial=c("complete.obs", "multRepl")) { 
  dots <- ..1
  if(!missing(..2))
    stop("only one data frame at a time")
  ## Keep only columns of class "qw"
  keep <- sapply(dots, inherits, what="qw")
  if(all(!keep))
    stop("No columns of class qw in dataset")
  Data <- dots[!keep] # Retain the non-qw data
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
    Data <- Data[-deleted, ]
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
  type <- match.arg(type)
  initial <- match.arg(initial)
  retval <- NULL
  for(grp in unique(group)) {
  	picks <- group == grp
  	if(sum(picks) > 2) {
  		ret <- lrEM(X[picks, ], label=0, dl=dls[picks, ], 
  								rob=type=="robust", ini.cov=initial,
  								suppress.print=TRUE)
  		if(!is.null(Data))
  			ret <- cbind(Data[picks, ], ret)
  		retval <- rbind(retval, ret)
  	} else
  		warning("group ", grp, " has fewer than 3 observations, skipped")
  }
  return(retval)
}