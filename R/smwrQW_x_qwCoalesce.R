#' Replace missing values
#'
#' Constructs a vector with as few missing values as possible from a selected
#'sequence of vectors.
#'
#' @param \dots any number of objects of class "qw." Missing values are permitted.
#' @param means logical, if \code{TRUE}, then compute the mean of all nonmissing
#'values in the data sepcified in \dots. if \code{FALSE}, then select the first 
#'nonmissing value the sequence spcified in \dots.
#' @param lt.tol an arbitrary tolerance metric to drop interval-censoring. See 
#'\code{\link{mean.qw}} for details.
#' @return For \code{means} set to \code{FALSE}, an object of class "qw" in which each element 
#'is determined by selecting the first non-missing value in the order in which they are
#'specified in the argument list. The first step is to construct a matrix from
#'all arguments. The output is initially set to column 1; for any missing values
#'in the column, the data from column 2 are used and so on until all columns have
#'been searched or all missing values replaced. The metadata are set from the
#'corresponding column.
#'
#'For \code{means} set to \code{FALSE}, an object of class "qw" computed from the 
#'mean of the nonmissing values in each row of the data specified in \dots.
#'The metadata, except for reporting limit information, are set from
#'the first entry in \dots.
#' @note This function is most useful for creating a column in a dataset from
#'related columns that represent different methods. For example, a single
#'column of alkalinity may be desired when there are multiple columns of
#'alkalinity determined by various methods.
#' @keywords internal manip
#' @export
qwCoalesce <- function(..., means=FALSE, lt.tol=0.1) {
	##
 	dots <- list(...)
 	Nqw <- length(dots)
 	if(means) {
 		# Brute force means across the columns
 		retval <- dots[[1]]
 		for(i in seq(length(retval))) {
 			row <- lapply(dots, function(x) x[i])
 			e <- mean(do.call(c, row), lt.tol=lt.tol)
 			## Now insert each slot
 			retval@.Data[i,] <- e@.Data
 			retval@remark.codes[i] <- e@remark.codes
 			retval@value.codes[i] <- e@value.codes
 			retval@reporting.level[i] <- e@reporting.level
 			retval@reporting.method[i] <- e@reporting.method
 			retval@reporting.units[i] <- e@reporting.units
 			retval@analyte.method[i] <- e@analyte.method
 			retval@analyte.name[i] <- e@analyte.name
 			retval@unique.code[i] <- e@unique.code
 		}
 	} else {
 		retval <- dots[[1]]
 		## This is slow, but it does work
 		for(i in seq(2, Nqw)) {
 			Miss <- is.na(retval)
 			if(any(Miss)) {
 				retval@.Data[Miss,] <- dots[[i]]@.Data[Miss,]
 				retval@remark.codes[Miss] <- dots[[i]]@remark.codes[Miss]
 				retval@value.codes[Miss] <- dots[[i]]@value.codes[Miss]
 				retval@reporting.level[Miss] <- dots[[i]]@reporting.level[Miss]
 				retval@reporting.method[Miss] <- dots[[i]]@reporting.method[Miss]
 				retval@reporting.units[Miss] <- dots[[i]]@reporting.units[Miss]
 				retval@analyte.method[Miss] <- dots[[i]]@analyte.method[Miss]
 				retval@analyte.name[Miss] <- dots[[i]]@analyte.name[Miss]
 				retval@unique.code[Miss] <- dots[[i]]@unique.code[Miss]
 			}
 		}
 	}
	return(retval)
}
