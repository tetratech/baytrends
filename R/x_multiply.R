#' @title Multiply Water-Quality Data by a Number
#'
#' @description Multiplies water-quality data by a number while maintaining the 
#'integrity of the data.
#'
#' @details Multiplication is not defined for objects of class "qw" because it
#'can change some of the metadata, in particular the units, the analytical method,
#'or the unique identifier (pcode).
#'
#' @include a1_smwrQW_qw-class.R
#' @param x an object of water-quality (class "qw"). Missing value are permitted and result
#'in corresponding missing values in the output.
#' @param factor a single numeric value
#' @param units character reporting units of the resulting operation, if not supplied, 
#'retained from \code{x}.
#' @param analyte character the name of the resulting analyte.
#' @param analyte.method character the name of the analytical method for the result,
#'by default "ALGOR."
#' @param pcode the parameter code of the result, if not supplied, retained from \code{x}.
#' @return An object of class "qw" that is the result of the requested operation.
#' @export multiply
multiply <- function(x, factor, units, analyte, analyte.method="ALGOR", pcode="") {
  ##
  ## Start processing, use the first argument as the core object to be returned
  retval <- x
  Len <- length(retval)
  ## Modify the data and the reporing level
	retval@.Data <- retval@.Data * factor
  retval@reporting.level <- retval@reporting.level * factor
  if(!missing(units)) # Assign units
  	retval@reporting.units <- rep(units, length.out=Len)
  if(!missing(analyte)) # Assign analyte name
  	retval@analyte.name <- rep(analyte, length.out=Len)
  retval@analyte.method <- rep(analyte.method, length.out=Len)
  if(!missing(pcode)) # Assign unique name
  	retval@unique.code <- rep(pcode, length.out=Len)
  return(retval)
}
