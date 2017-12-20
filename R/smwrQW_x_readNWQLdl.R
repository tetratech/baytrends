#' @title Historical Reporting Limits
#'
#' @description Imports the historical reporting limits for all methods for a 
#'parameter code.
#'
#' @importFrom XML readHTMLTable
#' @importFrom lubridate today
#' @param parm_cd the parameter code.
#' @return A data frame of the analyte, methods, begin and end date for each
#'reporting level change, the reporting level type used, the long-term
#'detection limit and the reporting level.
#' @note This function works only within the internal USGS network.
#' @seealso \code{\link{qw-class}}
#' @references Lorenz, D.L., 2014, USGSqw OFR.
#' @keywords IO
#' @examples
#'
#'\dontrun{
#'readNWQLdl("00608")
#'}
#'
#' @export
readNWQLdl <- function(parm_cd) {
  ## Coding history:
  ##    2012Sep21 DLLorenz original Coding
  ##    2012Dec28 DLLorenz Roxygenized
  ##    2012Dec28          This version
  ##
  if(missing(parm_cd))
    stop("parm_cd is required")
  parm_cd <- zeroPad(parm_cd, 5)
  myurl <- paste("http://nwql.cr.usgs.gov/usgs/limits/limits.cfm?st=p&ss=",
                 parm_cd, sep="")
  retval <- readHTMLTable(myurl, stringsAsFactors=FALSE)[[3]] # that is the one
  names(retval) <- gsub(" ", "", names(retval)) # remove spaces
  ## Fix the columns
  warn <- options("warn")
  options(warn=-1) # Supress NAs by coercion messages
  retval$StartDate=as.Date(retval$StartDate, format="%Y%m%d")
  retval$EndDate=as.Date(retval$EndDate, format="%Y%m%d")
  retvalReportLevelCode=toupper(retval$ReportLevelCode)
  retval$DetectionLevel=as.numeric(retval$DetectionLevel)
  retval$ReportingLevel=as.numeric(retval$ReportingLevel)
  # Fix NAs in the date--to be able to make range comparisons
  retval$StartDate[is.na(retval$StartDate)] <- as.Date("1900-01-01")
  retval$EndDate[is.na(retval$EndDate)] <- today()
  ## Restore warning and return data
  options(warn)
  return(retval)
}
