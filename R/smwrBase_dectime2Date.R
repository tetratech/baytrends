#'Date Conversion
#'
#'Convert time data expressed as year and fractional part of year to class "Date."
#'
#' Added from smwrBase.
#'
#' @param x the decimal date to convert.
#' @param Date.noon correct from noon correction for \code{dectime}.
#' @return A vector of class "Date" corresponding to each value in \code{x}.
#' @note A small value, representing about 1 minute, is added to each value in \code{x}
#'to prevent truncation errors in the conversion. This can cause some errors if
#'the data were converted from date and time data.
#' @seealso 
#Flip for production/manual
#'\code{\link{dectime}}, \code{\link[base]{as.Date}}
#\code{\link{dectime}}, \code{as.Date} (in base package)
#' @keywords internal manip
#' @export
#' @examples
#'
#'dectime("02/07/2013", date.format="%m/%d/%Y")
#'# Convert back the printed result:
#'dectime2Date(2013.103)
dectime2Date <- function(x, Date.noon=TRUE) {
  ## Add about 1 minute to the data to prevent truncation errors
  x <- x + 2.e-6
  xtrun <- trunc(x)
  xfrac <- x - xtrun
  if(Date.noon)
    xfrac <- xfrac - 0.5/366
  as.Date(xfrac * (365 + leap_year(xtrun)),
          origin=ISOdate(xtrun,1,1))
}
