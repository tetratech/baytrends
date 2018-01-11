#' @title Base Day
#'
#' @description Computes the base day of the year, a reference value that can be 
#'used to group days for the computation of summary statistics.  From smwrBase package.
#'
#' @details The base day is computed such that all dates have the same reference value
#'regardless of whether the year is a leap year or not. If \code{year} is 
#'"calendar," then the factor levels or day number begin on January 1; if \code{year} is
#'"water," then the factor levels or day number begin on October 1; and if \code{year} is
#'"climate," then the factor levels or day number begin on April 1.
#'
#' @param x a vector of class POSIXt, Dates, or character that represents a
#'date. Missing values are permitted.
#' @param numeric a logical value; \code{TRUE} means return the numeric value
#'of the day, \code{FALSE} means return a factor.
#' @param year a character string indicating the basis of the factor levels. See
#'\bold{Details}.
#' @return An integer value representing the base day number if \code{numeric}
#'is \code{TRUE}. Otherwise a factor with levels for every day of the year.
#' @import digest lubridate memoise
#' @keywords internal manip
#' @export
#' @examples
#'
#'# The default numeric result
#'baseDay(c("2000-02-29", "2000-03-01", "2001-03-01"))
#'# The result as a factor
#'baseDay(c("2000-02-29", "2000-03-01", "2001-03-01"), numeric=FALSE)
baseDay <- function(x, numeric=TRUE, year=c("calendar", "water", "climate")) {
  ##
  x <- as.POSIXlt(x)
  retval <- format(x, format="%b %d")
  year=match.arg(year)
  if(year == "calendar") {
    levels <- format(seq(as.Date("2000-01-01"), by=1, length.out=366),
                     format="%b %d")
  } else if(year == "water") {
    levels <- format(seq(as.Date("1999-10-01"), by=1, length.out=366),
                     format="%b %d")
  } else
    levels <- format(seq(as.Date("1999-04-01"), by=1, length.out=366),
                     format="%b %d")
  retval <- factor(retval, levels=levels)
  if(numeric)
    retval <- as.integer(retval)
  return(retval)
}
