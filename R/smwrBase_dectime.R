#' @title Decimal Time
#'
#' @description Convert date/time data to be expressed as year and fractional part of year. This
#'can be useful for plotting or representing time in a regression model.
#'
#' @details The format for \code{times} must be one of "hm," "hms," or "ms." Note that
#'this is actually a conversion function, see \bold{See Also}. If \code{times}
#'is missing, \code{dates} is class "Date," and \code{Date.noon} is \code{TRUE},
#'then set the time to 12:00, so that the decimal time represents the center of
#'the day.
#'
#' Added from smwrBase.
#'
#' @param dates a vector of a valid date object, or character representation of
#'dates. Missing values are permitted and produce corresponding missing
#'values in the output.
#' @param times a character representation of times. Missing values are
#'permitted and produce corresponding missing values in the output.
#' @param time.format format to convert \code{times}. See \bold{Details}.
#' @param date.format format to convert \code{dates} is character.
#' @param Date.noon logical, if \code{TRUE} and \code{dates} is class "Date," then
#'set set the time to noon, otherwise no time adjustment is made. See \bold{Details}.
#' @param year.type a character string indicating the type of year to determine the
#'offset, must be one of "calendar," "water," or "climate."
#' @return A vector representation of the data in decimal format--year and
#'decimal fraction.
# baytrends, comment out links below
# @seealso
#Flip for production/manual
#\code{\link[lubridate]{hm}}, \code{\link[base]{strptime}}
#\code{hm} (in lubridate package), \code{strptime} (in base package)
#' @keywords internal manip
#' @examples
#'
#'dectime("11/11/1918", date.format="%m/%d/%Y")
#'dectime(1988:1990)
#' @export
dectime <- function(dates, times, time.format, date.format, Date.noon=TRUE,
                    year.type=c("calendar", "water", "climate")) {
  ## Coding history:
  ##    2004Nov16 DLLorenz Original
  ##    2011May29 DLLorenz Conversion to R
  ##    2011Jul01 DLLorenz decimal_date does not handle missing values
  ##    2012Jun04 DLLorenz Added option to just pass dates if numeric
  ##    2013Feb02 DLLorenz Prep for gitHub
  ##    2013Jun19 DLLorenz Added Date.noon argument
  ##
  if(is.numeric(dates))
    return(dates)
  if(is.character(dates))
    dates <- as.Date(dates, format=date.format)
  if(!missing(times)) {
    ConvTime <- get(time.format)
    times <- ConvTime(times)
    dates <- dates + times
  } else if(class(dates)[[1L]] == "Date" && Date.noon)
    dates <- dates + hm("12:00")
  if(any(is.na(dates)))
    dates <- na2miss(dates, "0000-01-01") # Force to 0
  retval <- decimal_date(dates)
  retval[retval == 0] <- NA
  year.type <- match.arg(year.type)
  # These are the adjustments used in seasonPlot (smwrGraphs)
  if(year.type == "water")
    retval <- retval - 0.7485
  else if(year.type == "climate")
    retval <- retval - 0.248
  return(retval)
}
