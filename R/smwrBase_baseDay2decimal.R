#'Base Day
#'
#'Computes the decimal time representation of the base day of the year.  
#'From smwrBase package.
#'
#'
#' @param x a vector of baseDay values, character, or factors of the form month 
#'abbreviation and day number, generally created from \code{baseDay}. Missing 
#'values are permitted and result in missing values in the output. Unmatched 
#'values also result in missing values in the output.
#' @return A numeric value representing the base day.
#' @keywords manip
#' @seealso \code{\link{baseDay}}
#' @examples
#'# The baseDay ordered by calendar year
#'bd.tmp <- baseDay(c("2000-02-29", "2000-03-01", "2001-03-01"), 
#'   numeric=FALSE)
#'baseDay2decimal(bd.tmp)
#'# ordered by water year, result should agree
#'bd.tmp <- baseDay(c("2000-02-29", "2000-03-01", "2001-03-01"), 
#'   numeric=FALSE, year="water")
#'baseDay2decimal(bd.tmp)
#' @export
baseDay2decimal <- function(x) {
  ## Coding history:
  ##    2014Apr29 DLLorenz Initial coding
  ##
  x <- as.character(x)
  bds <- baseDay(seq(as.Date("2000-01-01"), by="day", length.out=366),
                 numeric=FALSE)
  retval <- (match(x, bds) - 1)/366
  return(retval)
}
