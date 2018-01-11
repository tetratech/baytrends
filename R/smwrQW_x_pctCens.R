#' Percent Censored
#'
#' Computes the percentage of censoring ("left," "right," or "multiple") for an 
#'object.
#'
#' @param x the object to compute the percentage of censoring. Missing values
#'are permitted and removed before computing the percentage,
#' @param type the type of censoring desired. For "left," only the percent of
#'left-censored data is computed. For "right," only the percent of
#'right-censored data is computed. For "multiple," the percentage of left-, right-,
#'and interval-censored data is returned.
#' @return The percentage of the kind of censoring specified by \code{type} is
#'returned. Zero is returned if all data are missing!
#' @note This function will work for any type of data that has a method for 
#'\code{censoring} and \code{as.mcens}.
#' @seealso \code{\link{censoring}}
#' @keywords internal censored attribute
#' @examples
#'
#'pctCens(as.lcens(c(1:5, NA), 2)) # left-censored 
#'
#' @export
pctCens <- function(x, type="multiple") {
  ## Coding history:
  ##    2013Aug21 DLLorenz Original coding
  ##
  if((xtype <- censoring(x)) == "none")
    return(0)
  type <- match.arg(type, c("left", "right", "multiple"))
  x <- x[!is.na(x)]
  N <- length(x)
  if(N == 0L) {
    return(0)
  }
  xcl <- class(x)[1L]
  if(xtype == "left" && existsMethod("as.lcens", c(xcl, "missing", "missing"))) {
    x <- as.lcens(x)
    Nl <- sum(x@censor.codes)
    Nr <- Ni <- 0L
  } else {
    x <- as.mcens(x)
    Nl <- sum(x@censor.codes == -1L)
    Nr <- sum(x@censor.codes == 1L)
    Ni <- sum(x@interval)
  }
  if(type == "left")
    return(100*Nl/N)
  if(type == "right")
    return(100*Nr/N)
  return(100*(Nl + Nr + Ni)/N)
}
