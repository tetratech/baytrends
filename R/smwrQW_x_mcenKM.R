#' @title Estimate Statistics
#'
#' @description Support function for computing statistics for multiply-censored data.
#'
#' @importFrom survival survfit Surv
#' @param x an object of "mcens" to compute 
#' @param group the group variable
#' @return An object of class "survfit."
#' @keywords misc
#' @export
mcenKM <- function(x, group) {
  ## Coding history:
  ##    2012Jan22 DLLorenz Initial Coding.
  ##
  ## Flip min-max, max-min
  maxval <- -x@.Data[, 1L]
  minval <- -x@.Data[, 2L]
  ## remove NAs from data
  Good.data <- !is.na(maxval)
  if(sum(Good.data) > 0) { # At least one value
    maxval <- maxval[Good.data]
    minval <- minval[Good.data]
    ## For Surv, replace infinites with NA (treats as left/right censored,
    ##  not as interval
    maxval[is.infinite(maxval)] <- NA
    minval[is.infinite(minval)] <- NA
    if(missing(group))
      retval <- survfit(Surv(minval, maxval, type="interval2") ~ 1)
    else {
      group <- group[Good.data]
      retval <- survfit(Surv(minval, maxval, type="interval2") ~ group)
    }
  }
  else # no data
    retval <- list(NoData=TRUE)
  ## Fix type if necessary, only worry about right-censored mcens data
  if(any(is.na(minval)))
    retval$type <- "left"
  return(retval)
}
