#' @title Estimate Statistics
#'
#' @description Support function for computing statistics for left-censored data.
#'
#' @importFrom survival survfit Surv
#' @param x an object of "lcens" to compute.
#' @param group the group variable.
#' @param conf.int the confidence interval .
#' @return An object of class "survfit."
#' @keywords misc
#' @export
mdlKM <- function(x, group, conf.int=.95) {
  ##
  pvalues <- x@.Data[, 1]
  rvalues <- x@censor.codes
  ## remove NAs from data
  Good.data <- !is.na(pvalues)
  if(sum(Good.data) > 0) { # At least one value
    pvalues <- -pvalues[Good.data] # reverse data
    rvalues <- !rvalues[Good.data] # reverse sense for survfit
    if(missing(group)) {
      retval <- survfit(Surv(pvalues, rvalues) ~ 1, conf.int=conf.int, conf.type="plain")
    } else {
      group <- group[Good.data]
      retval <- survfit(Surv(pvalues, rvalues) ~ group, conf.int=conf.int, conf.type="plain")
    }
  }
  else # no data
    retval <- list(NoData=TRUE)
  return(retval)
}
