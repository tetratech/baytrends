#' Extract the Number of Observations from a Fit
#'
#' Extracts the number of observations from a censored regression object: 
#'method for "censReg" object.
#'
#' @param object an object of class "censReg"---output from \code{censReg}.
#' @param \dots further arguments passed to or from other methods.
#' @return The number of observations used to construct the model.
#' @seealso \code{\link{censReg}}
#' @keywords regression
#' @importFrom stats nobs
#' @export
#' @method nobs censReg
nobs.censReg <- function(object, ...)
  ## Coding history:
  ##    2012Dec31 DLLorenz Initial Coding
  ##    2013Jan21 DLLorenz Fixed component name
  object$NOBSC
