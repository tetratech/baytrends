#'Extract Model Fitted Values
#'
#' Extracts the fitted values from a censored regression object: method 
#'for "censReg" object.
#'
#' @param object an object of class "censReg"---output from \code{censReg}
#' @param suppress.na.action logical, suppress the effects of the
#'\code{na.action} in the call to \code{censReg} and return only the fitted
#'values corresponding to the fitted data.
#' @param type the type of fitted value. To get fitted values on the
#'original scale of the response, use "response." For back-transformed log-normal 
#'fitted values, use "link." To get the unbiased estimates of the
#'mean for log-normal data, use "mean." Note that "link" and "mean" do not work
#'for the common-log distribution.
#' @param \dots further arguments passed to or from other methods.
#' @return The fitted values from the regression.
#' @seealso \code{\link{censReg}}
#' @keywords internal regression
#' @export
#' @method fitted censReg
fitted.censReg <- function(object, suppress.na.action=FALSE, 
													 type=c("response", "link", "mean"), ...) {
  ## Coding history:
  ##    2012Sep25 DLLorenz Initial Coding
  ##    2012Dec31 DLLorenz Roxygenized
  ##    2014Aug01 DLLorenz Added type argument
  ##
	type <- match.arg(type)
	if(type == "mean") {
		fits <- object$YPRED
	} else {
		fits <- as.vector(object$XLCAL %*% object$PARAML[1L:object$NPAR])
		if(type == "link" && object$LogNorm)
			fits <- exp(fits)
	}
	if(is.null(object$na.action) || suppress.na.action)
    return(fits)
  else return(naresid(object$na.action, fits))
}
