#' @title Estimate Censored Values
#'
#' @description Estimates values for censored data.
#'
#' @details The methods of Regression on Order Statistics (ROS) and MLE is
#'explained in Helsel (2012). The "log ROS" first log-transforms the data and
#'back-transforms the estimated values. The triangular method distributes the
#'censored value assuming a triangular distribution between 0 and the single
#'detection limit. Quinn and Keogh (2003) describe alternatives to simple 
#'substituion of a single value that sample from an alternate distribution.
#'The triangular distribution is a reasonable distribution when the percentage
#'of censored data is relatively small, say less than 30 percent. The "fill" and
#'"log fill" methods implement the method described by Gleit (1985).
#'
#' @param x an object of class "lcens." Missing values are ignored.
#' @param method the method to use for estimating censored values:
#'"ROS," "log ROS," "MLE," or "log MLE" are valid for any left-
#'or multiply-censored data ; "triangular," "fill" and "log fill" are valid for 
#'left-censored data  with a single detection limit; \code{method} is ignored for 
#'uncensored data.
#' @param alpha the offset fraction to be used for plotting position; typically in [0,0.5].
#' @return A vector of sorted estimates and actual values with an attribute of the 
#'censoring levels.
#' @seealso \code{\link{as.double.qw}}
#' @references Helsel, D.R. 2012, Statistics for censored environmental data 
#'using Minitab and R: New York, Wiley, 324 p.\cr
#'Quinn, G.P. and Keogh, M.J., 2003, Experimental design and data analysis for 
#'biologists, Cambridge University Press, Cambridge, UK, 539 p.
#' @keywords internal manip
#' @examples
#'set.seed(5420)
#'XR <- sort(rnorm(10))
#'XF <- fillIn(as.lcens(XR, -1)) # censors lowest 3 values
#'#How'd we do?
#'rbind(XR, XF)
#'#Note that this is unusual because all 10 random values were less than the mean!
#' @export
fillIn <- function(x, method, alpha) {
  ## Coding history:
  ##    2012Mar09 DLLorenz original coding
  ##    2012Oct18 DLLorenz Added more methods
  ##    2013Jan01 DLLorenz Roxygenized
  ##    2014Sep04 DLLorenz conversion to methods
  ##
	UseMethod("fillIn")
}

#' @rdname fillIn
#' @export
#' @method fillIn numeric
fillIn.numeric <- function(x, method, alpha) {
	## The arguments method and alpha are ignored
	retval <- sort(x)
	attr(retval, "censorlevels") <- -Inf
	return(retval)
}

#' @rdname fillIn
#' @export
#' @method fillIn lcens
fillIn.lcens <- function(x, method="ROS", alpha=.4) {
	method <- match.arg(method, c("ROS", "log ROS", "MLE", "log MLE",
																"triangular"))
	temp <- switch(method,
								 "ROS" = mdlROS(x, method=method, alpha=alpha),
								 "log ROS" = mdlROS(x, method=method, alpha=alpha),
								 "MLE" = mdlMLE(x, method=method, alpha=alpha),
								 "log MLE" = mdlMLE(x, method=method, alpha=alpha),
								 "triangular" = {CL <- censorLevels(x)
								 								if(length(CL) > 1L)
								 									stop("triangular method only for single detction limit")
								 								if(CL == -Inf)
								 									return(list(fitted=sort(x@.Data[, 1L]),
								 															censorlevels=double(0)))
								 								## Create fill-in values assuming a triangular distribution
								 								x <- x[!is.na(x)]
								 								xu <- sort(x@.Data[!x@censor.codes, 1L])
								 								## protect against uncensored values less than CL
								 								if(any(xu < CL))
								 									warning("uncensored values less than the detection limit: results may be invalid")
								 								xc <- sqrt(ppoints(sum(x@censor.codes), alpha)) * CL
								 								list(fitted=c(xc, xu),
								 										 censorlevels=rep(CL, length(xc)))
								 },
								 "fill" = {ret <- sdlFill(x, method, alpha)
								           list(fitted=ret$fitted, censorlevels=ret$censorlevels)
								 },
								 "log fill" = {ret <- sdlFill(x, method, alpha)
								 list(fitted=ret$fitted, censorlevels=ret$censorlevels)
								 })
	retval <- temp$fitted
	attr(retval, "censorlevels") <- temp$censorlevels
	return(retval)
}

#' @rdname fillIn
#' @export
#' @method fillIn mcens
fillIn.mcens <- function(x, method="ROS", alpha=.4) {
	method <- match.arg(method, c("ROS", "log ROS", "MLE", "log MLE"))
	temp <- switch(method,
								 "ROS" = mcenROS(x, method=method, alpha=alpha),
								 "log ROS" = mcenROS(x, method=method, alpha=alpha),
								 "MLE" = mcenMLE(x, method=method, alpha=alpha),
								 "log MLE" = mcenMLE(x, method=method, alpha=alpha))
	retval <- temp$fitted
	attr(retval, "censorlevels") <- temp$censorlevels
	return(retval)
}
	
	
	
