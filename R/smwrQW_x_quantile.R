#' Sample Quantiles
#'
#' Computes sample or estimated quantiles corresponding to the given probabilities:
#'methods for "lcens," "mcens," and "qw" data.
#'The smallest observation (censored or uncensored) corresponds
#'to a probability of 0 and the largest to a probability of 1.
#'
#' The methods available in the current version are "flipped K-M," "log ROS,"
#'"ROS," "log MLE," and "MLE." The method "flipped K-M" produces quantiles
#'using the Kaplan-Meier method on flipped data described by Helsel (2012). The
#'methods "log ROS" and "log MLE" are described by Helsel, 2012 and Helsel and
#'Cohn (1988).  The methods "ROS" and "MLE" are similar to "log ROS" and "log
#'MLE" except that no log- and back-transforms are made on the data.
#'
#' @aliases quantile.lcens quantile.mcens quantile.qw
#' @param x an object of a censored-data class whose sample quantiles are
#'wanted.  \code{NA} and \code{NaN} values are not allowed unless \code{na.rm}
#'is \code{TRUE}.
#' @param probs numeric vector of probabilities with values in [0,1].
#' @param na.rm logical; if \code{TRUE}, any \code{NA} and \code{NaN}s are
#'removed from \code{x} before the quantiles are computed.
#' @param names logical; if true, the result has a names attribute.
#' @param method the method to use for computing quantiles. See \bold{Details}.
#' @param type an integer between 1 and 9 selecting one of the nine quantile
#'algorithms described in \code{\link{quantile}}.
#' @param alpha the offset fraction to be used, depending on \code{method};
#'typically in [0, 0.5].
#' @param \dots not used, required for other methods.
#' @return An optionally named vector of the requested probabilities. The names
#'of values that would be left-censored are marked with "*."
#' @seealso \code{\link{quantile}}, \code{\link{censQuantile}}
#' @references Helsel, D.R. 2012, Statistics for censored environmental data 
#'using Minitab and R: New York, Wiley, 324 p.\cr
#'Helsel, D.R. and Cohn, T.A., 1988, Estimation of descriptive statistics for
#'multiply censored water quality data: Water Resources Research v. 24, n.
#'12, pp. 1997-2004
#' @keywords internal univar
#' @examples
#'
#'set.seed(28)
#'Xu <- rnorm(23)
#'quantile(as.lcens(Xu, 0)) 
#'
#' @importFrom stats quantile
#' @rdname quantile
#' @export
#' @method quantile lcens
quantile.lcens <- function(x, probs=seq(0, 1, 0.25), na.rm=FALSE, names=TRUE,
                           method="flipped K-M", type=2, alpha=0.4, ...) {
  ##    2012Mar07 DLLorenz Original Coding for quantile.lcens
  ##    2012Dec28 DLLorenz Roxygenized
  ##    2013Aug19 DLLorenz Added protections against empty/all missings/other mishaps
  ##
  method <- match.arg(method, c("flipped K-M", "log ROS", "ROS", "log MLE", "MLE"))
  if(!na.rm)
    if(any(is.na(x)))
      stop("missing values and NaN's not allowed if 'na.rm' is FALSE")
  if(all(is.na(x))) 
    return(quantile(NA_real_, na.rm=TRUE, names=names))
  if(method == "flipped K-M") {
    if(type != 2)
	  warning('type forced to 2 for method="flipped K-M"')
    step1 <- mdlKM(x)
    step2 <- mdlKMstats(step1, probs=probs)
    retval <- step2[[1L]]$qtiles
    if(!names)
      retval <- as.vector(retval)
    else if(any(sel <- probs < step2[[1L]]$censored)) { # Append * to censored
      names(retval)[sel] <- paste(names(retval)[sel], "*", sep="")
    }
  }
  else { # Remaining methods are all set from the corresponding mdl function
    retval <- try(switch(method,
                     "log ROS"=mdlROS(x, method=method, alpha=alpha),
                     "ROS"=mdlROS(x, method=method, alpha=alpha),
                     "log MLE"=mdlMLE(x, method=method, alpha=alpha),
                     "MLE"=mdlMLE(x, method=method, alpha=alpha)), silent=TRUE)
    if(class(retval)[1L] == "try-error" || any(is.na(retval$fitted))) { # protect against convergence failure
      retval <- quantile(NA_real_, probs=probs, na.rm=TRUE, names=names)
    } else
      retval <- quantile(retval$fitted, probs=probs, type=type, names=names)
  }
  return(retval)
}

#' @rdname quantile
#' @export
#' @method quantile mcens
quantile.mcens <- function(x, probs=seq(0, 1, 0.25), na.rm=FALSE, names=TRUE,
                           method="flipped K-M", type=2, alpha=0.4, ...) {
  ##    2013Jan22 DLLorenz Original Coding for quantile.mcens
  ##    2014Sep06 DLLorenz Additional methods
  ##
  method <- match.arg(method, c("flipped K-M", "log ROS", "ROS", "log MLE", "MLE")) 
  if(!na.rm)
    if(any(is.na(x)))
      stop("missing values and NaN's not allowed if 'na.rm' is FALSE")
  if(all(is.na(x))) 
    return(quantile(NA_real_, probs=probs, na.rm=TRUE, names=names))
  if(method == "flipped K-M") {
    if(type != 2)
	  warning('type forced to 2 for method="flipped K-M"')
    step1 <- try(mcenKM(x), silent=TRUE)
    if(class(step1)[1L] == "try-error") {
      retval <- quantile(NA_real_, probs=probs, na.rm=TRUE, names=names)
    } else {
      step2 <- mdlKMstats(step1, probs=probs)
      retval <- step2[[1L]]$qtiles
      if(!names)
        retval <- as.vector(retval)
      else {
        if(any(sel <- probs < step2[[1L]]$censored)) # Append * to censored
          names(retval)[sel] <- paste(names(retval)[sel], "*", sep="")
        if(step1$type == "left" && (any(sel <- probs > max(step1$surv))))
          names(retval)[sel] <- paste(names(retval)[sel], "!", sep="")
      }
    }
  } else { # Remaining methods are all set from the corresponding mcen function
    retval <- try(switch(method,
                     "log ROS"=mcenROS(x, method=method, alpha=alpha),
                     "ROS"=mcenROS(x, method=method, alpha=alpha),
                     "log MLE"=mcenMLE(x, method=method, alpha=alpha),
                     "MLE"=mcenMLE(x, method=method, alpha=alpha)), silent=TRUE)
    if(class(retval)[1L] == "try-error" || any(is.na(retval$fitted))) {
      retval <- quantile(NA_real_, probs=probs, na.rm=TRUE, names=names)
    } else
      retval <- quantile(retval$fitted, probs=probs, type=type, names=names)
  }

  return(retval)
}

#' @rdname quantile
#' @export
#' @method quantile qw
quantile.qw <- function(x, probs=seq(0, 1, 0.25), na.rm=FALSE, names=TRUE,
												method="flipped K-M", type=2, alpha=0.4, ...) {
	if(censoring(x) == "multiple") {
		retval <- quantile.mcens(as.mcens(x), probs=probs, na.rm=na.rm,
														 names=names, method=method, type=type,
														 alpha=alpha, ...)
	} else
		retval <- quantile.lcens(as.lcens(x), probs=probs, na.rm=na.rm,
														 names=names, method=method, type=type,
														 alpha=alpha, ...)
	return(retval)
}