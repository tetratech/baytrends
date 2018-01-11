#' Mean with Confidence Limits
#' 
#' Computes the mean and confidence limits of left-censored data.
#' 
#' @details The methods available in the current version are "log MLE," "MLE," "log ROS,"
#'"ROS," "log AMLE," and "AMLE." The methods "log ROS," "log MLE," and "log AMLE" are
#'described by Helsel (2012) and Helsel and Cohn (1988).  The methods "ROS,"
#'"MLE," and "AMLE" are similar to the previous except that no log- and
#'back-transforms are made on the data. For "log" methods, if any value in
#'\code{x} is negative, then a warning is generated and the returned value
#'contains missing values.
#'
#' @param x any R object than can be converted to class "lcens," or "mcens"
#'for multiply censored data, to compute the mean. 
#'Missing values are permitted and are removed before computing the mean.
#' @param method the method to use for computing the statistics. See
#'\bold{Details}.
#' @param CI the minimum desired confidence interval for each level specifed in
#'probs.
#' @param bound a character string indicating the desired bounds, "two.sided"
#' means the two-sided interval, "upper" means the upper bound of the interval,
#' and "lower" means the lower bound of the interval. Only a single character
#' is needed. The lower confidence limit is \code{-Inf} or 0 when \code{bound} is
#' "upper" and the upper confidence limit is \code{Inf} when \code{bound} is
#' "lower."
#' @param alpha the offset fraction to be used, depending on \code{method};
#'typically in [0,1].
#' @return A matrix of the mean, the lower confidence limit, the upper
#' confidence limit, and the probability represented by the confidence interval.
#' @note Helsel (2012) describes computing the confidence interval for the mean 
#'of normally dsitributed data in Sections 7.4.1 and 7.4.2; those approaches are 
#'used for \code{method} "MLE" and "AMLE." Helsel (2012) describes computing the 
#'confidence interval for the mean of lognormally dsitributed data in Sections 
#'7.5.1 and 7.5.2; those approaches are used for \code{method} "log MLE" and 
#'"log AMLE." Helsel (2012) describes bootstrapping the confidence interval for 
#'the mean when \code{method} is "ROS" or "log ROS" in Section 7.6. For this 
#'function, the number of replicates is set to a value to guarantee at least 50
#'replicate values lie outside of the requested confidence interval. The 
#'percentage type confidence interval is reported.
#' @seealso \code{\link{censStats}}
#' @references Helsel, D.R. 2012, Statistics for censored environmental data 
#'using Minitab and R: New York, Wiley, 324 p.
#'
#' @importFrom boot boot boot.ci
#' @keywords internal univar
#' @examples
#' 
#' ## Generate a random lognormal sample
#' set.seed(221)
#' XX.rn <- rlnorm(30)
#' # Compare log AMLE and log ROS
#' censMean.CI(XX.rn, method="log AMLE", CI=.80)
#' censMean.CI(XX.rn, method="log ROS", CI=.80)
#' # check upper bound for AMLE
#' censMean.CI(XX.rn, method="log AMLE", CI=.90, bound="upper")
#' @export censMean.CI
censMean.CI <- function(x, method="log AMLE", CI=0.90, bound=c("two.sided", "upper", "lower"),
											alpha=0.4) {
	##
	method <- match.arg(method, c("log MLE", "MLE", "log ROS", "ROS",
																"log AMLE", "AMLE"))
	if(censoring(x) == "multiple") {
		x <- as.mcens(x)
		if(method %cn% "AMLE") {
			stop("AMLE/log AMLE only valid for uncensored and left-censored data")
		}
		ckpos <- !all(x > 0, na.rm=TRUE) # left-censored NA unless < (less than 0)
	} else {
		x <- as.lcens(x)
		ckpos <- any(x@.Data[, 1L] <= 0)
	}
	x <- x[!is.na(x)] # keep the good ones
	N <- length(x)
  bound <- match.arg(bound)
  if(bound == "two.sided") {
  	ci <- 1 - (1 - CI)/2
  } else {
  	ci <- CI
  }
	# Figure out which to do
	cknot0 <- TRUE
	if(method %cn% "log" && ckpos) {
		est <- lci <- uci <- NA_real_
		cknot0 <- FALSE
		warning("Not all values are strictly positive")
	} else if(method %cn% "ROS") {
		# bootstrap it:
		bmin <- ceiling(50/(1-ci))
		# make statistic
		data <- data.frame(x=x)
		stfun <- function(dt, ind, method, ...) {
			censStats(dt[ind, 1], method=method)$mean
		}
		# do it
		step1 <- boot(data, stfun, R=max(500, bmin), method=method)
		step2 <- boot.ci(step1, conf=ci, type="perc")
		est <- step2$t0
		lci <- step2$percent[4L]
		uci <- step2$percent[5L]
	} else if(method %cn% "log") { # use Olsson's modification
		step1 <- censStats(x, method=method)
		est <- step1$mean
		# #adjust the sdlog if AMLE
		tOlsson <- qt(ci, N-1)*sqrt(step1$sdlog^2/N + step1$sdlog^4/(2*(N-1)))
		lci <- exp(step1$meanlog + 0.5*step1$sdlog^2 - tOlsson)
		uci <- exp(step1$meanlog + 0.5*step1$sdlog^2 + tOlsson)
	} else { # The usual
		step1 <- censStats(x, method=method)
		est <- step1$mean
		tNorm <- qt(ci, N-1)*step1$sd/sqrt(N)
		lci <- est - tNorm
		uci <- est + tNorm
	}
	if(cknot0) {
		if(bound == "upper") {
			if(method %cn% "log") {
				lci <- 0
			} else {
				lci <- -Inf
			}
		}
		if(bound == "lower") {
			uci <- Inf
		}
	}
	retval <- cbind(estimate=est, lcl=lci, ucl=uci, ci=CI)
	rownames(retval) <- "mean"
  return(retval)
}
