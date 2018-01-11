#' Quantiles with Confidence Limits
#' 
#' Computes sample quantiles and confidence limits of left-censored data 
#'for specified probabilities.
#' 
#' @details The \code{survfit} function in the \code{survival} package is used to compute
#'the flipped Kaplan-Meier statistics. The type of confidence interval is "plain," which
#'produces symmetric confidence intervals that maintain consistency when back flipping the 
#'estimates and is consisten with the B-C method described by Helsel (2012).
#'
#' @param x any R object than can be converted to class "lcens" to compute the sample 
#'quantiles. Missung values are permitted and are removed before computing the quantiles.
#' @param probs numeric vector of desired probabilities with values between 0
#'and 1.
#' @param CI the minimum desired confidence interval for each level specifed in
#'probs.
#' @param bound a character string indicating the desired bounds, "two.sided"
#' means the two-sided interval, "upper" means the upper bound of the interval,
#' and "lower" means the lower bound of the interval. Only a single character
#' is needed. The lower confidence limit is \code{-Inf} when \code{bound} is
#' "upper" and the upper confidence limit is \code{Inf} when \code{bound} is
#' "lower."
#' @return A matrix of sample quantiles, the lower confidence limit, the upper
#' confidence limit, and the probability represented by the confidence interval
#' corresponding to the probs levels in the sorted x data. Missing values denote
#' values less than the minimum (either censored or uncensored) value. The minimum
#' value is included as the "minimum" attribute of the matrix. The maximum
#' value is included as the "maximum" attribute of the matrix.
#' @seealso \code{\link{censQuantile}}
#' @references Helsel, D.R. 2012, Statistics for censored environmental data 
#'using Minitab and R: New York, Wiley, 324 p.
#' @keywords internal univar
#' @examples
#' 
#' ## Generate a random sample
#' set.seed(222)
#' XX.rn <- rexp(32)
#' # Compare uncensored to the binomial method
#' censQtiles.CI(XX.rn, probs=c(.25, .5, .75))
#' qtiles.CI(XX.rn, probs=c(.25, .5, .75))
#' 
#' @export censQtiles.CI
censQtiles.CI <- function(x, probs=0.5, CI=0.90, bound=c("two.sided", "upper", "lower")) {
	##
	x <- as.lcens(x)
  bound <- match.arg(bound)
  if(bound != "two.sided") {
  	ci <- 1 - (1 - CI)*2
  } else {
  	ci <- CI
  }
	# Probs get flipped
	pcalc <- 1 - probs
	step1 <- mdlKM(x, conf.int=ci)
  step2 <- quantile(step1, probs=pcalc)
	# Note that the B-C sign stats could be computed for each probability level
	# and from that score, the actual CI computed:
	# 1 - (1 - pnorm(BCmax)) + pnorm(BCmin) = pnorm(BCmax) + pnorm(BCmin)
	# where BCmax is the smallest B-C value that exceeds the critical value and
	# BCmin is the largest B-C value that is less than the critical value.
	# The eqn must be tweaked for bound="lower."
	qtiles <- -step2$quantile
	if(bound == "upper") {
		lci <- 0
	} else {
		lci <- -step2$upper # Note reverses logic
	}
	if(bound == "lower") {
		uci <- Inf
	} else {
		uci <- -step2$lower
	}
  retval <- cbind(estimate=qtiles, lcl=lci, ucl=uci, ci=CI)
	# Cheat to get the names of the selected quantiles
  rownames(retval) <- names(quantile(1, probs=probs))
	attr(retval, "minimum") <- -step1$time[length(step1$time)]
	attr(retval, "maximum") <- -step1$time[1L]
  return(retval)
}
