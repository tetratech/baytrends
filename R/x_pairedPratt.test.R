#' @title Test for difference in left-censored samples
#'
#' @description Tests for differences in paired left-censored samples from two
#'groups using Pratt's (1959) adjustment for ties.
#'
#' @importFrom coin wilcoxsign_test
#' @param x the paired samples to \code{y}. Forced to class "lcens." Missing 
#'values are removed before the analysis.
#' @param y the paired values to \code{x}. Missing values are removed before
#'the analysis.
#' @param alternative character string describing the alternative hypothesis for
#'(\code{x} - \code{y} compared to \code{mu}. Must be one of "two.sided," 
#'"greater," or "less." 
#' @param mu a numeric value specifying the test difference between \code{x}
#'and \code{y}. 
#' @param data.names character string to be used to explain the data. Default
#'names are derived from the data arguments.
#' @return An object of class "htest" that inherits "ppw."
#' @note The Pratt (1959) adjustment to the Wilcoxon signed-rank test was
#'used by Lindsey and Rupert (2012) to evaluate decadal changes in groundwater 
#'quality. Lindsey and Rupert (2012) used a fixed reporting level (0.06,
#'the largest among all of the data) and used a simple
#'substitute value 0.0599 for the censored values. The approach in \code{pairedPratt.test}
#'is to recensor values at the largest reporting limit, use one-half that value as the 
#'simple substitute and round differences greater than the simple substitute value
#'to multiples of the largest reporting limit; nonzero differences less than or equal to the
#'simple substitute value are rounded to that value. That rounding scheme
#'eliminates the ambiguity of comparing interval values created by the difference
#'between a censored value and an uncensored value.
#'
#'A \code{plot} method is supported for the returned object.
#'
#'
#' @section Null Hypothesis: The null hypothesis is that the
#'distributions are not different from one another.
#' @seealso \code{\link{ppw.test}}, \code{\link{lcens-class}}
#' @references Lindsey, B.D., and Rupert, M.G., 2012, Methods for evaluating temporal 
#'groundwater quality data and results of decadal-scale changes in chloride, 
#'dissolved solids, and nitrate concentrations in groundwater in the United States, 
#'1988--2010: U.S. Geological Survey Scientific Investigations Report 2012--5049, 46 p.
#'
#'Pratt, J.W., 1959, Remarks on zeros and ties in the Wilcoxon signed rank 
#'procedures: Journal of the American Statistical Association, v. 54, no. 287
#'p. 655--667.
#'
#' @keywords censored htest
#' @examples
#'
#'# Compare uncensored results
#'set.seed(699)
#'Xu <- sort(rlnorm(22, 0, 1))
#'Yu <- sort(rlnorm(22, .425, 1))
#'# Treat as paired samples
#'pairedPratt.test(Xu, Yu)
#'wilcox.test(Xu, Yu, paired=TRUE)
#'# The differences in the p-values are due to use of approximate p-value for
#'# the Pratt test
#' @export
pairedPratt.test <- function(x, y, alternative="two.sided", mu=0, data.names) {
  ##
  ## Error checks/force to lcens
  if(missing(data.names))
    data.names <- paste(deparse(substitute(x)), "and",
                        deparse(substitute(y)), sep=' ')
  x <- as.lcens(x)
  y <- as.lcens(y)
  if(length(x) != length(y))
    stop("Lengths of x and y must be the same for paired data.")
  keep <- !(is.na(x) | is.na(y))
  x <- x[keep]
  y <- y[keep]
  if(any(c(x@.Data[,1L], y@.Data[,1L]) < 0))
  	stop("Negative values in x or y")
  N <- length(x)
  alternative <- match.arg(alternative, c("two.sided", "greater", "less"))
  ## Compute the actual ranges of the differences
  mind <- ifelse(x@censor.codes, 0, x@.Data[,1L]) - y@.Data[,1L]
  maxd <- x@.Data[,1L] - ifelse(y@censor.codes, 0, y@.Data[,1L])
  ## The test:
  # Find the maximum censoring level and recensor
  maxrl <- max(x@.Data[,2L], y@.Data[,2L], 0)
  x@.Data[,2L] <- y@.Data[,2L] <- maxrl
  x@censor.codes <- x@.Data[,1L] < maxrl
  x@.Data[,1L] <- pmax(maxrl, x@.Data[,1L])
  y@censor.codes <- y@.Data[,1L] < maxrl
  y@.Data[,1L] <- pmax(maxrl, y@.Data[,1L])
  # Now compute differences and round
  SS <- maxrl/2
  x <- ifelse(x@censor.codes, SS, x@.Data[,1L])
  y <- ifelse(y@censor.codes, SS, y@.Data[,1L])
  xy <- x - y - mu
  if(maxrl > 0) { # round
  	# greater than maxrl/2
  	xy[abs(xy) > SS] <- round(xy[abs(xy) > SS]/maxrl)*maxrl
  	# less than maxrl/2
  	xy[abs(xy) < SS] <- round(xy[abs(xy) < SS]/SS)*SS
  }
  xygt0 <- abs(xy)[xy != 0]
  if(length(xygt0) < 11L && length(xygt0) == length(unique(xygt0))) {
  	dist="exact"
  } else {
  	dist="approximate"
  }
  # The test
  zero <- rep(0, N)
  RET <- wilcoxsign_test(xy ~ zero, distribution=dist, 
  											 alternative=alternative, zero.method="Pratt")
	stat <- RET@statistic@teststatistic
  names(stat) <- "Pratt Z"
  p.value <- RET@distribution@pvalue(stat)
  meth <- "Wilcoxon Rank-Sum Test with Pratt Adjustment for Zeros"
  names(mu) <- "difference"
  # recode xy as the actual scores (signed ranks)
  xy <- sign(xy) * rank(abs(xy))
  Scoremat <- cbind(x,y,xy)
  colnames(Scoremat) <- c("xScore", "yScore", "d")
  Scoremat <- cbind(Scoremat, minDiff=mind, maxDiff=maxd)
  param <- c(n=N)
  retval <- list(statistic = stat, parameters = param,
                 p.value = p.value, null.value = mu,
                 alternative = alternative,
                 method = meth, data.name = data.names,
                 PPWmat=Scoremat)
  oldClass(retval) <- c("htest", "ppw")
  return(retval)
}
