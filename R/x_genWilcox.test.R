#' @title Test for Difference in Censored Samples
#'
#' @description This function tests for differences in censored samples from 2
#'groups. Two methods are available---the Peto-Prentice test is
#'appropriate only for left-censored data. The Gehan test has
#'been extended to multiply censored data as suggested in
#'Gehan (1965) but uses a permutation test to compute the 
#'variance.
#'
#' @details If \code{y} is either type character or factor, then it is assumed to be a group
#'identifier. Anything else is treated as another set of sample and forced to the
#'appropriate class of censored data.
#'
#' The argument \code{method} must be one of "peto" or "gehan." It may also
#'be "best," which selects "peto" for  uncensored or 
#'left-censored data and "gehan" otherwise.
#'
#' @importFrom survival survfit Surv
#' @param x the samples from each group. Forced to the appropriate class. 
#'Missing values are removed before the analysis.
#' @param y either another set of samples or a group identifier with exactly
#'two groups. Missing values are removed before the analysis. See \bold{Details}.
#' @param alternative character string describing the alternative hypothesis.
#'Must be one of "two.sided, "greater," or "less."
#' @param method a character string indicating which method
#'to use for the analysis. See \bold{Details}.
#' @param data.names character string to be used to explain 
#' the data. Default names are derived from the data arguments.
#' @param gehan.seed an integer value to set the seed to
#'compute the variance of the Gehan statistic. If 0 (the default), then
#'use equation 9.5 from Helsel (2012) to compute the variance.
#' @return An object of class "htest" that inherits "genWilcox."
#' @note The \code{genWilcox.test} 
#'function uses the \code{survfit} function. Helsel (2012) describes flipping
#'the left-censored data so that small values become large and left-censored
#'values become right-censored values and adapt nonparametric techniques from
#'survival analysis. The results from \code{survfit} are printed as the sample
#'estimates when printing the output, the important columns are records,
#'the number in each group; events, the number of uncensored values; and 
#'median, the group median.\cr
#'A \code{plot} method is supported for the returned object.
#'
#' @section Null Hypothesis: The null hypothesis is that the
#'distributions are not different from one another.
#' @seealso \code{\link{survdiff}}, \code{\link{survfit}},
#'\code{\link{lcens-class}}
#' @references Gehan, E.A., 1965, A generalized Wilcoxon test for comparing
#'arbitraritly singly censored samples: Biometrika, v. 52, p. 203-223.\cr
#'
#'Harrington, D.P., and Fleming, T.R., 1982, A class of rank test procedures
#'for censored survival data: Biometrika, v. 69, p. 553-566.\cr
#'
#'Helsel, D.R. 2012, Statistics for Censored Environmental Data Using Minitab
#'and R: New York, Wiley, 324 p.\cr
#'
#'Peto, R., and Peto, J., 1972, Asymptotically efficient rank invariant test
#'procedures (with discussion): Journal of the Royal Statistical Society,
#'Series A v. 135, p. 185-206.\cr
#'
#'Prentice, R.L. 1978, Linear rank tests with right-censored data: Biometika, v
#'65, p 167-179.\cr
#'
#'Prentice, R.L., and Marke, P., 1979, A qualitative discrepancy between
#'censored data rank tests: Biometrika, v. 35, p. 861-867.\cr
#'
#' @keywords censored htest
#' @examples
#'
#'# Compare uncensored results
#'# First for grouped data
#'set.seed(69)
#'Xu <- rlnorm(22, 0, 1)
#'Yu <- rlnorm(22, .6, 1)
#'genWilcox.test(Xu, Yu)
#'wilcox.test(Xu, Yu)
#'# Compare effect of censoring
#'genWilcox.test(as.lcens(Xu, 1), Yu)
#'genWilcox.test(as.lcens(Xu, 1), as.lcens(Yu, 1))
#'
#' @export
genWilcox.test <- function(x, y, alternative="two.sided", 
                           method="best", data.names, gehan.seed=0) {
  ## Coding history:
  ##    2005Mar08 DLLorenz Initial Coding.
  ##    2005Jul14 DLLorenz Fixed date
  ##    2007Jan29 DLLorenz Modified to fail on infoRich coding
  ##    2012Aug13 DLLorenz Conversion to R and split ppw test
  ##    2013Jan01 DLLorenz Roxygenized
  ##    2013Sep03 DLLorenz Bug Fix for missing values
	##    2013Dec06 DLLorenz Added computations of estimates (median)
  ##    2014Jan02 DLLorenz Added Gehan test
  ##
  ## Error checks:
  if(missing(data.names)) {
    if(inherits(y, "factor")) {
      data.names <- levels(y)
      data.names <- paste(data.names[1L], "and", data.names[2L], sep=" ")
      num.y <- FALSE
    } else if(inherits(y, "character")) {
      data.names <- unique(y)
      data.names <- paste(data.names[1L], "and", data.names[2L], sep=" ")
      num.y <- FALSE
    } else { # get names from x and y
    data.names <- paste(deparse(substitute(x)), "and",
                        deparse(substitute(y)), sep=" ")
    num.y <- TRUE
    }
  }
  ## Set up methods
  method <- match.arg(method, c("best", "peto", "gehan"))
  if(method == "best") {
    if(censoring(x) == "multiple" || censoring(y) == "multiple"
       || class(x)[1L] == "mcens" || class(y)[1L] == "mcens" ) {
      method <- "gehan"
    } else
      method <- "peto"
  }
  if(method == "peto") {
    converter <- as.lcens
  } else
    converter <- as.mcens
  ## OK, do the test
  x <- converter(x)
  ## Create stacked data and group column
  if(num.y) {
    y <- converter(y)
    ## Remove missings
    x <- x[!is.na(x)]
    y <- y[!is.na(y)]
    group = factor(c(rep('x', length(x)), rep('y', length(y))), levels=c('x', 'y'))
    data <- c(x, y)
  }
  else { # Use y as group
    ## Remove missings
    keep <- !(is.na(x) | is.na(y))
    x <- x[keep]
    y <- y[keep]
    data <- x
    group <- as.factor(y) # make it a factor
    if(length(levels(group)) != 2L)
      stop("y must have exactly two levels")
  }
  alternative <- pmatch(alternative, c('two.sided', 'greater', 'less'))
  if(is.na(alternative))
    stop("Invalid choice for alternative, must match 'two.sided', 'greater', or 'less'")
  ## define the tests:
  PetoPrentice.test <- function(values, cenflag, group) {
    ## Perform the Kaplan-Meier analysis
    ## Note that the value is not important, and ranking preserves the link
    values <- rank(values)
    ## Required adjustment to values
    values <- ifelse(cenflag, values-.1, values)
    df <- data.frame(values=-values, cenflag=!cenflag)
    kmout <- survfit(Surv(values, cenflag) ~ 1, data=df, conf.type="none")
    kmout$time <- - kmout$time # convert back to actual ranks
    ## Define the link between the observed data and the K-M table
    ## and compute needed stats.
    link <- match(values, kmout$time)
    St <- kmout$surv[link]
    Stm1 <- c(1,kmout$surv) # dummy to create valid value for t=1
    Stm1 <- Stm1[link] # now associated with correct value
    U <- ifelse(cenflag, Stm1 - 1, St + Stm1 - 1)
    nums <- tapply(U, group, length)
    retval <- list(W=tapply(U, group, sum),
                   VarW=sum(U^2)*nums[1L]*nums[2L]/length(U)/(length(U)-1),
                   Ngroup=nums)
    ## retval is the computed W and the variance of W.
    return(retval)

  } # End of P-P.test
  Gehan.test <- function(values, group) {
    ## Perform the Gehan test as described in Helsel
    levs <- levels(group)
    ## Compute W, its variance and the number in each group
    W <- gehanScores(values[group==levs[1L]],
                     values[group==levs[2L]])
    W <- sum(W)
    nums <- table(group)
    ## Set up the permutation test
    gs <- double(1000)
    N <- length(group)
    if(gehan.seed) { # true if not 0
    	set.seed(gehan.seed)
    	for(i in seq(1000)) {
    		rs <- sample(N, nums[1L], replace=FALSE)
    		gs[i] <- sum(gehanScores(values[rs], values[-rs]))
    	}
    	VarW <- mean(gs^2) # Assume true mean of 0
    } else { # 0 use eqn 9.5 from Helsel 2012
    	H2 <- gehanScores(values)^2
    	VarW <- nums[1L]*nums[2L]*sum(H2)/(N*(N-1))
    }
    return(list(W=W, VarW=VarW, Ngroup=nums))
  }
  if(method == "peto") {
    ret1 <- PetoPrentice.test(data@.Data[,1L], data@censor.codes, group)
    stat <- ret1$W[1L] / sqrt(ret1$VarW)
    names(stat) <- "Peto-Prentice Z"
    meth <- "Peto-Prentice generalized Wilcoxon test"
  } else {
    ret1 <- Gehan.test(data, group)
    stat <- ret1$W[1L] / sqrt(ret1$VarW)
    names(stat) <- "Gehan Z"
    meth <- "Gehan generalized Wilcoxon test"
  }
  param <- ret1$Ngroup
  names(param) <- c("n", "m")
  ## Add finishing touches
  if(alternative == 1L)
    pvalue <- (1. - pnorm(abs(stat))) * 2.
  else if(alternative == 2)
    pvalue <- 1. - pnorm(stat)
  else # alternative is less than
    pvalue <- pnorm(stat)
  mu <- 0
  names(mu) <- "difference"
  ## Compute the median for each group
  ret2 <- survfit(Surv(-data@.Data[,1L], !data@censor.codes) ~ group, conf.type="none")
  ret2$time <- - ret2$time # convert back to actual values
  ret2$call <- NULL # remove unecessary info
  retval <- list(statistic = stat, parameters = param,
                 p.value = pvalue, null.value = mu,
                 alternative = c('two.sided', 'greater', 'less')[alternative],
                 method = meth, data.name = data.names,
  							 estimate=ret2)
  oldClass(retval) <- c("htest", "genWilcox")
  return(retval)
}
