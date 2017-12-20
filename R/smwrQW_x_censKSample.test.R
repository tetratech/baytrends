#' @title Test for difference in left-censored samples
#'
#' @description Performs a test for differences in left-censored samples from 2 
#'or more groups.
#'
#' @importFrom survival survdiff Surv
#' @param x the samples from each group. Forced to class "lcens." Missing values
#'are removed before the analysis.
#' @param groups the group identifier for each sample. Missing values are
#'removed before the analysis.
#' @param type the string "Peto" or "log-rank," depending on the desired type of
#'test. Default is "Peto."
#' @param data.names character string to be used to explain the data. Default
#'names are derived from the data arguments.
#' @return An object of class "htest."
#' @note The \code{censKSample.test} function use the \code{survdiff} function
#'to test for differences. Helsel (2012) describes flipping
#'the left-censored data so that small values become large and left-censored
#'values become right-censored values and adapt nonparametric techniques from
#'survival analysis.\cr
#'
#'Tableman and others (2004) state, "the Peto test emphasizes the beginning of
#'the survival curve in that earlier failures receive larger weights. The
#'log-rank test emphasizes the tail of the survival curve in that it gives
#'equal weight to each failure." For flipped data, earlier failures translates
#'to larger uncensored values. In general, the Peto test gives results that are
#'more similar to the Wilcoxon test if all data are uncensored.
#' @section Null Hypothesis: The null hypothesis is that the
#'distributions in each group are not different from one another.
#' @seealso \code{\link{survdiff}}, \code{\link{lcens-class}}
#' @references Helsel, D.R. 2012, Statistics for censored environmental data 
#'using Minitab and R: New York, Wiley, 324 p.\cr
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
#'Tableman, M., and Kim, J.S., 2004, Survival analysis using S---analysis of
#'time-to-event data: Boca Raton, Fla., Chapman and Hall, 280 p.\cr
#' @keywords censored htest
#' @examples
#'
#'# Compare uncensored results
#'set.seed(69)
#'Xu <- rlnorm(22, 0, 1)
#'Yu <- rlnorm(22, .4, 1)
#'Zu <- rlnorm(22, 1, 1)
#'Gu <- factor(rep(c("X", "Y", "Z"), each=22))
#'censKSample.test(c(Xu, Yu, Zu), Gu)
#'kruskal.test(c(Xu, Yu, Zu), Gu)
#'# Censor the data at 1
#'censKSample.test(as.lcens(c(Xu, Yu, Zu), 1), Gu)
#'
#' @export
censKSample.test <- function(x, groups, type="Peto", data.names) {
  ## Coding history:
  ##    2005Jul14 DLLorenz Initial dated version
  ##    2007Jan29 DLLorenz Modified to fail on infoRich coding
  ##    2012Aug13 DLLorenz Conversion to R
  ##    2012Dec28 DLLorenz Roxygenized
  ##    2012Dec28          This version
  ##
  ## determine rho for survdiff
  if(missing(data.names)) { # get names from x and y
    data.names <- paste(deparse(substitute(x)), 'by',
                        deparse(substitute(groups)), sep=' ')
  }
  rho <- pmatch(type, c("log-rank", "Peto")) - 1
  if(is.na(rho))
    stop("type must be either log-rank or Peto.")
  groups <- as.factor(groups)
  ## Remove missing values
  x <- as.lcens(x)
  nas <- is.na(x) | is.na(groups)
  x <- x[!nas]
  groups <- groups[!nas, drop=T]
  ## The test--flip data to convert to right-censoring.
  retval <- survdiff(Surv(-x@.Data[, 1], !x@censor.codes) ~ groups, rho=rho)
  stat <- retval$chisq
  names(stat) <- paste(c("log-rank", "Peto & Peto")[rho+1], "chi-square", sep=" ")
  params <- length(retval$obs) - 1
  names(params) <- "df"
  p.val <- 1 - pchisq(stat, params)
  alt <- "two.sided"
  meth <- "Left-censored k sample test"
  retval <- list(statistic=stat, parameters=params, p.value=p.val,
                 alternative=alt, method=meth, data.names=data.names)
  oldClass(retval) <- "htest"
  return(retval)
}
