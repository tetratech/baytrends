#'Multiple Comparisons
#'
#'Performs a multiple nonparametric comparison test among groups of left-censored
#'data.
#'
#'
#'@param x the observations the data are forced to class "lcens." Missing
#'values (NAs) are allowed and removed before the test is performed.
#'@param groups any group vector for the observations. Missing values (NAs) are
#'allowed and removed before the test is performed.
#'@param method the method for adjustment of p-value. See
#'\code{\link{p.adjust}} for options and details.
#'@param alpha the significance level of the test.
#'@return An object of class "censMCT."
#'@note need some kind of general description.
#'@seealso \code{\link{censKSample.test}}
#'@references Helsel, D.R. 2012, Statistics for censored environmental data 
#'using Minitab and R: New York, Wiley, 324 p.
#'@keywords censored nonparametric htest
#'@examples
#'
#'set.seed(69)
#'Xu <- rlnorm(22, 0, 1)
#'Yu <- rlnorm(22, .4, 1)
#'Zu <- rlnorm(22, 1, 1)
#'Gu <- factor(rep(c("X", "Y", "Z"), each=22))
#'# Censor the data at 1
#'censKSample.test(as.lcens(c(Xu, Yu, Zu), 1), Gu)
#'censMulticomp.test(as.lcens(c(Xu, Yu, Zu), 1), Gu)
#'
#'@export
censMulticomp.test <- function(x, groups, method="holm", alpha=0.05) {
  ## Coding history:
  ##    2012Sep18 DLLorenz Initial Coding
  ##    2012Dec28 DLLorenz Roxygenized
  ##    2012Dec28          This version
  ##
  xname=deparse(substitute(x))
  gname=deparse(substitute(groups))
  x <- as.lcens(x)
  gps <- as.character(unique(na.omit(groups)))
  mtc <- combn(gps, 2)
  N <- ncol(mtc)
  ## Test reverts to simple test if only 2 groups!
  if(N == 1)
    return(genWilcox.test(x, groups))
  Z <- P <- double(N)
  for(i in seq(N)) {
    Indiv <- genWilcox.test(x[groups == mtc[1, i]], x[groups == mtc[2, i]])
    Z[i] <- Indiv$statistic
    P[i] <- Indiv$p.value
  }
  tbl <- cbind(Zscore=Z, P.adjusted=p.adjust(P, method=method))
  rownames(tbl) <- paste(mtc[1,], mtc[2,], sep=" - ")
  retval <- list(title="Nonparametric left-censored multicomparison test",
                 method=method, alpha=alpha, response=xname, groups=gname,
                 table=tbl)
  class(retval) <- "censMCT"
  return(retval)
}
