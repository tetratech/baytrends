#' @title Test for difference in left-censored samples
#'
#' @description Tests for differences in paired left-censored samples from two
#'groups using the PPW test (O'Brien and Fleming, 1987).
#'
#' @details The O'Brien-Fleming adjustment forces the score for all paired censored
#'values to be equal to each other. It also forces equality between observed
#'values that are less than the detection limit in the paried sample by setting
#'the value to less than the detection limit.
#'
#' @importFrom survival survfit Surv
#' @param x the paired samples to \code{y}. Forced to class "lcens." Missing 
#'values are removed before the analysis.
#' @param y the paired values to \code{x}. Missing values are removed before
#'the analysis.
#' @param alternative character string describing the alternative hypothesis.
#'Must be one of "two.sided," "greater," or "less."
#' @param OBrienFleming logical, if \code{TRUE}, then apply the OBrienFleming otherwise it is
#'not applied. See \bold{Details}.
#' @param data.names character string to be used to explain the data. Default
#'names are derived from the data arguments.
#' @return An object of class "htest" that inherits "ppw."
#' @note The \code{ppw.test}
#'function uses the \code{survfit} function. Helsel (2012) describes flipping
#'the left-censored data so that small values become large and left-censored
#'values become right-censored values and adapt nonparametric techniques from
#'survival analysis.\cr
#'A \code{plot} method is supported for the returned object.
#'
#' @section Null Hypothesis: The null hypothesis is that the
#'distributions are not different from one another.
#' @seealso \code{\link{survdiff}}, \code{\link{survfit}},
#'\code{\link{lcens-class}}
#' @references
#'Helsel, D.R. 2012, Statistics for Censored Environmental Data Using Minitab
#'and R: New York, Wiley, 324 p.\cr
#'
#'O'Brien, P.C. and Fleming, T.R., 1987, A paired Prentice-Wilcoxon test for censored 
#'paried data: Biometrics, v. 43, p. 451--455.
#'
#' @keywords censored htest
#' @examples
#'
#'# Compare uncensored results
#'set.seed(699)
#'Xu <- sort(rlnorm(22, 0, 1))
#'Yu <- sort(rlnorm(22, .425, 1))
#'# Treat as paired samples
#'ppw.test(Xu, Yu)
#'wilcox.test(Xu, Yu, paired=TRUE)
#'
#' @export
ppw.test <- function(x, y, alternative="two.sided", OBrienFleming=TRUE,
                     data.names) {
  ## Coding history:
  ##    2005Mar08 DLLorenz Initial Coding.
  ##    2005Jul14 DLLorenz Fixed date
  ##    2007Jan29 DLLorenz Modified to fail on infoRich coding
  ##    2011Apr25 DLLorenz Modified to use the O'Brien-Fleming correction for
  ##                       common censoring between two observations
  ##    2012Aug13 DLLorenz Conversion to R and split ppw test
  ##    2013Jan05 DLLorenz Roxygenized
  ##    2013Jan21 DLLorenz Added info for diagnostic plots
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
  if(OBrienFleming) {
    xd <- x@.Data[,1L]
    xc <- x@censor.codes
    yd <- y@.Data[,1L]
    yc <- y@censor.codes
    for(i in seq(N)) {
      if(xc[i] && yc[i]) { # Both censored, make same
        xyMax <- max(xd[i], yd[i])
        x@.Data[i, 1L] <- y@.Data[i, 1L] <- xyMax
      }
      else if(xc[i] && xd[i] > yd[i]) { # x censored > y
        y@.Data[i, 1L] <- xd[i]
        y@censor.codes[i] <- TRUE
      }
      else if(yc[i] && yd[i] > xd[i]) { # y censored > x
        x@.Data[i, 1L] <- yd[i]
        x@censor.codes[i] <- TRUE
      }
    } # Done, no need to check uncensored observations
  }
  ## Test requires stacked data
  group = factor(c(rep("x", length(x)), rep("y", length(y))),
    levels=c("x", "y"))
  data <- c(x, y)
  ## Data must be rounded for computations within survfit
  data@.Data[,1L] <- signif(data@.Data[,1L], 9)
  alternative <- pmatch(alternative, c("two.sided", "greater", "less"))
  if(is.na(alternative))
    stop('Invalid choice for alternative, must match "two.sided", "greater", or "less"')
  ## Define the test:
  PPW.test <- function(values, cenflag, group) {
    ## data must be in exactly 2 groups of equal size and stacked
    ## x first, then y (for alternative not two.sided)
    ## required adjustment to values
    adjust <- min(diff(sort(unique(values))))/10.
    values <- ifelse(cenflag, values-adjust, values)
    adjust <- adjust / length(values)
    dupes <- unique(values[!cenflag][duplicated(values[!cenflag])])
    in.values <- values
    if(length(dupes) > 0) { #there are dupes
      for(i in seq(along=dupes)) {
        sel <- which(values == dupes[i])
        mult <- 0L:(length(sel) - 1L)
        in.values[sel] <- dupes[i] + adjust * mult
      }
    }
    ## create data frame and add observed value at 0 to compute
    ## correct probs
    df <- data.frame(values=c(0, -in.values), cenflag=c(T, !cenflag))
    kmout <- survfit(Surv(values, cenflag) ~ 1, data=df, na.action=na.exclude)
    kmout$time <- - kmout$time # convert back to actual values
    ## compute mean survival for tied values
    St <- kmout$surv
    if(length(dupes) > 0L) { #there are dupes
      for(i in seq(along=dupes)) {
        ## take advantage of the fact that the output are sorted
        sel <- which(values == dupes[i])
        mult <- seq(0L, 1L - length(sel))
        sel <- which(kmout$time == dupes[i])
        St[sel] <- mean(St[sel + mult])
      }
    }
    
    ## Define the link between the observed data and the kaplan meier table
    ## and compute needed stats.
    link <- match(values, kmout$time)
    St <- St[link]
    Uncen <- values[!cenflag]
    UncenSt <- St[!cenflag]
    UncenSt <- UncenSt[order(Uncen)]
    Uncen <- sort(Uncen) 
    Score <- 1. - 2*St
    ## This is not fast, but it works
    for(i in which(cenflag)) # fix each each censored score
      Score[i] <- 1. - UncenSt[which(Uncen > values[i])][1L]
    ## Compute d
    ## Reverse sense of d so that alternatives are in same direction--
    ## gives different sense of d from the original
    Score <- matrix(Score, ncol=2L)
    d <-  Score[, 2L] - Score[, 1L]
    return(list(Z=sum(d)/sqrt(sum(d^2)), Scores=Score, Diffs=d))
  } # end of PPW.test
  ret1 <- PPW.test(data@.Data[, 1L], data@censor.codes, group)
  stat <- ret1$Z
  names(stat) <- "Paired Prentice Z"
  meth <- "paired Prentice-Wilcoxon test"
  param <- length(group)/2L
  names(param) <- "n"
  ## add finishing touches
  if(alternative == 1L)
    pvalue <- (1. - pnorm(abs(stat))) * 2.
  else if(alternative == 2L)
    pvalue <- 1. - pnorm(stat)
  else # alternaitve is less than
    pvalue <- pnorm(stat)
  mu <- 0
  names(mu) <- "difference"
  Scoremat <- cbind(ret1$Scores, ret1$Diffs)
  colnames(Scoremat) <- c("xScore", "yScore", "d")
  ## For diagnostic plot, create min and max differences
  d1 <- x@.Data[, 1L] - ifelse(y@censor.codes, 0, y@.Data[, 1L])
  d2 <- ifelse(x@censor.codes, 0, x@.Data[, 1L]) - y@.Data[, 1L]
  mind <- pmin(d1, d2)
  maxd <- pmax(d1, d2)
  Scoremat <- cbind(Scoremat, minDiff=mind, maxDiff=maxd)
  retval <- list(statistic = stat, parameters = param,
                 p.value = pvalue, null.value = mu,
                 alternative = c("two.sided", "greater", "less")[alternative],
                 method = meth, data.name = data.names,
                 PPWmat=Scoremat)
  oldClass(retval) <- c("htest", "ppw")
  return(retval)
}
