#' Probability Plotting Positions
#'
#' Generates the sequence of probability points for uncensored and censored
#'values
#'
#'
#' @aliases censpp censpp.default censpp.lcens censpp.mcens
#' @param x a vector of observations
#' @param a the offset fraction to be used; typically in [0, 0.5].
#' @return A list with these components:
#'
#'Note that \code{xrcn} and \code{pprcn} are provided only if there are
#'right-censored values.
#'\item{x}{ sorted uncensored observations.}
#'\item{pp}{ corresponding plotting positions for uncensored observations.}
#'\item{xcen}{ sorted left-censored observations.}
#'\item{ppcen}{ corresponding plotting positions for left-censored
#'observations.}
#'\item{xrcn}{ sorted right-censored observations.}
#'\item{pprcn}{ corresponding plotting positions for right-censored
#'observations.}
#' @note The left-censored plotting positions are adjusted by the method of
#'Helsel and Cohn (1988), which produces a mean of 0.5 for all data.
#' @seealso \code{\link{ppoints}}
#' @references Helsel, D.R. and Cohn, T.A., 1988, Estimation of descriptive
#'statistics for multiply censored water quality data: Water Resources
#'Research v. 24, n. 12, pp.1997-2004
#' @keywords internal manip
#' @examples
#'# This example demonstrates the probability adjustments made for censoring
#'# The raw, uncensored data:
#'ppoints(7, .44)
#'# censor ar 0.5 (the lowest 3 values)
#'censpp(as.lcens(ppoints(7, .44), 0.5))
#'
#'@export
censpp <- function(x, a=.44) {
  ## Coding history:
  ##    2002Jul09 DLLorenz Based on coding supplied by David C. Leeth &  
  ##                        attributed in large part to Don MacQueen of 
  ##                        Lawrence Livermore.
  ##    2002Dec19 DLLorenz Bug fix.
  ##    2005Jun13 DLLorenz Converted input to lcens
  ##    2006Mar23 DLLorenz Removed warning about uncensored < censored
  ##    2007Jan29 DLLorenz Modified to allow infoRich coding
  ##    2007Nov14 DLLorenz Added a=alpha argument and renamed function
  ##    2007Dec21 DLLorenz Changed default a from .5 to .44
  ##    2010Jul23 DLLorenz Changed to allow negative values!
  ##                       Added some comments and collapsing of adjacent
  ##                       det. lims.
  ##    2012Mar02 DLLorenz Move to R package
  ##    2013Jan01 DLLorenz Roxygenized
  ##    2013Jan01          This version
  ##
  UseMethod("censpp")
}

#' @rdname censpp
#' @export
#' @method censpp default
censpp.default <- function(x, a=0.44) {
  if(length(x) == 1)
    return(NULL) # does not work like ppoints
  ## strip NAs
  x <- sort(x[!is.na(x)])
  return(list(x=x, pp=ppoints(x, a=a), xcen=numeric(0), ppcen=numeric(0)))
}

#' @rdname censpp
#' @export
#' @method censpp lcens
censpp.lcens <- function(x, a=0.44) {
  ## Sort the data
  x <- sort(x)
  x.ndflag <- x@censor.codes
  ## Extend vector of detection limits with -/+ infinity
  ## for use in loop that fills aj, bj, and cj below
  dl <- c(-Inf, unique(x@.Data[, 1][x.ndflag]), Inf)
  m <- length(dl)
  aj <- bj <- cj <- numeric(m)
  xunc <- x@.Data[, 1][!x.ndflag]
  xcen <- x@.Data[, 1][x.ndflag]
  ## Detections in aj and bj must be mutually exclusive
  ## Inequalities for aj based on Hirsch & Stedinger 1987;
  ## aj is the number of uncensored between det. lim.
  ## Inequalities for bj based on Helsel & Cohn 1988;
  ## bj is the total number of cen and uncen < det. lim.
  ## And cj is the number of censored = the det. lim.
  for (j in 1:m) {
    aj[j] <- sum( dl[j] <= xunc & xunc < dl[j+1] )
    bj[j] <- sum( xunc < dl[j] ) + sum( xcen <= dl[j] )
    cj[j] <- sum(xcen == dl[j])
  }
  ## Collapse adjacent det. lim. This produces more consistent pps for
  ## censored data, consistent in preserving the variance.
  if(m > 3) { # needed only if there is more than 1 det. lim.
    inew <- 1
    for(i in 2:(m-1)) {
      if(aj[i] == 0) {
        cj[i+1] <- cj[i] + cj[i+1]
      }
      else {
        inew <- inew + 1
        aj[inew] <- aj[i]
        bj[inew] <- bj[i]
        cj[inew] <- cj[i]
        dl[inew] <- dl[i]
      }
    }
    inew <- inew + 1
    aj[inew] <- aj[m]
    bj[inew] <- bj[m]
    cj[inew] <- cj[m]
    dl[inew] <- dl[m]
    ## fix the length of the overwritten vectors
    m <- inew
    length(aj) <- length(bj) <- length(cj) <- length(dl) <- m
  }
  ## Some checks
  if(sum(aj) != sum(!x.ndflag))
    warning("sum(aj) != # of hits = sum(!x.ndflag)\n  sum(aj) =",
            sum(aj), "\n  sum(!x.ndflag) =", sum(!x.ndflag), "\n")
  if(bj[m] != length(x))
    warning("bj[m] =", bj[m], "!= # of obs =", length(x),
            "\n")
  ## Vector of pr(exceed jth detection limit) j=1...m
  ## the m+1 entry defined = 0
  ## since dl[1] has been defined = 0, pe[1] can be forced = 1
  ## note: loop does not use aj[m+1] or bj[m+1] (which are extraneous at this point)
  pe <- c(1,numeric(m))
  for (j in m:2) {
    pe[j] <- pe[j+1]+(1-pe[j+1])*aj[j]/(aj[j]+bj[j])
  }
  ppunc <- xunc*0
  ppcen <- numeric(length(xcen))
  isum <- 0
  jsum <- 0  
  for (i in seq(m)) {
    if(aj[i] > 0) {
      for(j in seq(aj[i])) {
        ppt <- (j - a)/(aj[i] + 1 - 2*a)
        ppunc[isum + j] <- (1. - pe[i]) + (pe[i] - pe[i+1])*ppt
      }
      isum <- isum + aj[i]
    }
    if(cj[i] > 0) {
      for(j in seq(cj[i])) {
        ppt <- (j - a)/(cj[i] + 1 - 2*a)
        ppcen[jsum + j] <- (1 - pe[i])*ppt
      }
      jsum <- jsum + cj[i]
    }
  }
  list(x=xunc,pp=ppunc,xcen=xcen,ppcen=ppcen)
}

#' @rdname censpp
#' @export
#' @method censpp mcens
censpp.mcens <- function(x, a=0.44) {
  ## Censored plotting positions by K-M method, a is ignored
  ## CF is a censor flag-- 1 is left, 2 is interval/uncensored
  ##  3 is right
  x <- x@.Data # extract just the data
  xtarg <- (x[, 1] + x[, 2])/2
  N <- length(xtarg)
  CF <- rep(2, N)
  CF <- ifelse(x[, 1] == -Inf, 1, CF)
  CF <- ifelse(x[, 2] == Inf, 3, CF)
  xtarg <- ifelse(x[, 1] == -Inf, x[, 2], xtarg)
  xtarg <- ifelse(x[, 2] == Inf, x[, 1], xtarg)
  N <- sum(!is.na(xtarg))
  ord <- order(xtarg)
  x <- (xtarg[ord])[seq(N)]
  CF <- (CF[ord])[seq(N)]
  pp <- ppoints(N, a)
  ## First adjust <
  fct <- 1.0
  for(i in seq(N, 1, -1)) {
    if(CF[i] == 1)
      fct <- fct*i/(i-1)
    if(CF[i] != 1) # Do not change censored pps
      pp[i] <- pp[i]*fct
  }
  ## Adjust for >
  fct=1.0
  for(i in seq(1, N)) {
    if(CF[i] == 3)
      fct <- fct*(N-i+1)/(N-i)
    if(CF[i] != 3) # Do not change censored pps
      pp[i] <- 1 - (1 - pp[i])*fct
  }
  return(list(x=x[CF==2], pp=pp[CF==2], xcen=x[CF==1], ppcen=pp[CF==1],
         xrcn=x[CF==3], pprcn=pp[CF==3]))
}
