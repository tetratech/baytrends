#'Estimate Statistics
#'
#'Support function for computing statistics for left- and multiply censored 
#'data.
#'
#'@param x out from mdlKM or mcenKM
#'@param probs numeric vector of desrired probabilities
#'@return A list containing summary statistics.
#'@keywords internal misc
#'@export
mdlKMstats <- function(x, probs=c(.25, .5, .75)) {
  ## Coding history:
  ##    2005Jul14 DLLorenz Original dated version based on survmean()
  ##    2008Mar25 DLLorenz Fixed ucl-lcl limits
  ##    2008Jun04 DLLorenz Fixed lcl limit
  ##    2010Feb18 DLLorenz Modified to preserve mean and stdev
  ##    2012Mar08 DLLorenz Conversion to R with change in purpose to support
  ##                         function removed computation of u/lcls
  ##    2013Jan05 DLLorenz Roxygenized
  ##    2013Jan22 DLLorenz Extended to multiply-censored data
  ##
  ## The function below is called once for each line of output, 
  ##  i.e., once per curve.  It creates the line of output
  ##
  ## if any probs == 0, then return the "correct" percentage, and
  ## the minimum 
  pfun <- function(nused, time, surv, n.risk, n.event, probs, type) {
    ## Compute the mean of the curve, with "start.time" as 0
    ##   start by drawing rectangles under the curve a reative frequency histogram with unequal bin widths
    start.time <- min(time)
    n <- length(time)
    delta <- diff(c(start.time, time))
    ## Width of rectangles gives the mean
    rectangles <- delta * c(1, surv[ - n])
    mean <- -(sum(rectangles) + start.time)
    ## Set bias to NA if type is "left" (right-censored unflipped data)
    if(type == "left")
      bias <- biasadj <- NA
    else {
      bias <- surv[n] * (0 - time[n])
      biasadj <- ifelse(bias == 0, 0, 0 - time[n])
    }
    ## Compute variance
    hh = c(ifelse((n.risk[-n] - n.event[-n]) == 0, 0, 
    							n.event[-n]/(n.risk[-n] * (n.risk[-n] - n.event[-n]))), 0) * nused
    dif.time = c(diff(c(start.time, time)), 0)
    Var = sum(rev(cumsum(rev(dif.time * c(1, surv)))^2)[-1] * hh)
    events <- sum(n.event)
    # bias adjustment
    Var <- Var * (events/(events - 1))
    ## Compute the quantiles 
    minmin <- function(y, xx, prob) {
      prob <- round(prob,8)
      if(prob < min(y))
        return(xx[length(xx)])
      y <- round(y, 8)
      if(any(y == prob)) {
        if(any(y < prob))
          0.5 * (min(xx[y == prob]) + min(xx[y < prob]))
        else
          0.5 * (min(xx[y == prob]) + max(xx[y == prob]))
      }
      else
        min(xx[y <= prob])
    } # end of minmin
    qtiles <- -sapply(as.list(probs), minmin, y=surv, xx=time)
    names(qtiles) <- paste(100*round(probs,4), "%", sep="")
    names(nused) <- "N"
    obs <- c(n=nused, events=events)
    mean <- c(mean, bias, biasadj)
    names(mean) <- c("mean", "Max bias", "Bias base")
    n.censored <- n.risk - n.event - c(n.risk[-1], 0)
    censored <- which(n.censored > 0)
    n.censored <- n.censored[censored]
    names(n.censored) <- format((-time)[censored])
    return(list(obs=obs, censoring=n.censored, censored=min(surv),
                mean=mean, stdev=sqrt(Var), qtiles=qtiles))
  } # end of pfun
  probs <- sort(probs)
  stime <- x$time
  surv <- x$surv
  ## Four cases: strata Y/N  by  ncol(surv)>1 Y/N
  ##  Repeat the code, with minor variations, for each one
  out <- list()
  if(is.null(x$strata)) {
    if(is.matrix(surv)) {
      for(i in 1:ncol(surv)) {
        out[[i]] <- pfun(x$n, stime, surv[, i], x$n.risk,
                         x$n.event, probs, x$type)
      }
    }
    else {
      out[[1]] <- pfun(x$n, stime, surv, x$n.risk, x$n.event, probs, x$type)
    }
  }
  else {
    ## strata case
    nstrat <- length(x$strata)
    stemp <- rep(1:nstrat, x$strata)
    ## the index vector for strata1, 2, etc
    if(is.matrix(surv)) {
      ns <- ncol(surv)
      k <- 0
      for(i in 1:nstrat) {
        who <- (stemp == i)
        for(j in 1:ns) {
          k <- k + 1
          out[[k]] <- pfun(x$n[i], stime[who], surv[who, j], x$n.risk[who],
                           x$n.event[who], probs, x$type)
        }
      }
    }
    else {
      strata <- names(x$strata)
      for(i in 1:nstrat) {
        who <- (stemp == i)
        out[[strata[i]]] <- pfun(x$n[i], stime[who], surv[who],
                                 x$n.risk[who], x$n.event[who], probs, x$type)
      }
    }
  }
  out
}
