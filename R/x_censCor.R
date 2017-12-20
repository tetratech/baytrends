#' @title Correlation
#'
#' @description Computes the maximum likelihood estimate of the correlation between 
#'two possibly left-censored vectors. It is equivalent the the Pearson 
#'product-moment correlation for uncensored data.
#'
#' @details
#'\code{Full} may be either logical or a numeric vector. If \code{Full} is \code{TRUE},
#'then estimate the means and standard deviations for \code{x} and \code{y}. 
#'If \code{Full} is \code{FALSE}, use the initial maximum likelihood estimate for those
#'statistics. Otherwise \code{Full} can be a named vector containing \code{mnx}, the mean
#'for \code{x}; \code{sdx}, the standard deviation for \code{x}; \code{mny}, the mean
#'for \code{y}; \code{sdy}, the standard deviation for \code{y}. \code{Full} can be set
#'to \code{FALSE} if the optimization fails at large censoring levels or to improve
#'processing speed for large sample sizes.
#'
#' @importFrom survival survreg Surv
#' @importFrom mvtnorm dmvnorm pmvnorm
#' @param x any data that can be converted to a left-censored data object.
#' @param y any data that can be converted to a left-censored data object.
#' @param Full how to compute  the mean and standard deviation of \code{x} and \code{y}.
#'See \bold{Details}.
#' @param na.rm logical, remove missing values before computing the correlation?
#' @return A vector with these names:
#'\item{cor}{ the correlation between \code{x} and \code{y}.}
#'\item{mnx}{ the mean of \code{x}.}
#'\item{sdx}{ the standard deviation of \code{x}.}
#'\item{mny}{ the mean of \code{y}.}
#'\item{sdy}{ the standard deviation of \code{y}.}
#'\item{cx}{ the proportion of censored values of \code{x}.}
#'\item{cy}{ the proportion of censored values of \code{y}.}
#'\item{cxy}{ the proportion of censored values common to \code{x} and \code{y}.}
#'\item{n}{ the number of observations.}
#'\item{ll0}{ the log likelihood for cor=0}
#'\item{llcor}{ the log likelihood for cor=cor}
#' @references Lyles, R.H., Williams, J.K., and Chuachoowong R., 2001, Correlating two viral 
#'load assays with known detection limits: Biometrics,  v. 57 no. 4, p. 1238--1244.
#' @keywords summary censored
#' @examples
#'# Simple no censoring
#'set.seed(450)
#'tmp.X <- rnorm(25)
#'tmp.Y <- tmp.X/2 + rnorm(25)
#'cor(tmp.X, tmp.Y)
#'censCor(tmp.X, tmp.Y)
#'# Some censoring
#'censCor(as.lcens(tmp.X, -1), as.lcens(tmp.Y, -1))
#'
#' @export
censCor <- function(x, y, Full=TRUE, na.rm=TRUE) {
  ## Coding history:
  ##   2008Mar?? DLLorenz Initial version, based loosely on code from Lisa Newton
  ##   2008Dec31 DLLorenz Coded for inclusion in library
  ##   2009Mar17 DLLorenz Begin tweaks
  ##   2009Oct14 DLLorenz Use link functions for sd and rho
  ##   2013Feb22 DLLorenz Conversion to R
  ##
  ## Use link functions for sd and rho:
  ##  sd = exp(gam); gam = log(sd)
  ##  rho = (exp(del) - 1)/(exp(del) + 1);
  ##    del = log((rho + 1)/(1 - rho))
  ## Estimate means, standard deviations and correlation via mle,
  ## cor can be estimated using marginal means and sds
  ##
  logprobcor <- function(pars, x, y, cx, cy, pfixed) {
    ## pars is the parameter(s) to be estimated
    ## pfixed are the fixed parameters
    ## Code based on Lyles and others, Biometrics 2001 but data
    ## not sorted by type
    pars <- c(pars, pfixed)
    rho <- (exp(pars[1]) - 1)/(exp(pars[1]) + 1)
    mnx <- pars[2]
    mny <- pars[3]
    sdx <- exp(pars[4])
    sdy <- exp(pars[5])
    mny.x <- mny + ((rho * sdy)/sdx) * (x - mnx) # expected value of y given x
    mnx.y <- mnx + ((rho * sdx)/sdy) * (y - mny)
    sdy.x <- sdy*sqrt(1 - rho^2) # expected value of sdy given x
    sdx.y <- sdx*sqrt(1 - rho^2)
    xy <- cbind(x,y)
    ## Type 1 both uncensored, do not assume that there are always some of these
    sel <- (cx == 0 & cy == 0)
    if(any(sel)) {
    cov <- rho * sqrt(sdx^2 * sdy^2)
    sigma <- matrix(c(sdx^2, cov, cov, sdy^2), ncol=2)
    retval <- sum(-log(dmvnorm(xy[sel,,drop=FALSE], mean=c(mnx, mny), sigma=sigma)))
    } else {
    	cov <- rho * sqrt(sdx^2 * sdy^2)
    	sigma <- matrix(c(sdx^2, cov, cov, sdy^2), ncol=2)
    	retval <- 0
    }
    ## Type 2 only y censored
    sel <- (cx == 0 & cy == 1)
    if(any(sel))
      retval <- retval + sum(-log(dnorm(x[sel], mean=mnx, sd=sdx))
                             -log(pnorm(y[sel], mean=mny.x[sel], sd=sdy.x)))
    ## Type 3 only x censored
    sel <- (cx == 1 & cy == 0)
    if(any(sel))
      retval <-  retval + sum(-log(dnorm(y[sel], mean=mny, sd=sdy))
                              -log(pnorm(x[sel], mean=mnx.y[sel], sd=sdx.y)))
    ## Type 4 both x and y censored
    sel <- (cx == 1 & cy == 1)
    if(any(sel)) {
      tmp <- apply(xy[sel,, drop=FALSE], 1, function(x, mn, s) pmvnorm(upper=x, mean=mn, sigma=s),
                   mn =  c(mnx, mny), s=sigma)
      retval <- retval + sum(-log(tmp))
    }
    return(retval)
  } # end of logprobcor
  ## Force lcens objects
  x <- as.lcens(x)
  y <- as.lcens(y)
  OKs <- !is.na(x) & !is.na(y)
  N <- length(OKs)
  if(sum(OKs) < 3 || (sum(OKs) < N && !na.rm)) # return NA with N
    return(c(cor=NA, mnx=NA, mny=NA, sdx=NA, sdy=NA, cx=NA, cy=NA, cxy=NA,
             n=sum(OKs)))
  cx <- x@censor.codes[OKs]
  cy <- y@censor.codes[OKs]
  x <- x@.Data[OKs, 1L]
  y <- y@.Data[OKs, 1L]
  N <- length(x)
  ## Use survreg to estimate mean and sd if not provided
  if(is.numeric(Full)) {
    msx <- Full[c("mnx", "sdx")]
    msy <- Full[c("mny", "sdy")]
    Full <- FALSE
  }
  else {
    retval <- survreg(Surv(x, 1-cx, type="left") ~ 1, dist="gaus")
    msx <- c(retval$coef, retval$scale)
    retval <- survreg(Surv(y, 1-cy, type="left") ~ 1, dist="gaus")
    msy <- c(retval$coef, retval$scale)
  }
  ## Start with naive correlation and compute link fcn
  corn <- cor(x, y)
  del <- log((corn+1)/(1-corn))
  if(Full) {
    pars <- c(del, msx[1], msy[1], log(msx[2]), log(msy[2]))
    pfixed <- NULL
  }
  else {
    pars <- del
    pfixed <- c(msx[1], msy[1], log(msx[2]), log(msy[2]))
  }
  ## Compute the log-likelihood at cor=0
  par0 <- pars
  par0[1L] <- 0
  ll0 <- -logprobcor(par0, x, y, cx, cy, pfixed)
  # The BFGS method gives a better estimate than the default, but will fail occasionally
  retval <- tryCatch(optim(pars, logprobcor, method="BFGS",
  												 x=x, y=y, cx=cx, cy=cy, pfixed=pfixed),
  									 error=function(e) optim(pars, logprobcor,
  									 												x=x, y=y, cx=cx, cy=cy, pfixed=pfixed))
  ## Compute the log-likelihood at cor=cor
  pars <- retval$par
  llcor <- -logprobcor(pars, x, y, cx, cy, pfixed)
  retval <- c(retval$par, pfixed, sum(cx)/N, sum(cy)/N,
              sum(cx*cy)/N, N)
  ## Back transform
  retval[1] <- (exp(retval[1]) - 1)/(exp(retval[1]) + 1)
  retval[4] <- exp(retval[4])
  retval[5] <- exp(retval[5])
  names(retval) <- c("cor", "mnx", "mny", "sdx", "sdy", "cx", "cy", "cxy", "n")
  return(c(retval, ll0=ll0, llcor=llcor))
}
