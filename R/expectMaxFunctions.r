#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#  function:      Expectation substitution functions
#
#  programmer:    Elgin S. Perry, Ph. D.
#
#  date:          3/2/2016, revised 8/23/2016 for include normal expectation
#
#  address:       377 Resolutions Rd.
#                 Colonial Beach, Va.  22443
#
#  voice phone:   (410)610-1473
#  email:         eperry@chesapeake.net
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# 
# 01Jun2018: JBH: added "df$" to mu to create df$mu in lognormal functions
# 23May2018: JBH: added "row.names = NULL"  to df <- data.frame(l=l, u=u, mu=mu)
#                 to avoid warning message of row names were found from a short 
#                 variable and have been discarded

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# LogNormal Case Functions ####

#' @title Expectation maximization function: Log-normal case, Cens
#'
#' @param df data frame
#' @param dep dependent variable
#' @param mu predicted values from mgcv::gam
#' @param sigma model standard deviation
#' @keywords internal
#' @export
.ExpLNmCens <- function(df,dep,mu,sigma)   {
  # computes expected value within a censoring window, for log-normal distribution
  # using an mCens object.
  ec <- .ExpLNlCens(df[,dep]@.Data[,1],df[,dep]@.Data[,2],mu,sigma)
  ec <- .ExpLNrCens(ec$l,ec$u,mu,sigma)
  ec <- .ExpLNiCens(ec$l,ec$u,mu,sigma)
  ExpLNmCens.return <- ec
}
#' @title Expectation maximization function: Log-normal case, left censured
#' @param l l
#' @param u u
#' @param mu predicted values from mgcv::gam
#' @param sigma model standard deviation
#' @param lCens default=NA
#' @keywords internal
.ExpLNlCens <- function(l,u,mu,sigma,lCens=NA) {
  # computes expected value for left censored interval for log-normal
  df <- data.frame(l=l, u=u, mu=mu, row.names = NULL)

  if(is.na(lCens[1])) {
    df$lcens <- ((df$l==0) & is.finite(df$u))
  } else {
    df$lcens <- lCens
  }

  if (any(df$lcens)) {
    df$zu[df$lcens] <- (log(df$u[df$lcens]) - df$mu[df$lcens])/sigma # 01Jun2018
    df$ec[df$lcens] <- exp(df$mu[df$lcens] + sigma**2/2)* stats::pnorm(df$zu[df$lcens]-sigma) / stats::pnorm(df$zu[df$lcens])
    df$l[df$lcens] <- df$ec[df$lcens]
    df$u[df$lcens] <- df$ec[df$lcens]
  }
  ExpLNlCens.return <- df[,c('l','u')]
}
#' @title Expectation maximization function: Log-normal case, right censured
#' @param l l
#' @param u u
#' @param mu predicted values from mgcv::gam
#' @param sigma model standard deviation
#' @param rCens default=NA
#' @keywords internal
.ExpLNrCens <- function(l,u,mu,sigma,rCens=NA) {
  # computes expected value for right censored for log-normal
  df <- data.frame(l=l, u=u, mu=mu, row.names = NULL)

  if(is.na(rCens[1])) {
    df$rcens <- (is.finite(df$l) & is.infinite(df$u))
  } else {
    df$rcens <- rCens
  }

  if (any(df$rcens))  {
    df$zl[df$rcens] <- (log(df$l[df$rcens]) - df$mu[df$rcens])/sigma # 01Jun2018
    df$ec[df$rcens] <- exp(df$mu[df$rcens] + sigma**2/2)* (1-stats::pnorm(df$zl[df$rcens]-sigma)) / (1-stats::pnorm(df$zl[df$rcens]))
    df$l[df$rcens] <- df$ec[df$rcens]
    df$u[df$rcens] <- df$ec[df$rcens]
  }
  ExpLNrcens.return <- df[,c('l','u')]
}
#' @title Expectation maximization function: Log-normal case, i censured
#' @param l l
#' @param u u
#' @param mu predicted values from mgcv::gam
#' @param sigma model standard deviation
#' @param iCens default=NA
#' @keywords internal
.ExpLNiCens <- function(l,u,mu,sigma,iCens=NA) {
  # computes expected value for interval censored for log-normal
  df <- data.frame(l=l, u=u, mu=mu, row.names = NULL)

  if(is.na(iCens[1])) {
    df$icens <- (is.finite(df$l) & is.finite(df$u) & !(df$l==df$u))
  } else {
    df$icens <- iCens
  }

  if (any(df$icens)) {
    df$zl[df$icens] <- (log(df$l[df$icens]) - df$mu[df$icens])/sigma # 01Jun2018
    df$zu[df$icens] <- (log(df$u[df$icens]) - df$mu[df$icens])/sigma # 01Jun2018
    df$ec[df$icens] <- exp(df$mu[df$icens] + sigma**2/2)*
      (stats::pnorm(df$zu[df$icens]-sigma)-stats::pnorm(df$zl[df$icens]-sigma)) /
      (stats::pnorm(df$zu[df$icens])-stats::pnorm(df$zl[df$icens]))
    df$l[df$icens] <- df$ec[df$icens]
    df$u[df$icens] <- df$ec[df$icens]
  }
  ExpLNicens.return <- df[,c('l','u')]
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Normal Case Functions ####

#' @title Expectation maximization function: Normal case
#'
#' @param df data frame
#' @param dep dependent variable
#' @param mu predicted values from mgcv::gam
#' @param sigma model standard deviation
#' @keywords internal
#' @export
.ExpNmCens <- function(df,dep,mu,sigma)   {
  # computes expected value within a censoring window, for normal distribution
  # using an mCens object.
  #  df <- tdf; dep <- 'tdep'; mu <- tdf$mu;
  ec <- .ExpNlCens(df[,dep]@.Data[,1],df[,dep]@.Data[,2],mu,sigma)
  ec <- .ExpNrCens(ec$l,ec$u,mu,sigma)
  ec <- .ExpNiCens(ec$l,ec$u,mu,sigma)
  ExpNmCens.return <- ec
}
#' @title Expectation maximization function: Normal case, left censured
#' @param l l
#' @param u u
#' @param mu predicted values from mgcv::gam
#' @param sigma model standard deviation
#' @param lCens default=NA
#' @keywords internal
.ExpNlCens <- function(l,u,mu,sigma,lCens=NA) {
  # computes expected value for left censored interval for normal
  df <- data.frame(l=l, u=u, mu=mu, row.names = NULL)

  if(is.na(lCens[1])) {
    df$lcens <- ((df$l==-Inf) & is.finite(df$u))
  } else {
    df$lcens <- lCens
  }

  if (any(df$lcens)) {
    df$zu[df$lcens] <- (df$u[df$lcens] - df$mu[df$lcens])/sigma
    df$ec[df$lcens] <- df$mu[df$lcens] - sigma*(stats::dnorm(df$zu[df$lcens])/stats::pnorm(df$zu[df$lcens]))
    df$l[df$lcens] <- df$ec[df$lcens]
    df$u[df$lcens] <- df$ec[df$lcens]
  }
  ExpNlCens.return <- df[,c('l','u')]
}
#' @title Expectation maximization function: Normal case, right censured
#' @param l l
#' @param u u
#' @param mu predicted values from mgcv::gam
#' @param sigma model standard deviation
#' @param rCens default=NA
#' @keywords internal
.ExpNrCens <- function(l,u,mu,sigma,rCens=NA) {
  # computes expected value for right censored for normal
  df <- data.frame(l=l, u=u, mu=mu, row.names = NULL)

  if(is.na(rCens[1])) {
    df$rcens <- (is.finite(df$l) & is.infinite(df$u))
  } else {
    df$rcens <- rCens
  }

  if (any(df$rcens))  {
    df$zl[df$rcens] <- (df$l[df$rcens] - df$mu[df$rcens])/sigma
    df$ec[df$rcens] <- df$mu[df$rcens] + sigma*(stats::dnorm(df$zl[df$rcens])/(1-stats::pnorm(df$zl[df$rcens])))
    df$l[df$rcens] <- df$ec[df$rcens]
    df$u[df$rcens] <- df$ec[df$rcens]
  }
  ExpNrcens.return <- df[,c('l','u')]
}
#' @title Expectation maximization function: Normal case, i censured
#' @param l l
#' @param u u
#' @param mu predicted values from mgcv::gam
#' @param sigma model standard deviation
#' @param iCens default=NA
#' @keywords internal
.ExpNiCens <- function(l,u,mu,sigma,iCens=NA) {
  # computes expected value for interval censored for log-normal
  df <- data.frame(l=l, u=u, mu=mu, row.names = NULL)
  if(is.na(iCens[1])) {
    df$icens <- (is.finite(df$l) & is.finite(df$u) & !(df$l==df$u))
  } else {
    df$icens <- iCens
  }

  if (any(df$icens)) {
    df$zl[df$icens] <- (df$l[df$icens] - df$mu[df$icens])/sigma
    df$zu[df$icens] <- (df$u[df$icens] - df$mu[df$icens])/sigma
    df$ec[df$icens] <- df$mu[df$icens] - sigma*((stats::dnorm(df$zu[df$icens])- stats::dnorm(df$zl[df$icens]))/
                       (stats::pnorm(df$zu[df$icens])-stats::pnorm(df$zl[df$icens])))
    df$l[df$icens] <- df$ec[df$icens]
    df$u[df$icens] <- df$ec[df$icens]
  }

  ExpNicens.return <- df[,c('l','u')]
}


# tdf <- data.frame(mu = rep(5,6),
#                   l = c(-Inf,-Inf,8,9,1,8),
#                   u = c(1,2,Inf,Inf,2,9) )
# tdf$tdep <- cbind(tdf$l,tdf$u)
# sigma <- 1
# tl <- .ExpNlCens(tdf$l,tdf$u,tdf$mu,sigma); cbind(tdf,tl)
# tr <- .ExpNrCens(tdf$l,tdf$u,tdf$mu,sigma); cbind(tdf,tr)
# ti <- .ExpNiCens(tdf$l,tdf$u,tdf$mu,sigma); cbind(tdf,ti)
# tm <- .ExpNmCens(tdf,'tdep',tdf$mu,sigma); cbind(tdf,tm)
#   mu    l   u tdep.1 tdep.2         l         u
# 1  5 -Inf   1   -Inf      1 0.7743929 0.7743929
# 2  5 -Inf   2   -Inf      2 1.7169013 1.7169013
# 3  5    8 Inf      8    Inf 8.2830987 8.2830987
# 4  5    9 Inf      9    Inf 9.2256071 9.2256071
# 5  5    1   2      1      2 1.7395457 1.7395457
# 6  5    8   9      8      9 8.2604543 8.2604543
# (.ExpNmCens(tdf,'tdep',tdf$mu,sigma))
#
#
# tdf <- data.frame(mu = rep(5,6),
#                   l = c(0,0,8,9,1,8),
#                   u = c(10,20,Inf,Inf,2,9))
# tdf$tdep <- cbind(tdf$l,tdf$u)
# sigma <- 1
# (.ExpLNmCens(tdf,'tdep',tdf$mu,sigma))
#            l          u
# 1   7.626690   7.626690
# 2  14.462724  14.462724
# 3 245.109335 245.109335
# 4 245.295821 245.295821
# 5   1.677003   1.677003
# 6   8.518219   8.518219

