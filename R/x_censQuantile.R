#'Sample Quantiles
#'
#'Computes sample or estimated quantiles corresponding to the given
#'probabilities.  The smallest observation (censored or uncensored) corresponds
#'to a probability of 0 and the largest to a probability of 1.
#'
#'The methods available in the current version are "flipped K-M," "log ROS,"
#'"ROS," "log MLE," and "MLE." The method "flipped K-M" produces quantiles
#'using the Kaplan-Meier method on fliiped data described by Helsel (2012). The
#'methods "log ROS" and "log MLE" are described by Helsel, 2012 and Helsel and
#'Cohn (1988).  The methods "ROS" and "MLE" are similar to "log ROS" and "log
#'MLE" except that no log- and back-transforms are made on the data.
#'
#'@rdname censQuantile
#'@aliases censQuantile censQuantile.default censQuantile.lcens
#'censQuantile.mcens censQuantile.qw
#'@param x an object of a censored-data class whose sample quantiles are
#'wanted.
#'@param probs numeric vector of probabilities with values in [0,1].
#'@param na.rm logical; if \code{TRUE}, any \code{NA} and \code{NaN}s are
#'removed from \code{x} before the quantiles are computed.
#'@param method the method to use for computing quantiles. See \bold{Details}.
#'@param type an integer between 1 and 9 selecting one of the nine quantile
#'algorithms detailed below to be used. See \code{\link{quantile}} for a
#'description of the types.
#'@param alpha the offset fraction to be used, depending on \code{method};
#'typically in [0,1].
#'@return An object of class "censQuantile" with one named component for each
#'level requested in \code{probs}. The values returned for 
#'\code{method}="flipped K-M" can be of class "lcens."
#'@seealso \code{\link{quantile}}, \code{\link{quantile.lcens}}
#'@references Helsel, D.R. 2012, Statistics for censored environmental data 
#'using Minitab and R: New York, Wiley, 324 p.\cr
#'Helsel, D.R. and Cohn, T.A., 1988, Estimation of descriptive statistics for
#'multiply censored water quality data: Water Resources Research v. 24, n.
#'12, pp. 1997-2004
#'@keywords univar
#'@examples
#'
#'set.seed(28)
#'Xu <- rnorm(23)
#'censQuantile(Xu) # Uncensored (true) quartiles
#'censQuantile(as.lcens(Xu, 0)) # How'd ROS do?
#'censQuantile(as.lcens(Xu, 0), method="MLE") # How'd ROS do?
#'
#'@export
censQuantile <- function(x, probs=seq(0, 1, 0.25), na.rm=FALSE, method="ROS",
                         type=2, alpha=0.4) {
  ##    2012Mar29 DLLorenz Original Coding 
  ##    2012Dec28 DLLorenz Roxygenized
  ##    2013Jan05 DLLorenz Return object of class "censQuantile"
  ##    2013Jan22 DLLorenz Added mcens method
  ##
  ## In contrast to quantile, these functions return NAs if necesssary
  UseMethod("censQuantile")
}

#' @export
#'@method censQuantile default
#'@rdname censQuantile
censQuantile.default <- function(x, probs=seq(0, 1, 0.25), na.rm=FALSE,
                                 method="", type=2, alpha=0.4) {
  ## Treat x as numeric
  tags <- paste(format(100 * probs, trim = TRUE, digits = 3), "%", sep="")
  if(method != "")
    warning("method ignored")
  if(!na.rm && any(is.na(x))) {
    retval <- as.list(probs*NA)
    names(retval) <- tags
    class(retval) <- "censQuantile"
    return(retval)
  }
  retval <- as.list(quantile(x, probs=probs, type=type, na.rm=na.rm))
  class(retval) <- "censQuantile"
  return(retval)
}

#' @export
#'@method censQuantile lcens
#'@rdname censQuantile
censQuantile.lcens <- function(x, probs=seq(0, 1, 0.25), na.rm=FALSE,
                               method="ROS", type=2, alpha=0.4) {
  tags <- paste(format(100 * probs, trim = TRUE, digits = 3), "%", sep="")
  if(!na.rm && any(is.na(x))) {
    retval <- as.list(probs*NA)
    names(retval) <- tags
    class(retval) <- "censQuantile"
    return(retval)
  }
  retval <- as.list(quantile(x, probs=probs, method=method, type=type, 
  													 alpha=alpha, na.rm=na.rm))
  ## The names of retval indicate whether the results is left-censored or not
  nrvl <- names(retval)
  censored <- substring(nrvl, nchar(nrvl)) == "*"
  for(i in which(censored)) {
    retval[[i]] <- as.lcens(retval[[i]], retval[[i]], censor.codes=TRUE)
    names(retval)[i] <- substring(nrvl[i], 1, nchar(nrvl[i]) - 1)
  }
  class(retval) <- "censQuantile"
  return(retval)
}

#' @export
#'@method censQuantile mcens
#'@rdname censQuantile
censQuantile.mcens <- function(x, probs=seq(0, 1, 0.25), na.rm=FALSE,
                               method="flipped K-M", type=2, alpha=0.4) {
  tags <- paste(format(100 * probs, trim = TRUE, digits = 3), "%", sep="")
  if(!na.rm && any(is.na(x))) {
    retval <- as.list(probs*NA)
    names(retval) <- tags
    class(retval) <- "censQuantile"
    return(retval)
  }
  retval <- as.list(quantile(x, probs=probs, method=method, type=type, 
  													 alpha=alpha, na.rm=na.rm))
  ## The names of retval indicate whether the results is left-censored or not
  nrvl <- names(retval)
  censored <- substring(nrvl, nchar(nrvl)) == "*" # left
  for(i in which(censored)) {
    retval[[i]] <- as.lcens(retval[[i]], retval[[i]], censor.codes=TRUE)
    names(retval)[i] <- substring(nrvl[i], 1, nchar(nrvl[i]) - 1)
  }
  censored <- substring(nrvl, nchar(nrvl)) == "!" # right
  for(i in which(censored)) {
    retval[[i]] <- as.mcens(retval[[i]], NA_real_)
    names(retval)[i] <- substring(nrvl[i], 1, nchar(nrvl[i]) - 1)
  }
  class(retval) <- "censQuantile"
  return(retval)
}

#' @export
#'@method censQuantile qw
#'@rdname censQuantile
censQuantile.qw <- function(x, probs=seq(0, 1, 0.25), na.rm=FALSE,
                               method="log MLE", type=2, alpha=0.4) {
  ## Convert to appropriate type and use that method
  Cens <- censoring(x)
  if(Cens == "none")
    return(censQuantile.default(as.numeric(x), probs=probs, na.rm=na.rm,
           type=type)) # method ignored
  if(Cens == "left")
    return(censQuantile.lcens(as.lcens(x), probs=probs, na.rm=na.rm,
           type=type, method=method, alpha=alpha))
  ## Else
  return(censQuantile.mcens(as.mcens(x), probs=probs, na.rm=na.rm,
           type=type, method=method, alpha=alpha))
}
