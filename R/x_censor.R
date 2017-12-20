#' @title Multiply-censored Data Conversion
#'
#' @description Censors a vector at user-set limits for left- or right-censoring.
#'
#' @include as.lcens.R as.mcens.R
#' @param x a numeric vector. Missing values are permitted.
#' @param left a vector of left-censoring thresholds. If the length is greater
#'than 1, then they are replicated to the length of \code{x}.
#' @param right a vector of right-censoring thresholds. If the length is greater
#'than 1, then they are replicated to the length of \code{x}.
#' @return An S4 object of class "lcens" if \code{right} is \code{Inf}, otherwise
#'an S4 object of class "mcens."
#' @note Missing velues are generated wherever \code{right} is less than or equal
#'to \code{left}.\cr
#'The \code{censor} function is provided as a simple function to create either left-
#'or multiply-censored data from numeric values, primarily for simulation pruposes.
#'The functions \code{as.lcens} or \code{as.mcens} offer greater flexibility and 
#'should be used for real data.
#' @references Lorenz, D.L., in preparation
#' @seealso \code{\link{as.lcens}}, \code{\link{as.mcens}}
#' @export
#' @keywords methods manip
#' @examples
#'## Create a random vector
#'set.seed(651)
#'x.tmp <- rnorm(5)
#'censor(x.tmp, 0, .75)
censor <- function(x, left=-Inf, right=Inf) {
	if(right == Inf)
		return(as.lcens(x, left))
  left <- rep(left, length.out=length(x))
  xl <- ifelse(x < left, -Inf, x)
  xr <- ifelse(x < left, left, x)
  right <- rep(right, lenght.out=length(x))
  xr <- ifelse(x > right, Inf, xr)
  xl <- ifelse(x > right, right, xl)
	Invalids <- right <= left
	xl[Invalids] <- NA
	xr[Invalids] <- NA
  return(as.mcens(xl, xr))
}