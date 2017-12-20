#' @title Gehan Test Scores
#' 
#' @description Computes Gehan scores that compare the values in one
#'group to the values in another group.
#'
#' @details If \code{y} is missing, then \code{x} is compared to itself 
#'(with \code{NA}s removed) and the output is the u-score described in Helsel
#'(2012).
#' 
#' @param x any object that can be converted to class "mcens."
#'Missing values are permitted and result in corresponding 
#'missing values in the output.
#' @param y any object that can be converted to class "mcens."
#'Missing values are permitted but result in 
#'missing values in the output unless \code{na.rm} is 
#'\code{TRUE}. See \bold{Details}.
#' @param na.rm remove missing values from \code{y} before
#'comparing to \code{x}?
#' @return A numeric vector that is the result of the comparison
#'of each value in \code{x} to all values in \code{y}. It is the
#'equivalent of the "h" score in described by Helsel, but the 
#'signs are reversed so that larger values in \code{x} have
#'larger values in the output.
#' @note If the output is the u-score, \code{y} is missing, then the
#'values can be converted to ranks by dividing by 2 and adding the mean
#'rank, (length(\code{x}) + 1)/2.
#' @seealso \code{\link{survdiff}}, \code{\link{survfit}},
#'\code{\link{lcens-class}}
#' @references Gehan, E.A., 1965, A generalized Wilcoxon test for comparing
#'arbitraritly singly censored samples: Biometrika, v. 52, p. 203-223.\cr
#'
#'Helsel, D.R. 2012, Statistics for Censored Environmental Data Using Minitab
#'and R: New York, Wiley, 324 p.\cr
#'
#' @export
gehanScores <- function(x, y, na.rm=FALSE) {
  ## Coding history:
  ##    2014Jan02 DLLorenz Initial Coding.
  ##    2014Oct07 DLLorenz debugging for NAs
	##
	x <- na.exclude(x)
	xomit <- attr(x, "na.action")
  xm <- as.mcens(x)
  x <- xm@.Data
  # Adjust less-thans and greater-thans
  xv <- sort(unique(as.vector(x)))
  xv <- xv[is.finite(xv)]
  adjust <- min(diff(xv))/10
  x[xm@censor.codes == -1L, 2L] <- x[xm@censor.codes == -1L, 2L] - adjust
  x[xm@censor.codes == 1L, 1L] <- x[xm@censor.codes == 1L, 1L] + adjust
  N <- nrow(x)
  # Check y
  if(missing(y)) {
    y <- x
    M <- N
  } else { # Repeat processing for y
  	# Remove missings from y if indicated
  	if(na.rm) {
  		y <- y[!is.na(y)]
  	}
    ym <- as.mcens(y)
    y <- ym@.Data
    yv <- sort(unique(as.vector(y)))
    yv <- yv[is.finite(yv)]
    adjust <- min(diff(yv))/10
    y[ym@censor.codes == -1L, 2L] <- y[ym@censor.codes == -1L, 2L] - adjust
    y[ym@censor.codes == 1L, 1L] <- y[ym@censor.codes == 1L, 1L] + adjust
    M <- nrow(y)
  }

  # Check for y all missing
  if(M == 0L)
    return(rep(NA_real_, N))
  # Compute the number strictly less than or strictly greater than each value
  L <- rowSums(x[, 1L] > matrix(y[, 2L], nrow=N, ncol=M, byrow=TRUE))
  G <- rowSums(x[, 2L] < matrix(y[, 1L], nrow=N, ncol=M, byrow=TRUE))
  return(naresid(xomit, L - G))
}
