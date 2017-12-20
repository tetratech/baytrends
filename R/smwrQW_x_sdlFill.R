#'Estimate Statistics
#'
#'Support function for computing statistics for left-censored data.
#'
#'@param x the data to estimate, Missing values permitted and ignored.
#'Must be an object of class "lcens," a numeric vector, or the output from censpp.
#'@param method the method to use, either "fill" or "log fill"
#'@param alpha the offset for plotting postion (not used, required for other methods).
#'@return A list containing the mean and standard deviation, filled in
#'values for the censored values, and the maximum censor level.
#'@note This function uses the method of Gleit (1985). That method computes
#'the mean and standard deviation of the data, estimates synthetic values for
#'the censored values using a method similar to ROS, and repeats the process until the 
#'mean and standard deviation stabilize.
#'@references Gleit, A., 1985, Estimation for small normal data sets with detection limits:
#'Environmental Science and Technology, v. 19, no. 12, p. 1201--1206.
#'@keywords misc
#'@export
sdlFill <- function(x, method="fill", alpha=0.4) {
  ## Coding history:
  ##   2012Oct05 DLLorenz Original Coding
  ##   2012Oct05          This version.
  method <- match.arg(method, c("fill", "log fill"))
  if(class(x) != "list")
    x <- censpp(x, a=alpha)
  if(length(unique(x$xcen)) > 1)
    stop("sdlFill only valid for single detection limit")
  ## Convert the pps to normal quantiles
  x$pp <- qnorm(x$pp)
  x$ppcen <- qnorm(x$ppcen)
  cenlev <- x$xcen
  if(method == "log fill") {
    x$x <- log(x$x)
    x$xcen <- log(x$xcen)
  }
  ## Initial estimate of the mean and sd
  coefold <- lsfit(x$pp, x$x)$coefficients
  ## Estimate the expected values
  x$xcen <- x$ppcen *  coefold[2L] + coefold[1L]
  coefnew <- c(mean(c(x$x, x$xcen)), sd(c(x$x, x$xcen)))
  i <- 0L
  while((sum((coefold - coefnew)^2) > 1.e-6) && i < 100L) {
    coefold <- coefnew
    x$xcen <- x$ppcen *  coefold[2] + coefnew[1]
    coefnew <- c(mean(c(x$x, x$xcen)), sd(c(x$x, x$xcen)))
    i <-  i + 1L
  }
  if(i >= 100L)
    warning("interations exceeded")
  if(method == "log fill")
    retval <- list(meanlog=coefnew[1L], sdlog=coefnew[2],
                   fitted=exp(c(x$xcen, x$x)), censorlevels=cenlev)
  else
    retval <- list(mean=coefnew[1L], sd=coefnew[2],
                   fitted=c(x$xcen, x$x), censorlevels=cenlev)
  return(retval)
}
