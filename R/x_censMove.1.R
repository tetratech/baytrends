#' Censored Maintenance of Variance Extension, Type 1
#'
#' Calculates the Maintenance Of Variance Extension, Type 1 (MOVE.1) for record 
#'extension by fitting a Line of Organic Correlation (LOC) regression model to
#'left-censored data.
#'
#' The left- or right-hand side of the formula may be any numeric variable, just as with
#'\code{move.1} or a variable of class "lcens." The response variable must be the name of
#'a column and not converted using \code{as.lcens} in the call.
#'
#' If distribution is "normal," then the data in x and y are assumed to have a 
#'bivariate normal distribution. Otherwise, they are assumed to have a bivariate 
#'log-normal distribution and a logarithmic transform is applied to both x and y 
#'before analysis. The natural logarithm is used if distribution is "lognormal" and 
#'the commmon logarithm is used if distribution is "commonlog."
#'
#' @param formula a formula with a single response variable on the left and a single
#'explanatory variable on the right. See \bold{Details}.
#' @param data the data to search for the variables in \code{formula}.
#' @param subset an expression to select a subset of the data.
#' @param na.action what to do with missing values.
#' @param distribution the distribution of the data, either "normal," "lognormal,"
#'or "commonlog." See \bold{Details}.
#' @return An object of class "move.1" as decribed in the documentation for \code{move.1}.
#'Except, there are no \code{fitted.values} or \code{residuals} components, the values of
#'\code{x} and \code{y} include estimated values (See \bold{Note}), and components \code{cx}
#'and \code{cy}, which indicate left-censoring are included.
#' @note Because predictions are made using the \code{predict} method for class "move.1,"
#'and do not expect censored values, the explanatory variable in \code{formula} must
#'refer to a column in \code{data} and not converted using the \code{as.lcens} function;
#'the user must convert the explanatory variable column to class "lcens" before using 
#'\code{censMove.1}.
#' 
#' The components \code{x} and \code{y} are the actual values if the data are 
#'uncensored. If \code{y} is censored and \code{x} is not censored, then \code{y} is the
#'expected value of \code{y}, given its censored value and the value of \code{x} and likewise
#'for \code{x} censored and \code{y} not censored. If both are cesnored, then the values 
#'are computed from their expected values given the censored value without respect to
#'any condition of the other.
#' @seealso \code{\link{move.1}}
#' @references will need some.
#' @keywords regression censored
#' @examples
#'\dontrun{
#'library(smwrData)
#'data(IonBalance)
#'# Build model for non missing Alkalinity
#'move.1(Anion_sum ~ Cation_sum, data=IonBalance, 
#'  subset=abs(Pct_Diff) < 10) 
#'# Compare to censored
#'censMove.1(Anion_sum ~ Cation_sum, data=IonBalance, 
#'  subset=abs(Pct_Diff) < 10) 
#'# The difference in standard deviations is due to using sd/MLE 
#'}
#'
#'@export
censMove.1 <- function(formula, data, subset, na.action, distribution="normal") {
  ## Coding history:
  ##    2014Mar09 DLLorenz Original Coding
  ##
  call <- match.call()
  m <- match.call(expand.dots = FALSE)
  m$distribution <- NULL
  m$drop.unused.levels <- TRUE
  m[[1L]] <- as.name("model.frame")
  m <- eval(m, sys.parent())
  if(ncol(m) != 2L)
  	stop("Must specify a single response and a single explanatory variable in the formula")
  Y <- m[, 1L]
  yname <- names(m)[1L]
  X <- m[, 2L]
  xname <- names(m)[2L]
  fit <- list(coefficients=double(2L))
  ## Extract missing value info
  fit$na.action <- attr(m, "na.action")
  ## Adjust for transforms
  distribution <- match.arg(distribution, c("normal", "lognormal", "commonlog"))
  if(distribution == "lognormal") {
  	Y <- log(Y)
  	X <- log(X)
  	xname <- paste("log(", xname, ")", sep='')
  	yname <- paste("log(", yname, ")", sep='')
  }
  else if(distribution == "commonlog") {
  	Y <- log10(Y)
  	X <- log10(X)
  	xname <- paste("log10(", xname, ")", sep='')
  	yname <- paste("log10(", yname, ")", sep='')
  }
  X <- as.lcens(X)
  Y <- as.lcens(Y)
  retcor <- censCor(X, Y)
  ybar <- retcor[3L]
  yvar <- retcor[5L]^2
  xbar <- retcor[2L]
  xvar <- retcor[4L]^2
  fit$R <- retcor[1L]
  fit$p.value <- 1. - pchisq(-2*(retcor[10L] - retcor[11L]), 1)
  fit$coefficients[2L] <- sqrt(yvar/xvar)
  if(fit$R < 0)
  	fit$coefficients[2L] <-  - fit$coefficients[2L]
  fit$coefficients[1L] <- ybar - fit$coefficients[2L] * xbar
  names(fit$coefficients) <- c("(Intercept)", xname)
  ## Complete steps for the model
  fit$call <- call
  ## Pack the object with other necessary information
  ## This is a fair way to estimate values for x and y, naive when both are censored
  x <- X@.Data[, 1L]
  y <- Y@.Data[, 1L]
  cx <- X@censor.codes
  cy <- Y@censor.codes
  # Compute std err of y and of x from eqns of SLR, accounting for MLE
  sey <- sqrt((1 - retcor[1L]^2)*retcor[5L]^2*retcor[9L]/(retcor[9L] - 2))
  sex <- sqrt((1 - retcor[1L]^2)*retcor[4L]^2*retcor[9L]/(retcor[9L] - 2))
  # These are the regression coefs, Bx is predict y from x
  Bx <- retcor[1L]*sqrt(yvar/xvar)
  By <- retcor[1L]*sqrt(xvar/yvar)
  # Step through each one computing estimated values
  for(i in seq(retcor[9L])) {
  	# Nothing to do if neither censored
  	if(cy[i] && !cx[i]) { # only cy is censored, use inverse Mill's ration to estimate
  		ey <- retcor[3L] + Bx*(x[i] - retcor[2L]) # Expected valus of y given x
  		y[i] <- ey - sey*dnorm(y[i], ey, sey)/pnorm(y[i], ey, sey)
  	} else if(!cy[i] && cx[i]) { # only cx is censored
  		ex <- retcor[2L] + By*(y[i] - retcor[3L]) # Expected valus of y given x
  		x[i] <- ex - sex*dnorm(x[i], ex, sex)/pnorm(x[i], ex, sex)
  	} else if(cy[i] && cx[i]) { # both censored, just estimate from the distribution
  		y[i] <- retcor[3L] - retcor[5L]*dnorm(y[i], retcor[3L], retcor[5L])/pnorm(y[i], retcor[3L], retcor[5L])
  		x[i] <- retcor[2L] - retcor[4L]*dnorm(x[i], retcor[2L], retcor[4L])/pnorm(x[i], retcor[2L], retcor[4L])
  	} 		
  }
  fit$x <- x
  fit$y <- y
  fit$cx <- cx
  fit$cy <- cy
  fit$xstats <- c(mean=xbar, sd=sqrt(xvar))
  fit$ystats <- c(mean=ybar, sd=sqrt(yvar))
  fit$var.names <- c(yname, xname)
  fit$model <- m
  class(fit) <- "move.1"
  return(fit)
}
