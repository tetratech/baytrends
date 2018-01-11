#' @title Censored Data Regression
#'
#' @description Builds a linear regression model for censored response data.
#'
#' @details The left-hand side of the formula may be any numeric variable, just as with
#'\code{lm} or a variable of class "lcens," "mcens," or "qw."\cr
#'For un- or left-censored data, AMLE is used unless weights are specified in
#'the model, then MLE is used, through a call to \code{survreg}. For any other 
#'censored data, MLE is used.\cr
#'If \code{dist} is "normal," then the regression analysis assumes that the
#'residuals are normally distributed. If \code{dist} is "lognormal," then
#'the regression analysis assumes that the residuals are lognormally
#'distributed. In this case, predicted values are back transformed and
#'optionally bias corrected to represent the expected mean. If \code{dist}
#'is "commonlog," then the response data are transformed using \code{log10}
#'and those residuals are assumed to be normally distributed. No back 
#'transformation is made for predicted values.
#'
#' @param formula a formula describing the regression model. See \bold{Details}.
#' @param data the data to search for the variables in \code{formula}.
#' @param subset an expression to select a subset of the data.
#' @param weights a variable to use for weights.
#' @param na.action what to do with missing values.
#' @param dist the distribution of the data, either "normal," "lognormal,"
#'or "commonlog." See \bold{Details}.
#' @return An object of class "censReg."
#' @note A special feature of \code{censReg} is the ability to expand
#'the formula passed as a variable rather than as an expressly entered
#'formula. This feature is intended to facilitate certain kinds of scripts that 
#'construct a formula from combinations of variables in a dataset.
#'
#' Objects of class "censReg" have these methods:
#'\describe{
#'\item{coef}{extract the regression coefficents}
#'\item{extractAIC}{compute another information criterion (AIC) value}
#'\item{fitted}{extract the fitted values}
#'\item{logLik}{compute the log-likelihood of the fitted model}
#'\item{nobs}{extract the number of observations used to fit the model}
#'\item{plot}{create some basic diagnostic plots}
#'\item{predict}{predict new values from the model}
#'\item{print}{print the model}
#'\item{residuals}{extract the residuals from the model}
#'\item{rmse}{compute the root mean squared error (residual standard error)}
#'\item{summary}{compute summary information about the model}
#'\item{vcov}{extract the variance-covariance matrix from the model}
#'\item{vif}{compute the variance inflation factors for the explanatory variables in the model}
#'}
#'
#' The AMLE method has arbitrary limitations on the size of the data: 24 explanatory variables
#'and 5000 observations. Exceeding these will generate a warning message with the error code in
#'the output object.
#'
#' @seealso \code{\link{lm}}, \code{\link{survreg}}
#' @references Lorenz, 2015, smwrQW.\cr
#'Breen, R., 1996, Regression models: censored, sample selected, or truncated data:
#'Sage University Paper series on Quantitative Applications in the Social Sciences, 
#'07-111, Thousand Oaks, CA, \cr
#'Cohn, T.A., 1988, Adjusted maximum likelihood estimation of moments
#'of lognormal populations from type I censored samples: 
#'U.S. Geological Survey Open-File Report 88-350, 34 p.\cr
#' @keywords internal regression censored
#' @examples
#'
#'set.seed(345)
#'X <- runif(24, 1, 5)
#'Y <- X/2 + rnorm(24)
#'lm(Y ~ X) # the uncensored regression
#'censReg(as.lcens(Y, 1) ~ X) # censored at 1
#'
#' @export
censReg <- function(formula, data, subset, weights, na.action, dist="normal") {
  ## Coding history:
  ##    2012Jul23 DLLorenz Original Coding
  ##    2012Jul31 DLLorenz Modifications for prediction and load estimation
  ##    2012Sep25 DLLorenz Begin mods for various options
  ##    2012Dec28 DLLorenz Roxygenized
  ##    2014Sep04 DLLorenz Formula fix by Parker Norton
  ##
  call <- match.call()
  m <- match.call(expand.dots = FALSE)
  UseWt <- !missing(weights)
  dist <- match.arg(dist, c("normal", "lognormal", "commonlog"))
  ## Remove components not needed for model.frame
  m$dist <- NULL
  m[[1]] <- as.name("model.frame")
  m <- eval(m, parent.frame())
  Terms <- attr(m, "terms")
  ## Make sure that the formula is a formula and not a symbol--this
  # improves output and subsequent formula references
  if (typeof(call$formula) == "symbol") {
  	call$formula <- formula(Terms)
  }
  xvars <- as.character(attr(Terms, "variables"))
  Y <- model.extract(m, "response")
  X <- model.matrix(Terms, m)
  if((yvar <- attr(Terms, "response")) > 0L)
    xvars <- xvars[ - yvar]
  if(length(xvars) > 0L) {
    xlevels <- lapply(m[xvars], levels)
    xlevels <- xlevels[!unlist(lapply(xlevels, is.null))]
    if(length(xlevels) == 0L)
      xlevels <- NULL
  }
  else xlevels <- NULL
  if(UseWt)
    Wt <- model.extract(m, "weights")
  else
    Wt <- rep(1, nrow(X))
  na.message <- attr(m, "na.message")
  saved.na.action <- attr(m, "na.action")
  if(class(Y) == "numeric")
    Y <- as.lcens(Y)
  else if(class(Y) == "qw") {
    ## Required to fix the inability of model extraction to select all required parts
    if(!is.null(saved.na.action)) {
      Y@reporting.level <- Y@reporting.level[-saved.na.action]
      Y@remark.codes <- Y@remark.codes[-saved.na.action]
    }
    if(censoring(Y) == "multiple")
      Y <- as.mcens(Y)
    else
      Y <- as.lcens(Y)
  }
  else if(class(Y) == "lcens" && !is.null(saved.na.action))
    Y@censor.codes <- Y@censor.codes[-saved.na.action]
  ## OK, do it
  if(class(Y) == "lcens" && !UseWt)
    fit <- censReg_AMLE.fit(Y, X, dist)
  else
    fit <- censReg_MLE.fit(Y, X, Wt, dist)
  fit$call <- call
  fit$terms <- Terms
  fit$na.action <- saved.na.action
  fit$na.message <- na.message
  fit$xlevels <- xlevels
  class(fit) <- "censReg"
  if(fit$IERR > 0) {
  	warning("Fatal error code: ", fit$IERR)
  } else if(fit$IERR < 0) {
  	warning("Nonfatal error code: ", fit$IERR)
  }
  return(fit)
}
