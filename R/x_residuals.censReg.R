#' @title Extract Model Residuals
#'
#' @description Extracts the residuals from a censored regression object: method 
#'for "censReg" object.
#'
#' @details The value for \code{type} can be any one of the following:
#'\tabular{ll}{ Value \tab Description\cr
#'"working" \tab Residuals with censored residuals replaced by their expected
#'values\cr
#'"response" \tab Residuals from the linear predictor\cr
#'"influence" \tab An estimate of Cook's D values based on "working"
#'residuals\cr
#'"leverage" \tab The hat diagonals\cr
#'"S-L" \tab The square-root of the absolute value of the residuals with
#'censored residuals replaced by their expected value\cr }
#'Also, any other value of \code{type} for \code{\link{residuals.survreg}}
#'can be used to obtain those residuals. Note that "working" and "response"
#'are defined in the table above, in keeping with older versions of
#'\code{censReg}.
#'
#' @importFrom survival survreg Surv
#' @param object an object of class "censReg"---output from \code{censReg}
#' @param type The type of residuals, see \bold{Details}.
#' @param suppress.na.action logical, suppress the effects of the
#'\code{na.action} in the call to \code{censReg} and return only the fitted
#'values corresponding to the fitted data.
#' @param \dots further arguments passed to or from other methods.
#' @return The residuals from the regression as specified by \code{type}.
#' @seealso \code{\link{censReg}}
#' @keywords regression
#' @export
#' @method residuals censReg
residuals.censReg <- function(object , type="working",
                              suppress.na.action=FALSE, ...) {
  ## Coding history:
  ##    2012Sep25 DLLorenz Initial Coding
  ##    2012Dec31 DLLorenz Roxygenized
  ##    2013Apr24 DLLorenz Completed the additional residuals
  ##
  ## The full suite of residuals that rely on likelihood can be extracted
  ##  by constructing the survreg model!
  if(object$IERR > 0)
    return(NA)
  ## match type
  types <- c("working", "response", "influence", "leverage", "S-L", "deviance")
  type.ndx <- pmatch(type, types)
  if(is.na(type.ndx)) {
    if(object$method == "AMLE")
      MT <- survreg(Surv(object$YLCAL, !object$CENSFLAG, type="left") ~
                    object$XLCAL[, -1L, drop=FALSE], dist="gaussian")
    else
      MT <- object$survreg
      MT$na.action <- NULL # Controlled below
    Residuals <- residuals(MT, type=type)
    if(is.null(object$na.action) || suppress.na.action)
      return(Residuals)
    else return(naresid(object$na.action, Residuals))
  }
  type <- types[type.ndx]
  if(type == "working")
    Residuals <- object$RESID
  else if(type == "response")
    Residuals <- object$YLCAL - fitted(object, suppress.na.action = TRUE)
  else if(type == "S-L") { # return sqrt(abs(Resids)) (SAR)
    Residuals <- object$YLCAL - fitted(object, suppress.na.action = TRUE)
    Cens <- object$CENSFLAG
    Rmse <- rmse(object)
    ## Modify censored residuals with the expected value of the SAR
    if(object$method == "AMLE") {
      for(i in which(Cens))
        Residuals[i] <- integrate(function(x) sqrt(abs(x))*dnorm(x,0,Rmse),
                                  -Inf, Residuals[i]/Rmse)$value
      Residuals[!Cens] <- sqrt(abs(Residuals[!Cens]))
    }
    else {
      for(i in which(Cens < 0L))
        Residuals[i] <- integrate(function(x) sqrt(abs(x))*dnorm(x,0,Rmse),
                                  -Inf, Residuals[i]/Rmse)$value
      for(i in which(Cens > 0L))
        Residuals[i] <- integrate(function(x) sqrt(abs(x))*dnorm(x,0,Rmse),
                                  Residuals[i]/Rmse, Inf)$value
      Residuals[Cens == 0L] <- sqrt(abs(Residuals[Cens == 0L]))
    }
  }
  else if(type == "deviance") {
    if(object$method == "AMLE")
      MT <- survreg(Surv(object$YLCAL, !object$CENSFLAG, type="left") ~
                    object$XLCAL[, -1L, drop=FALSE], dist="gaussian")
    else
      MT <- object$survreg
      MT$na.action <- NULL
      Residuals <- residuals(MT, type="deviance")
  }
  else {
    Hats <- hat(object$XLCAL, intercept = TRUE)
    if(type == "influence") # return something like Cook's D
      Residuals <- (object$RESID^2) * Hats / ((1 - Hats)^2) /
        object$PARAML[object$NPAR + 1] / object$NPAR
    else
      Residuals <- Hats
  }
  if(is.null(object$na.action) || suppress.na.action)
    return(Residuals)
  else return(naresid(object$na.action, Residuals))
}
